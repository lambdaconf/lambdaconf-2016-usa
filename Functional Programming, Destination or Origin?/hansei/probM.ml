(* The library to support a probabilistic embedded domain specific language *)
(* 
  The library lets the user define first-order probabilistic computations
  and perform exact and approximate inference (rejection and importance
  sampling).
  The library expresses probabilistic computations in direct style 
  (so that deterministic computations `run at full speed') 
  lets the programmer reify and reflect probabilistic `effects'.
*)

(* Open the DelimCC library
   http://okmij.org/ftp/continuations/implementations.html#caml-shift
*)
open Delimcc
open Printf
open Ptypes
open Inference
(* We also use polymorphic maps, pMap.* in the current directory *)



(* The interface to the reifier, which `runs' the probabilistic computation *)

type pReq = Done |  Choice of (unit -> pReq) cdist
 
let pp = new_prompt ();;

(* We often use mutable variables as `communication channel', to appease
   the type-checker. The variable stores the `option' value --
   most of the time, None. One function writes a Some x value,
   and another function almost immediately reads the value -- exactly
   once. The protocol of using such a variable is a sequence of
   one write almost immediately followed by one read.
   We use the following helpers to access our `communication channel'.
*)
let from_option = function Some x -> x | None -> failwith "fromoption";;

let read_answer r = let v = from_option !r in r := None; v (* for safety *)

(* First-class storage: for the implementation of `thread-local' memory *)
module Memory = struct
  type 'a loc = int * 'a option ref 
  type cell   = unit -> unit
  module M = Map.Make(struct type t = int let compare x y = x - y end)
  type mem = cell M.t
  let newm = M.empty
  let genid : unit -> int =	(* generating fresh locations *)
    let counter = ref 0 in
    fun () -> let v = !counter in counter := succ v; v
  let new_loc () = (genid (), ref None)
  let new_cell (_,chan) x = fun () -> chan := Some x
  let mref (id,chan) m =
    let cell = M.find id m in
    cell (); read_answer chan
  let mset ((id,chan) as loc) x m =
    M.add id (new_cell loc x) m
end;;

(* The first-class storage is immutable and pure functional. Only the pointer
   to it is mutable. This is critical as different threads may have different
   views of the same `memory' and one thread's updating its memory
   should not affect other threads that share it.
*)
let thread_local = ref Memory.newm;;

(* We try to make it appear as if thread_local is a part of the continuation.
   We should think that the control prompt delimits not only the continuation
   but also thread_local. Thus we should think push_prompt to be a composition
   of Delimcc.push_prompt and the mem_prompt defined below.
*)

let mem_prompt th = 
   let mem = !thread_local in
   let v = th () in
   thread_local := mem; v;;

(* The basic reifier itself: reify a probabilistic computation
   into a weighted search tree pV
*)

(* The simple-minded reifier is as follows:
let reify0 (thunk : unit -> 'a) : 'a pV =
  let () = thread_local := Memory.newm in
  let answer = ref None in
  let rec interp = function 
    | Done -> [(1.0, V (read_answer answer))] (* deterministic value *)
    | Choice ch -> List.map (fun (p,th) -> (p, C (fun () -> interp (th ()))))
	          ch
  in interp (push_prompt pp (fun () -> answer := Some (thunk ()); Done))
;;

  To allow for nesting, push_prompt should delimit not only control
  but also `memory', to properly implement `thread-local' memory.
  In the following code, we effectively `compose' push_prompt with mem_prompt.
*)

let reify0 (thunk : unit -> 'a) : 'a pV =
  let answer = ref None in
  let rec interp = function 
    | Done -> [(1.0, V (read_answer answer))] (* deterministic value *)
    | Choice ch -> List.map (fun (p,th) -> (p, C (fun () -> interp (th ()))))
	          ch
  in
  let mem = !thread_local in
  let v = push_prompt pp (fun () -> 
	 thread_local := Memory.newm; answer := Some (thunk ()); Done) in
  thread_local := mem; 
  interp v;;
 

(* Two basic library functions for probabilistic programming *)

(* We make it appear as if capturing the continuation also captures
   the dynamic environment, thread_local. Shift implicitly
   wraps the captured continuation into a prompt; and so we
   add mem_prompt: whenever control is delimited, so should be `memory'.
*)
let dist (choices : 'a dist) : 'a  =
  let curr_mem = !thread_local in
  shift0 pp (fun k -> 
    Choice 
       (List.map (fun (p,v) -> 
	 (p, (fun () -> 
	          let mem = !thread_local in
		  let () = thread_local := curr_mem in
		  let v = k v in
		  thread_local := mem; v))) choices))
;;

let fail () = abort pp (Choice []);;

(* The `inverse' of reify: reflect a search tree into a program
   denoting the same distribution. 
*)

let reflect (tree : 'a pV) : 'a  =
  let curr_mem = !thread_local in
  let rec make_choices k pv =
    Choice (List.map (
	    function (p,V x) -> (p, fun () -> k x)
                  |  (p,C x) -> (p, fun () -> make_choices k (x ()))) pv) in
  shift0 pp (fun k -> 
    make_choices (fun x -> 
      let mem = !thread_local in
      let () = thread_local := curr_mem in
      let v = k x in
      thread_local := mem; v)
     tree);;

(* ---------------  A few general purpose distributions *)

(* Binary boolean choice with probability p *)
let flip p = dist [ (p,true); (1. -. p, false)];;

(* Uniform choice from [0..(n-1)] *)
let uniform = function
 | 1 -> 0
 | n when n > 1 -> 
     let p = 1. /. float_of_int n in
     let rec loop pacc acc i = 
       if i = 0 then dist ((1. -. pacc, i)::acc)
       else loop (pacc +. p) ((p,i)::acc) (pred i)
     in loop 0.0 [] (pred n)
 | _ -> failwith "uniform: non-positive count n"
;;

(* Uniform choice from an array *)
let uniformly arr = 
  let n = uniform (Array.length arr) in
  arr.(n);;

(* Uniform choice of an integer from [low..high] *)
let uniform_range low high =
  low + uniform (high - low + 1);;

(* Geometric distribution with the prob p:
   gives 0 with the prob p,
   gives 1 with the prob (1-p)*p, etc.
*)

let geometric p = 
  let rec loop n = [(p,        C (fun () -> [(1.0, V n)])); 
		    (1.0 -. p, C (fun () -> loop (succ n)))]
  in reflect (loop 0)
;;


(* Bounded geometric distribution, with the largest number n and 
   with the prob p:
     gives 0 with the prob p,
     gives 1 with the prob (1-p)*p, etc.
   The normalization constant is the inverse of:
    p + (1-p)*p + (1-p)^2*p + ... (1-p)^n * p =
    1 - (1-p)^(n+1)
*)

let geometric_bounded n p = 
  let rec expi x = function 0 -> 1.0
    | 1 -> x 
    | n -> x *. (expi x (pred n)) in
  let norm = 1. -. expi (1. -. p) (n+1) in
  let rec loop pp i = 
    if i > n then [] else (pp, i) :: (loop ((1. -. p) *. pp) (succ i))
  in
  dist (loop (p /. norm) 0)
;;



(* ---------------  Other convenient derived functions *)

(* Assert the present evidence *)
let observe (test : ('a -> bool)) (thunk : unit -> 'a) : 'a =
  let v = thunk () in if test v then v else fail ();;



(* Selectors, used by approximate inference procedures *)

(* Random selection from a list of choices, using system randomness *)
let random_selector randomseed : 'a selector =
  let () = Random.init randomseed in
  let rec selection r ptotal pcum = function
      | [] -> failwith "Choice selection: can't happen"
      | ((p,th)::rest) -> 
	  let pcum = pcum +. p in
	  if r < pcum then (ptotal,th)
	  else selection r ptotal pcum rest
  in fun choices ->
    let ptotal = List.fold_left (fun pa (p,_) -> pa +. p) 0.0 choices in
    let r = Random.float ptotal in     (* 0<=r<ptotal *)
    selection r ptotal 0.0 choices
;;

(* A selector from a list of choices relying on the non-determinism
   supported by the parent reifier.
*)
let dist_selector ch =
  let ptotal = List.fold_left (fun pa (p,_) -> pa +. p) 0.0 ch in
  (ptotal, dist (List.map (fun (p,v) -> (p /. ptotal, v)) ch))
;;


(* Composition of the reifier and the explorer: defined for convenience *)

(* Reify a computation and do the exact inference to the given depth *)
let exact_reify (thunk : unit -> 'a) : 'a pV =
  explore None (reify0 thunk);;

(* This reifier does sampling. A delayed branch is forced but not
   expanded further 
*)
let sample_rejection selector nsamples (thunk : unit -> 'a) : 'a pV =
  rejection_sample_dist selector nsamples (reify0 thunk);;

let sample_importance selector nsamples (thunk : unit -> 'a) : 'a pV =
  sample_dist selector 
    {sample_runner =
     fun z th -> 
      let rec loop z = function 0 -> (z,nsamples) | n -> loop (th z) (pred n)
      in loop z nsamples}
    (shallow_explore 3 (reify0 thunk));;

(* Variable elimination optimization: transform a stochastic function
   a -> b to a generally faster function
*)
let variable_elim f arg =
  reflect (exact_reify (fun () -> f arg))

(* ------------------------------------------------------------------------
  Call-by-need and non-determinism

  If the user wrote
	let x = e in body

  he could add laziness by re-writing the above expression as

        letlazy (fun () -> e) (fun y -> body[x: = y ()])
  where body[x: = y ()] means that all free occurrences of 'x' in the body
  are replaced with y ().

  Now, 'e' is not evaluated until y () is executed. However, when 'e' has been
  evaluated, 
        letlazy (fun () -> e) (fun y -> body[x: = y ()])
  is equivalent to the original
	let x = e in body
  That is, all occurrences of y () in body evaluate to the same value.
  That value may be different in different worlds though.


  However, the `lazy variable' may escape from the body:
      letlazy (fun () -> e) (fun y -> y)
  This is especially likely if y is incorporated into lazy data structures
  like lists with the lazy spine. Such data structures are common, see
  probabilistic CFG generating sequences of arbitrary length. The solution is
  to do `lazy let insertion' at the point of reification. So, lazy
  variables get `the global scope'. The code below does exactly that (and
  then a few optimizations, so that the `lazy let insertion' might not
  be easy to notice). The solution is identical to that
  of emulating _global_ reference cells with delimited continuations.

  Because now lazy variables are in the global scope, we simplify the syntax
  of letlazy: we consider `body' to be always the identity.

  Lazy vs delayed evaluation: to quote, ``the difference is that delayed
  evaluation always eventually evaluates the thunk (even if the body does
  not need it), just in case the thunk could fail.  For efficiency, I
  think it makes sense to have both delayed and lazy evaluation.''
  In implementing lazy evaluation, the major concern is not to evaluate
  the computation unless it really needed. In the delayed evaluation, we
  no longer have this concern. Thus we may evaluate the binding expression
  right upfront and reify the result. If the exploration of the expression
  to bind to a lazy variable yielded a value as one of the choices,
  we are all set. Otherwise, we should set up an exploration, along
  the lines of non-probabilistic non-deterministic programming. We are
  no longer interested in probabilities; we just want to find out if there
  is at least one way an expression may yield a value.

*)

let letlazy e =
  let loc = Memory.new_loc () in
  let deref () = 
    try Memory.mref loc !thread_local 
    with Not_found ->
	let v = e () in
	let m' = Memory.mset loc v !thread_local in
	let () = thread_local := m' in
	v
  in deref;;

(* We should use more general primitives, which are nesting-safe:
   letlazy_nesting.
   These primitives can be used without nesting, only letlazy_nesting
   is a little-bit slower. 
   The idea: if a lazy variable cannot be found in the local memory
   of the current thread, ask the parent. That is, re-execute
   the dereference in the local memory of the parent thread.
   (Dereferencing can affect the local memory!)
*)

type 'a memo_val = MV of 'a | MT of (unit -> 'a);;
let letlazy_nesting e =
   let loc = Memory.new_loc () in
   let m' = Memory.mset loc (MT e) !thread_local in
   let () = thread_local := m' in
   let () = printf "New loc %d\n" (fst loc) in
   let rec deref () = 
    let () = printf "Deref loc %d\n" (fst loc) in
    match (try Some (Memory.mref loc !thread_local) with Not_found -> None) with
      | Some (MV v) -> printf "found\n"; v	(* Already evaluated *)
      | Some (MT e) ->				(* Not yet evaluated *)
            let () = printf "found unevaluated\n" in
            let v = e () in		        (* Evaluate and memoize *)
	    let m' = Memory.mset loc (MV v) !thread_local in
	    let () = thread_local := m' in
	    v
      | None -> (* didn't find ourselves in the local scope. Ask the parent *)
         let () = printf "not found: asking parent\n" in
         let mem = !thread_local in
         shift0 pp 
          (fun k ->  Choice [(1.0, fun () -> 
                                  let () = printf "... asking parent\n" in
                                  let v = deref () in
                                  mem_prompt (fun () ->
                                  (* Note in our memory too *)
	                          let m' = Memory.mset loc (MV v) mem in
	                          let () = thread_local := m' in
	                          k v))])
  in deref;;


(* The original, obsolete version ... 

(* 
   The following may look quite convoluted, and the use of assignment
   gratuitous. The sole advantage is avoiding multiple prompts while
   appeasing the typechecker. The reason to stick to only one prompt
   is to be able to integrate call-by-need with user-reification:
   when the user wishes to reify the computation that needs an
   `external' CBN variable.
   We must note an optimization: rather than filling in the needed_answer
   all the time, we can just fill it in once. We no longer need
   to send the Needed request just to access the variable. We still
   need to capture the rest of the computation, so that we can capture
   the Choice events and make sure we set needed_answer appropriately
   every time the main scheduler chooses our thread for execution.
   Another approach is to introduce a new kind of request to let the scheduler
   now about the new lazy variable. The scheduler will maintain a
   `lazy var env' per thread. That approach now looks very close to
   what I had in FLP.
   But we leave these optimizations for the latter. The big
   advantage of the following code is that read_answer enforces certain
   invariants how the communication variable is used. Whenever something
   goes wrong, we get a run-time errors. The optimizations mask the
   implementation errors.
*)


let genvar : unit -> needed_var =	(* generating fresh needed var names *)
  let counter = ref 0 in
  fun () -> let v = !counter in counter := succ v; v
;;


let letlazy_old e =
  let myvar = genvar () in
  let needed_answer = ref None in	(* communication variable *)
  let reified_e     = ref None in
  let reify_e () = match !reified_e with (* The depth of reify doesn't matter *)
                    | None -> let re = reify (Some 2) e in
		              reified_e := Some re; re
		    | Some re -> re in
  (* Reply with the memoized value *)
  let rec memoized_req v = function
    | Done -> Done
    | Choice ch -> 
	  Choice (List.map (fun (p,t) -> (p,fun () -> 
	    push_prompt pp (fun () -> memoized_req v (t ())))) ch)
    | Needed (n,k) when n = myvar -> 
	(* printf "letlazy: memoized_req for our var %d\n" myvar; *)
	reply_memo v k
    | Needed (n,k) ->
	(* printf "letlazy: memoized_req for other var %d\n" n; *)
	Needed(n,fun () -> memoized_req v (push_prompt pp k))
  and
  reply_memo v k = needed_answer := Some v; memoized_req v (push_prompt pp k) in
  (* Handle the initial request for the value of needed variable *)
  let rec initial_req = function
    | Done -> Done
    | Choice ch -> 
	  Choice (List.map (fun (p,t) -> (p,fun () -> 
	    push_prompt pp (fun () -> initial_req (t ())))) ch)
    | Needed (n,k) when n = myvar ->
	(* printf "letlazy: init_req for our var %d\n" myvar; *)
	(* reply_memo (reflect (reify_e ())) k *)
	reply_memo (e ()) k
    | Needed (n,k) ->
	(* printf "letlazy: init_req for other var %d\n" n; *)
	Needed(n,fun () -> initial_req (push_prompt pp k))
  in
  let make_req () = shift0 pp (fun k -> Needed (myvar,k));
                    read_answer needed_answer
  in shift pp (fun k -> initial_req (k make_req))
;;
*)


(* Stochastic (memoized) functions.
   Let f :: a -> Prob b
 be a stochastic `function': when applied to an argument, it gives a
 a sample from the distribution of its result. Applying that `function'
 again to the same argument will generally give a different sample,
 although from the same distribution.

 The function (memo f) :: a -> Prob b
 is then a proper function: applied to an argument, it gives a sample from
 its output distribution. When applied to the argument again, it gives the
 same sample.

 The memo facility is the generalization of letlazy: indeed, letlazy
 is the particular case of memo when the function to memoize has the
 argument unit.
*)

let memo f =
  let loc = Memory.new_loc () in
  let m' = Memory.mset loc PMap.empty !thread_local in
  let () = thread_local := m' in
  let deref x = 
    let table = Memory.mref loc !thread_local in
    try PMap.find x table
    with Not_found ->
	let v = f x in			(* may change memory! *)
	let table = Memory.mref loc !thread_local in
	let m' = Memory.mset loc (PMap.add x v table) !thread_local in
	let () = thread_local := m' in
	v
  in deref;;

