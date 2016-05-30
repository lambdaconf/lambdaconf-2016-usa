(* The library to support a probabilistic embedded domain specific language *)
(*            Probabilistic inference procedures                            *)
(* These procedures implement different exploration strategies over a
   lazy search tree, transforming a search tree into a flatter form         *)

open Ptypes
open Printf
(* We also use polymorphic maps, pMap.* in the current directory *)
;;

(* ------------------------------------------------------------------------ *)
(*    Exact inference strategies: deterministic search procedures           *)

(* Explore and flatten the tree: perform exact inference to the given depth *)
(* If maxdepth is None, explore as far as possible   *)
let explore (maxdepth : int option) (choices : 'a pV) : 'a pV =
  let rec loop p depth down choices ((ans,susp) as answers) =
    match choices with
    | [] -> answers
    | (pt,V v)::rest -> loop p depth down rest 
	                   (PMap.insert_with (+.) v (pt *. p) ans,susp)
    | (pt,C t)::rest when down ->
	let down' = match maxdepth with Some x -> depth < x | None -> true in
	loop p depth down rest
	  (loop (pt *. p) (succ depth) down' (t ()) answers) 
    | (pt, c)::rest -> loop p depth down rest (ans,(pt *. p,c)::susp) in
  let (ans,susp) = loop 1.0 0 true choices (PMap.empty,[])
  in PMap.foldi (fun v p a -> (p,V v)::a) ans susp;;

let nearly_one = 1.0 -. 1e-7;;		(* For robust comparison with 1.0 *)

(* Explore but do not flatten the tree: 
   perform exact inference to the given depth
   We still pick out all the produced answers and note the failures. *)
let shallow_explore maxdepth (choices : 'a pV) : 'a pV =
  let add_answer pcontrib v mp = PMap.insert_with (+.) v pcontrib mp in
  let rec loop pc depth ans acc = function
    | [] -> (ans,acc)
    | (p,V v)::rest -> loop pc depth (add_answer (p *. pc) v ans) acc rest
    | c::rest when depth >= maxdepth -> loop pc depth ans (c::acc) rest
    | (p,C t)::rest -> 
	  let (ans,ch) = loop (pc *. p) (succ depth) ans [] (t ()) in
	  let ptotal = List.fold_left (fun pa (p,_) -> pa +. p) 0.0 ch in
	  let acc =
	    if ptotal = 0.0 then acc
	    else if ptotal < nearly_one then
	     (p *. ptotal, let ch = List.map (fun (p,x) -> (p /. ptotal,x)) ch
	                   in C (fun () -> ch))::acc
	    else (p, C (fun () -> ch))::acc in
	  loop pc depth ans acc rest
  in
  let (ans,susp) = loop 1.0 0 PMap.empty [] choices
  in PMap.foldi (fun v p a -> (p,V v)::a) ans susp;;

(* Explore the tree till we find the first success -- the first leaf
   (V v) -- and return the resulting tree. If the tree turns out to
   have no leaves, return the empty tree.
*)

let rec first_success: 'a pV -> 'a pV = function
  | [] -> []
  | ((_,Ptypes.V _) :: _) as l  -> l
  | (pt,Ptypes.C t) :: rest ->	(* Unclear: expand and do BFS *)
      first_success (rest @ List.map (fun (p,v) -> (pt *. p,v)) (t ()))


(* ------------------------------------------------------------------------ *)
(*    Semi-Exact inference strategies: deterministic search procedures      *)
(*			  over a subtree				    *)

(* A bounds estimator: obtain the bounds on the probabilty
   of evidence.
   The object probabilistic program must return (), or fail.
   Currently I don't know how to assign bounds when several values
   may be returned.
   This restriction seems consistent with Problog, which too determines
   bounds on the probability of a query.

   We traverse the tree breadth-first. If the number of unexplored branches
   raises above the threshold, we discard the branch with the lowest
   probability mass. A discarded branch with the probability mass p contributes
   0 to the current lower bound and p to the current upper bound.
   A successful branch with mass p contributes p to both bounds.
   A failed branch contributes 0 to both bounds.
*)
let bounded_explore maxsize (choices : unit pV) : prob * prob =
  let rec loop explore pc low high jqueue jqsize = function
    | [] -> next low high jqueue jqsize
    | (p,V _)::rest -> 
	let pe = pc *. p in
	loop explore pc (low +. pe) (high +. pe) jqueue jqsize rest
    | (p,C t)::rest -> 
	if explore then
	    loop explore pc low high 
	           (PMap.insert_with (@) (pc *. p) [t] jqueue) (jqsize + 1)
	         rest
	else loop explore pc low (high +. pc *. p) jqueue jqsize rest
  and next low high jqueue = function
    | 0 -> (low,high)
    | jqsize when jqsize < maxsize ->
	let ((p,t::ts),jqueue) = PMap.delete_find_max jqueue in
	let jqueue = if ts = [] then jqueue else PMap.add p ts jqueue in
	loop true p low high jqueue (jqsize - 1) (t ())
    | jqsize ->
	let ((p,t::ts),jqueue) = PMap.delete_find_min jqueue in
	let jqueue = if ts = [] then jqueue else PMap.add p ts jqueue in
	loop false p low high jqueue (jqsize - 1) (t ())
  in loop true 1.0 0. 0. PMap.empty 0 choices
;;
(* The convergence, however, is not as good as sampling on sorted.ml *)


(* ------------------------------------------------------------------------ *)
(*	Approximate inference strategies:				    *)
(*  Trace a few paths from the root to a leaf of the search tree            *)
(* The following procedures are non-deterministic; they use a given selector*)
(* procedure, of the type 'selector', to chose among the alternatives.      *)
(* For top-level inference, the selector uses system random generator.      *)

(* Naive, rejection sampling: the baseline *)
let rejection_sample_dist (selector: 'a vc selector) nsamples ch : 'a pV =
  let rec loop pcontrib ans = function
    | [(p,V v)]  -> PMap.insert_with (+.) v (p *. pcontrib) ans
    | []         -> ans
    | [(p,C th)] -> loop (p *. pcontrib) ans (th ())
    | ch -> 			(* choosing one thread randomly *)
	let (ptotal,th) = selector ch in
	loop (pcontrib *. ptotal) ans [(1.0,th)] in
  let rec driver ch ans = function
    | 0 -> let ns = float_of_int nsamples in
           printf "rejection_sample: done %d worlds\n" nsamples;
           PMap.foldi (fun v p a -> (p /. ns,V v)::a) ans []
    | n -> driver ch (loop 1.0 ans ch) (pred n) in
  driver ch PMap.empty nsamples
;;

(* Sample a distribution with a look-ahead exploration *)
(* A single sample can give us more than one data point: if one of
   the choices is a definite value, we note it right away, with
   its weight. The rest of the choices will be re-scaled automatically.
*)
(* Given a sampler, a function 'seed->'seed, run it a certain number
   of times and return the resulting seed and the number of runs
*)
type sample_runner = 
        {sample_runner : 'seed. 'seed -> ('seed -> 'seed) -> 'seed * int};;

let sample_dist (selector : 'a pV selector) (sample_runner : sample_runner)
    ch : 'a pV =
  let look_ahead pcontrib (ans,acc) = function (* explore the branch a bit *)
    | (p,V v) -> (PMap.insert_with (+.) v (p *. pcontrib) ans, acc)
    | (p,C t) -> begin
	match t () with
	| [] -> (ans,acc)
	| [(p1,V v)] -> 
	    (PMap.insert_with (+.) v (p *. p1 *. pcontrib) ans, acc)
	| ch ->
	    let ptotal = List.fold_left (fun pa (p,_) -> pa +. p) 0.0 ch in
	    (ans,
 	      if ptotal < nearly_one then
	        (p *. ptotal, List.map (fun (p,x) -> (p /. ptotal,x)) ch)::acc 
              else (p, ch)::acc)
    end in
  let rec loop pcontrib ans = function
    | [(p,V v)]  -> PMap.insert_with (+.) v (p *. pcontrib) ans
    | []         -> ans
    | [(p,C th)] -> loop (p *. pcontrib) ans (th ())
    | ch -> 			(* choosing one thread randomly *)
	begin
	match List.fold_left (look_ahead pcontrib) (ans,[]) ch with
	| (ans,[]) -> ans
	| (ans,cch) ->
	let (ptotal,th) = selector cch in
	loop (pcontrib *. ptotal) ans th end in
  let toploop pcontrib ans cch =	(* cch are already pre-explored *)
    let (ptotal,th) = selector cch in
    loop (pcontrib *. ptotal) ans th in
  let driver pcontrib vals cch =
    let (ans,nsamples) = 
      sample_runner.sample_runner PMap.empty 
	(fun ans -> toploop pcontrib ans cch) in
    let ns = float_of_int nsamples in
    let ans = PMap.foldi
	         (fun v p ans -> 
                   PMap.insert_with (+.) v (ns *. p) ans) vals ans in
    printf "sample_importance: done %d worlds\n" nsamples;
    PMap.foldi (fun v p a -> (p /. ns,V v)::a) ans [] in
  let rec make_threads pcontrib ans ch =  (* pre-explore initial threads *)
    match List.fold_left (look_ahead pcontrib) (ans,[]) ch with
    | (ans,[]) -> (* pre-exploration solved the problem *)
	PMap.foldi (fun v p a -> (p,V v)::a) ans []
    | (ans,[(p,ch)]) -> (* only one choice, make more *)
	make_threads (pcontrib *. p) ans ch
	  (* List.rev is for literal compatibility with an earlier version *)
    | (ans,cch) -> driver pcontrib ans (List.rev cch)
  in
  make_threads 1.0 PMap.empty ch
;;

(* Another idea for a better sampler: given the list of threads, the pV
structure, split in in two and sample the two parts separately; then combine the
results. Preferably sample all high-weight threads separately from low-weight
threads, so that low-probability threads are not starved by high-probability
ones.
*)

(*
The sample_dist above works great; yet there are cases where one-step
look-ahead is insufficient. Consider the following program
  (* select a random point with 0..9 square *)
  let random_pos () = (uniform 10, uniform 10)
  let model () =
    let _ = geometric_bounded 3 0.98 in
    let np = 0 in
    let np =  if not (flip 0.98) then np else
       if random_pos () <> (3,5) then fail () else succ np in
    let np =  if not (flip 0.98) then np else
       if random_pos () <> (3,5) then fail () else succ np in
    let np =  if not (flip 0.98) then np else
       if random_pos () <> (3,5) then fail () else succ np in
    if np <> 3 then fail ()
;;

let [(9.41192000000000293e-07, V ())] =
exact_reify model;;

Alas, doing importance sampling on the above model fails.
   sample_importance (random_selector 17) 1500 model;;
gives no samples. The reason of course is the final test: we have
to go through a long series of choices before we do the test, at which
point it fails. We should bring the evidence checking closer to the
point of choice... Perhaps lightweiht constraint solving may help?

Here is a different idea for an improved importance sampling:
when the sampling process encounters a failure, as in these lines
    let rec loop pcontrib ans = function
    | [(p,V v)]  -> PMap.insert_with (+.) v (p *. pcontrib) ans
    | []         -> ans (* failure detected *)
accumulate pcontrib (probably weighed by 1/nsamples) as the failure
probability of the main thread, selected by the driver.
Use the failure probability to scale the probabilities of the main
thread to affect the selection by the main driver (we should
keep the scaling factor around to correct for the probabilities of the
found answers). So that the more
failures are reported by a thread, the less likely the driver will
select it.
*)

(* The following reification procedures didn't show any compelling advantage *)

(*
let observe_la test maxdepth th = 
  reflect (reify (Some maxdepth) (fun () -> (observe test th)));;

(* Now we not only force the delayed branch, but also flatten it,
   up to the given depth. The simple sample_reify is the case of
   maxdepth = 0
*)
let sample_explore_reify 
    randomseed nsamples maxdepth (thunk : unit -> 'a) : 'a pV =
  sample_dist randomseed nsamples (explore (Some maxdepth)) 
  (reify (Some maxdepth) thunk);;

(* For large models, the memoizer may run out of memory... *)
let sample_explore_reify_memoize
    randomseed nsamples maxdepth (thunk : unit -> 'a) : 'a pV =
  let mem_explore ch =
    List.map (function (p,C th) -> (p, let v = lazy (explore (Some maxdepth) 
						       (th ()))
	                               in C (fun () -> Lazy.force v))
	             | (p,V v)  -> (p, V v)) ch in
  sample_dist randomseed nsamples mem_explore
  (reify (Some maxdepth) thunk);;
*)

(* ------------------------------------------------------------------------ *)
(*			Utilities					    *)

    (* Estimate the approximation error by computing the mean and the
     * standard deviation over multiple runs of the sampler *)
let statistics (randomseed1, randomseed2) (sampler : (int -> 'a pV)) =
  let answers = Hashtbl.create 17 in
  for randomseed = randomseed1 to randomseed2 do
    List.iter
      (fun (p, V v) ->
	try let (pold, p2old) = Hashtbl.find answers v in
	Hashtbl.replace answers v (pold +. p, p2old +. p *. p)
	with Not_found -> Hashtbl.add answers v (p, p *. p))
      (sampler randomseed)
  done;
  let n = float_of_int (randomseed2 - randomseed1 + 1) in
  Hashtbl.fold
    (fun v (p,p2) a -> (v, p /. n, sqrt ((p2 -. p *. p /. n) /. n)) :: a)
    answers [];;

(* Normalize the distribution. We also return the total probability mass,
   the normalization constant.
*)
let normalize l = 
  let total = List.fold_left (fun acc (p,_) -> p +. acc) 0.0 l in
  (total, List.map (fun (p,v) -> (p /. total,v)) l);;

(* Time the execution *)
let timeit thunk =
  let time_start = Sys.time () in
  let r = thunk () in
  Printf.printf "\nTime spent: %g sec\n" (Sys.time () -. time_start);
  r;;

