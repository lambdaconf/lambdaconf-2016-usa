(*
 A benchmark of shift/reset: Filinski's representing non-determinism monads

  The benchmark is taken from Sec 6.1 of
    Martin Gasbichler, Michael Sperber: Final Shift for Call/cc: Direct
    Implementation of Shift and Reset, ICFP'02, pp. 271-282. 
    http://www-pu.informatik.uni-tuebingen.de/users/sperber/papers/shift-reset-direct.pdf

 This is a micro-benchmark: it is very non-determinism-intensive. It doesn't
 seem to be very indicative: the List monad outperforms all other
 implementations. There are hardly any deterministic opartions, so
 the overhead of the list monad is negligible.
*)

(* Small language with non-determinism: just like the one in our DSL-WC paper *)

module type SampleND = sig
  type 'a repr
  val int : int -> int repr
  val add : int repr -> int repr -> int repr
  val lam : ('a repr -> 'b repr) -> ('a -> 'b repr) repr
  val app : ('a -> 'b repr) repr -> ('a repr -> 'b repr)
  val amb : int repr list -> int repr
  val run : (unit -> int repr) -> int list
end;;


(* Benchmark cases *)
module WW(S: SampleND) = struct
  open S
  let test () =
    let f = lam (fun x ->
      add (add x (amb [int 6; int 4; int 2; int 8])) 
	         (amb [int 2; int 4; int 5; int 4; int 1])) in
    app f (amb [int 0; int 2; int 3; int 4; int 5; int 32])
end;;

let ww_answer = 
List.sort compare [8; 10; 11; 10; 7; 6; 8; 9; 8; 5; 4; 6; 7; 6; 3; 10; 12; 13;
12; 9; 10; 12; 13; 12; 9; 8; 10; 11; 10; 7; 6; 8; 9; 8; 5; 12; 14; 15;
14; 11; 11; 13; 14; 13; 10; 9; 11; 12; 11; 8; 7; 9; 10; 9; 6; 13; 15;
16; 15; 12; 12; 14; 15; 14; 11; 10; 12; 13; 12; 9; 8; 10; 11; 10; 7;
14; 16; 17; 16; 13; 13; 15; 16; 15; 12; 11; 13; 14; 13; 10; 9; 11; 12;
11; 8; 15; 17; 18; 17; 14; 40; 42; 43; 42; 39; 38; 40; 41; 40; 37; 36;
38; 39; 38; 35; 42; 44; 45; 44; 41];;

(* Real benchmark cases *)
module WWW(S: SampleND) = struct
  open S
  let test () =
    let f = lam (fun x ->
      add (add x (amb [int 6; int 4; int 2; int 8])) 
	         (amb [int 2; int 4; int 5; int 4; int 1])) in
    app f (app f (amb [int 0; int 2; int 3; int 4; int 5; int 32]))
end;;

module WWWW(S: SampleND) = struct
  open S
  let test () =
    let f = lam (fun x ->
      add (add x (amb [int 6; int 4; int 2; int 8])) 
	         (amb [int 2; int 4; int 5; int 4; int 1])) in
    app f (app f (app f (amb [int 0; int 2; int 3; int 4; int 5; int 32])))
end;;

module W5(S: SampleND) = struct
  open S
  let test () =
    let f = lam (fun x ->
      add (add x (amb [int 6; int 4; int 2; int 8])) 
	         (amb [int 2; int 4; int 5; int 4; int 1])) in
    app f 
      (app f (app f (app f (amb [int 0; int 2; int 3; int 4; int 5; int 32]))))
end;;

(* Different implementations of SampleND *)

(* Non-determinism monad as a list of successes *)
module NDList = struct
  type 'a repr = 'a list
  let int x = [x]
  let add xs ys = 		(* time taken to optimize and deforest *)
    List.rev
      (List.fold_left (fun z y -> 
	List.fold_left (fun z x -> (x+y)::z) z xs) [] ys)
  let lam f = [fun x -> f [x]]
  let app fs xs = 
    List.rev
      (List.fold_left (fun z x -> 
	List.fold_left (fun z f -> (f x) @ z) z fs) [] xs)
  let amb = List.concat
  let run m = m ()
end;;

let [101; 102; 201; 202] =
 let module M = struct
  open NDList
  let t = add (amb [int 1; int 2]) (amb [int 100; int 200])
 end in M.t
;;

let module M = WW(NDList) in 
assert (List.sort compare (NDList.run M.test) = ww_answer);;


(* CPS-based non-determinism *)
module NDCPS = struct
  type w = int list			(* have to fix the answer-type for amb*)
  type 'a repr = ('a -> w) -> w
  let int x = fun k -> k x
  let add xs ys = fun k -> xs (fun x -> ys (fun y -> k (x+y)))
  let lam f = fun k -> k (fun x -> f (fun k -> k x))
  let app fs xs = fun k -> fs (fun f -> xs (fun x -> f x k))
  let amb xs = fun k -> 
    List.rev (List.fold_left (fun z x -> List.rev_append (x k) z) [] xs)
  let run m = m () (fun x -> [x])
end;;

let [101; 201; 102; 202] =
 let module M = struct
  open NDCPS
  let t () = add (amb [int 1; int 2]) (amb [int 100; int 200])
 end in NDCPS.run M.t
;;

let module M = WW(NDCPS) in 
assert (List.sort compare (NDCPS.run M.test) = ww_answer);;

(* Direct-style non-determinism *)
module NDDirect = struct
  open Delimcc
  let p = new_prompt ()
  type 'a repr = 'a
  let int x = x
  let add = (+)
  let lam f = f
  let app f x = f x
  let amb xs = shift0 p (fun k ->
    List.rev (List.fold_left (fun z x -> List.rev_append (k x) z) [] xs))
  let run m = push_prompt p (fun () -> [m ()])
end;;

let [101; 102; 201; 202] =
 let module M = struct
  open NDDirect
  let t () = add (amb [int 1; int 2]) (amb [int 100; int 200])
 end in NDDirect.run M.t
;;

let module M = WW(NDDirect) in 
assert (List.sort compare (NDDirect.run M.test) = ww_answer);;

(* Direct-style non-determinism with call/cc: *)
(* the straightforward re-writing NDCPS into direct style using call/cc *)
(* Needs Xavier Leroy's callcc library: download from his web page *)
(*
#directory "ocaml-callcc-1.0/";;
#load "callcc.cma";;
*)

(*
module NDCallcc = struct
  open Callcc
  type 'a repr = 'a
  let int x = x
  let add = (+)
  let lam f = f
  let app f x = f x
  let k_reset = ref (fun _ -> failwith "no reset")
  let amb (xs:int list) = 
    callcc (fun k -> 
      let make_choice v =
	let old_k_reset = !k_reset in
	let res =
	  callcc (fun kret -> (k_reset := (throw kret); throw k v))
	in k_reset := old_k_reset; res in
      let res = 
	List.rev (List.fold_left (fun z x -> 
	  List.rev_append (make_choice x) z) [] xs) in
      !k_reset res)
  let run m = callcc (fun k -> let () = k_reset := throw k in
                               let v = m () in 
			       !k_reset [v];
			       failwith "unreachable")
end;;
 
let [101; 102; 201; 202] =
 let module M = struct
  open NDCallcc
  let t () = add (amb [int 1; int 2]) (amb [int 100; int 200])
 end in NDCallcc.run M.t
;;

let module M = WW(NDCallcc) in 
assert (List.sort compare (NDCallcc.run M.test) = ww_answer);;
*)

(* Running the benchmark *)

(* Time the execution *)
let timeit thunk =
  let time_start = Sys.time () in
  let r = thunk () in
  Printf.printf "\nTime spent: %g sec\n" (Sys.time () -. time_start);
  r;;

module Bench(S:SampleND) = struct
  module M3 = WWW(S)
  module M4 = WWWW(S)
  module M5 = W5(S)
  let test1 () = 
    Printf.printf "The www benchmark\n";
    for i = 1 to 5 do
      timeit (fun () -> 
	Printf.printf "result: %d\n" (List.length (S.run M3.test)))
    done
  let test2 () = 
    Printf.printf "\nThe wwww benchmark\n";
    for i = 1 to 5 do
      timeit (fun () -> 
	Printf.printf "result: %d\n" (List.length (S.run M4.test)))
    done
  let test5 () = 
    Printf.printf "\nThe w5 benchmark\n";
    for i = 1 to 5 do
      timeit (fun () -> 
	Printf.printf "result: %d\n" (List.length (S.run M5.test)))
    done
end;;

(* Since NDDirect does a very shallow embedding, we can dispense
   with the overhead of module for all operations except the
   non-deterministic once
*)
module W5Direct = struct
  let amb = NDDirect.amb
  let run = NDDirect.run
  let test () =
    let f = fun x -> x + amb [6; 4; 2; 8] + amb [2; 4; 5; 4; 1] in
    f (f (f (f (amb [0; 2; 3; 4; 5; 32]))))
  let test5 () = 
    Printf.printf "\nThe w5 direct benchmark\n";
    for i = 1 to 5 do
      timeit (fun () -> 
	Printf.printf "result: %d\n" (List.length (run test)))
    done
end;;

(* Shown are the medians of 5 consecutive runs.
OCaml 3.12, bytecode
*)


Printf.printf "\nNDList\n";;
let module M = Bench(NDList) in (M.test1 (); M.test2 (); M.test5 ());;
(*
The www benchmark
result: 2400
Time spent: 0.0016 sec

The wwww benchmark
result: 48000
Time spent: 0.042 sec

The w5 benchmark
result: 960000
Time spent: 0.91 sec
*)

Printf.printf "\nNDCPS\n";;
let module M = Bench(NDCPS) in (M.test1 (); M.test2 (); M.test5 ());;
(*
The www benchmark
result: 2400
Time spent: 0.003 sec

The wwww benchmark
result: 48000
Time spent: 0.095 sec

The w5 benchmark
result: 960000
Time spent: 2.61 sec
*)

Printf.printf "\nNDDirect\n";;
let module M = Bench(NDDirect) in (M.test1 (); M.test2 (); M.test5 ());;
(*
The www benchmark
result: 2400
Time spent: 0.009 sec

The wwww benchmark
result: 48000
Time spent: 0.21 sec

The w5 benchmark
result: 960000
Time spent: 4.94 sec
*)


Printf.printf "\nNDDirect standalone\n";;
W5Direct.test5 ();;
(*
Bytecode:
The w5 direct benchmark
result: 960000
Time spent: 4.8 sec
*)

(*
let module M = Bench(NDCallcc) in (M.test1 (); M.test2 (); M.test5 ());;
The results are similar to those of NDDirect: the stack depth is
very low, it does not matter that we copy the whole stack or the part of it.

*)

(* Analysis
Rates of growth: test2/test1 and test5/test2:
NDList:   29.7 and 18.3
NDCPS:    30.2 and 26.8
NDDirect: 24.2 and 23.0
*)
