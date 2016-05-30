(*
 A benchmark of delimcc: comparing abort with native OCaml exceptions

 The benchmark times raising an exception that unwinds a large portion
 of the stack. We compute the product of the list of numbers, throwing exception
 upon encountering 0. For benchmarking purposes, we intentionally use
 non-tail-recursive product computation, and make sure 0 occurs at the very end
 of the list.

*)

open Delimcc;;

(* Make the list for the benchmark: all ones with 0 at the end *)

let make_test_data n = 
  let rec loop acc = function
    | 0 -> List.rev (0::acc)
    | n -> loop (1::acc) (pred n)
  in loop [] n;;

exception Zero

let test1_ex lst =
  let f x acc =  if x = 0 then raise Zero else x * acc
  in
  try List.fold_right f lst 1 with Zero -> 0
;;

let 6 = test1_ex [1;2;3];;
let 0 = test1_ex [1;2;0;3];;

let test1_abort lst =
  let p = new_prompt () in
  let f x acc =  if x = 0 then abort p 0 else x * acc
  in
  push_prompt p (fun () -> List.fold_right f lst 1)
;;

let 6 = test1_abort [1;2;3];;
let 0 = test1_abort [1;2;0;3];;


exception Other

(* Testing throwing an exception in the presence of many irrelevant handlers*)

let test2_ex lst =
  let f x acc =  if x = 0 then raise Zero else x * acc in
  let rec loop acc = function
    | []   -> acc
    | h::t -> try f h (loop acc t) with Other -> -1
  in
  try loop 1 lst with Zero -> 0
;;


let 6 = test2_ex [1;2;3];;
let 0 = test2_ex [1;2;0;3];;


let test2_abort lst =
  let p  = new_prompt () in
  let p' = new_prompt () in
  let f x acc =  if x = 0 then abort p 0 else x * acc in
  let rec loop acc = function
    | []   -> acc
    | h::t -> push_prompt p' (fun () -> f h (loop acc t))
  in
  push_prompt p (fun () -> List.fold_right f lst 1)
;;

let 6 = test2_abort [1;2;3];;
let 0 = test2_abort [1;2;0;3];;


(* Running the benchmark *)

(* Time the execution *)
let timeit thunk =
  let time_start = Sys.time () in
  let r = thunk () in
  Printf.printf "\nTime spent: %g sec\n" (Sys.time () -. time_start);
  r;;

let bench testf =
    for i = 1 to 5 do
      timeit (fun () -> 
	Printf.printf "result: %d\n" (testf ()))
    done
;;

(* A longer list causes stack overflow *)

let () = 
  Printf.printf "test1_ex\n\n";
  bench (fun () -> test1_ex (make_test_data 110000));;

let () = 
  Printf.printf "test1_abort\n\n";
  bench (fun () -> test1_abort (make_test_data 110000));;

let () = 
  Printf.printf "test2_ex\n\n";
  bench (fun () -> test2_ex (make_test_data 110000));;

let () = 
  Printf.printf "test2_abort\n\n";
  bench (fun () -> test2_abort (make_test_data 110000));;

(* The median result of 5 runs:
OCaml 3.12

Bytecode:
test1_ex:    0.055 sec
test1_abort: 0.051 sec

test2_ex:    0.056 sec
test2_abort: 0.054 sec


Native code:
test1_ex:    0.039 sec
test1_abort: 0.037 sec

test2_ex:    0.042 sec
test2_abort: 0.039 sec

*)
