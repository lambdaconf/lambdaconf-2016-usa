(* A test suite for delimited continuations *)

open Delimcc
open Printf

let abort' p e = take_subcont p (fun _ -> e);; (* less-efficient abort *)

let shouldbe msg fmt vexp v =
  if v = vexp 
     then printf "%s gave the expected answer %s\n" msg (fmt v)
     else 
      (printf "%s:expected answer %s obtained %s\n" msg (fmt vexp) (fmt v);
       failwith "unexpected answer")
;;

let int = string_of_int;;

let test1 = 
  let () = printf "test1\n" in
  let p = new_prompt () in
  assert (not (is_prompt_set p));
  shouldbe "test1" int 1 
    (push_prompt p (fun () -> assert (is_prompt_set p); 1))
;;
(* 1 *)

let test2 = 
  let () = printf "test2\n" in
  let p = new_prompt () in
  let v = push_prompt p (fun () -> push_prompt p (fun ()->5)) in
  shouldbe "test2" int 9 (v+4)
;;
(* 9 *)

let test3 = 
  let () = printf "test3\n" in
  let p = new_prompt () in
  let v = push_prompt p (fun () -> (abort p 5) + 6) in
  shouldbe "test3" int 9 (v+4)
;;
(* 9 *)

let test3 = 
  let () = printf "test3, less efficient\n" in
  let p = new_prompt () in
  let v = push_prompt p (fun () -> (take_subcont p (fun _ () -> 5)) + 6) in
  shouldbe "test3" int 9 (v+4)
;;
(* 9 *)

let test3' = 
  let () = printf "test3'\n" in
  let p = new_prompt () in
  let v = push_prompt p (fun () ->
    push_prompt p (fun () -> (abort p 5) + 6)) in
  shouldbe "test3'" int 9 (v+4)
;;
(* 9 *)

let test3'' = 
  let () = printf "test3''\n" in
  let p = new_prompt () in
  let v = push_prompt p (fun () ->
    let _  = push_prompt p (fun () -> (abort p 5) + 6) in
    let v1 = abort p 7 in
    v1 + 10) in
  shouldbe "test3''" int 27 (v+20)
;;
(* 27 *)

let test3'' = 
  let () = printf "test3'', less efficient\n" in
  let p = new_prompt () in
  let v = push_prompt p (fun () ->
    let _ = push_prompt p (fun () -> (abort' p (fun () -> 5)) + 6) in
    let v1 = abort' p (fun () -> 7) in
    v1 + 10) in
  shouldbe "test3''" int 27 (v+20)
;;
(* 27 *)

let test3''' = 
  try 
  let () = printf "test3'''\n" in
  let p = new_prompt () in
  let _ = push_prompt p (fun () ->
    let _ = push_prompt p (fun () -> (abort p 5) + 6) in
    let v1 = abort p 7 in
    v1 + 10) in 
  assert( not (is_prompt_set p));
  let _ = abort p 9 in
  assert false
  with Failure e -> printf "expected error: %s\n" e
;;
(* Failure *)

let test3''' = 
  try 
  let () = printf "test3''', less efficient\n" in
  let p = new_prompt () in
  let _ = push_prompt p (fun () ->
    let _ = push_prompt p (fun () -> (abort' p (fun () -> 5)) + 6) in
    let v1 = abort' p (fun () -> 7) in
    v1 + 10) in 
  assert( not (is_prompt_set p));
  let _ = abort p 9 in
  assert false
  with Failure e -> printf "expected error: %s\n" e
;;
(* Failure *)
(* Compactification is the most brutal GC operation. If the heap is corrupted,
   most likely compactification will fail with some error, such as segmentation
   fault.
*)
Gc.compact();;

let test4 = 
  let () = printf "\ntest4\n" in
  let p = new_prompt () in
  let v = push_prompt p (fun () -> 
    let v1 = take_subcont p (fun sk () -> 
      push_prompt p (fun () -> push_subcont sk (fun () -> 
	Gc.full_major(); 5)))
    in v1 + 10) in
  shouldbe "test4" int 35 (v+20)
;;
(* 35 *)

(* The classical shift test 
  (display (+ 10 (reset (+ 2 (shift k (+ 100 (k (k 3))))))))
  ; --> 117
*)


let test5 = 
  let () = printf "\ntest5\n" in
  let p0 = new_prompt () in
  let _ = new_prompt () in
  let v0 = push_prompt p0 (fun () -> 
    let v = shift p0 (fun sk -> sk (sk 3) + 100)
    in v + 2) in
  shouldbe "test5" int 117 (v0+10)
;;
(* 117 *)

let test5' = 
  let () = printf "\ntest5'\n" in
  let p0 = new_prompt () in
  let _  = new_prompt () in
  let v0 = push_prompt p0 (fun () -> 
    let v = shift p0 (fun sk -> sk 3 + 100)
    in v + 2) in
  shouldbe "test5'" int 115 (v0+10)
;;
(* 115 *)

let test5'' = 
  let () = printf "\ntest5''\n" in
  let p0 = new_prompt () in
  let p1 = new_prompt () in
  let v0 = push_prompt p0 (fun () -> 
    let v = shift p0 (fun sk -> 
      sk (push_prompt p1 (fun () -> 9 + sk (abort p1 3))) + 100)
    in v + 2) in
  shouldbe "test5''" int 115 (v0+10)
;;
(* 115 *)

let test5''' = 
  let () = printf "\ntest5'''\n" in
  let p0 = new_prompt () in
  let p1 = new_prompt () in
  let v0 = push_prompt p0 (fun () -> 
    let v = shift p0 (fun sk -> 
      sk (fun () ->
	push_prompt p1 
	  (fun () -> 9 + sk (fun () -> abort p1 3))) + 100)
    in v () + 2) in
  shouldbe "test5'''" int 115 (v0+10)
;;

let test54 = 
  let () = printf "\ntest54\n" in
  let p0 = new_prompt () in
  let p1 = new_prompt () in
  let v0 = push_prompt p0 (fun () -> 
    let v = shift p0 (fun sk -> 
      sk (fun () ->
	push_prompt p1 
	  (fun () -> 9 + sk (fun () -> abort p0 3))) + 100)
    in v () + 2) in
  shouldbe "test54" int 124 (v0+10)
;;


let test6 = 
  let () = printf "\ntest6\n" in
  let p1 = new_prompt () in
  let p2 = new_prompt () in
  let pushtwice sk = push_subcont sk 
      (fun () -> push_subcont sk (fun () -> 3)) in
  let v0 = push_prompt p1 (fun () -> 
    push_prompt p2 (fun () -> 
      take_subcont p1 (fun sk () -> pushtwice sk)) + 1) in
  shouldbe "test6" int 15 (v0+10)
;;


(* The most difficult test *)
let test7 = 
  let () = printf "\ntest7\n" in
  let p1 = new_prompt () in
  let p2 = new_prompt () in
  let p3 = new_prompt () in
  let pushtwice sk = 
    push_subcont sk (fun () -> 
      push_subcont sk (fun () ->
	take_subcont p2 (fun sk2 () -> 
	  push_subcont sk2 (fun () -> 
	    push_subcont sk2 (fun () -> 3))))) in
  let v = push_prompt p1 (fun () -> 
    push_prompt p2 (fun () -> 
      push_prompt p3 (fun () -> 
	take_subcont p1 (fun sk () -> pushtwice sk)) + 10) + 1) in
  shouldbe "test7" int 135 (v+100)
;;

let test7' = 
  let () = printf "\ntest7'\n" in
  let p1 = new_prompt () in
  let p2 = new_prompt () in
  let p3 = new_prompt () in
  let pushtwice sk = 
    sk (fun () -> 
      sk (fun () ->
	shift p2 (fun sk2 -> sk2 (fun () -> sk2 (fun () -> 3))) ())) in
  let v = push_prompt p1 (fun () -> 
    push_prompt p2 (fun () -> 
      push_prompt p3 (fun () -> shift p1 pushtwice ()) + 10) + 1) in
  shouldbe "test7'" int 135 (v+100)
;;
(* 135 *)

let test7'' = 
  let () = printf "\ntest7''\n" in
  let p1 = new_prompt () in
  let p2 = new_prompt () in
  let p3 = new_prompt () in
  let pushtwice sk = 
    sk (fun () -> 
      sk (fun () ->
	shift0 p2 (fun sk2 -> sk2 (fun () -> sk2 (fun () -> 3))) ())) in
  let v = push_prompt p1 (fun () -> 
    push_prompt p2 (fun () -> 
      push_prompt p3 (fun () -> shift0 p1 pushtwice ()) + 10) + 1) in
  shouldbe "test7'" int 135 (v+100)
;;
(* 135 *)


(* Testing interaction of exceptions and delimited continuations *)
exception E1
let test9 = 
  let () = printf "\ntest9: test exceptions\n" in
  let p = new_prompt () in
  try
    push_prompt p (fun () -> try shift p (fun sk -> raise E1)
	                     with E1 -> assert false)
  with E1 -> printf "OK\n"
;;

let test9' = 
  let () = printf "\ntest9': test exceptions\n" in
  let p = new_prompt () in
  try
    push_prompt p (fun () -> 
      try shift p (fun sk -> sk (sk (raise E1)))
      with E1 -> assert false)
  with E1 -> printf "Cleared to top. Now check that prompts are cleared\n";
  assert (not (is_prompt_set p))
;;

let test9'' = 
  let () = printf "\ntest9'': test exceptions\n" in
  let p = new_prompt () in
  try
    push_prompt p (fun () -> 
      try shift p (fun sk -> sk (fun () -> sk (fun () -> raise E1))) ()
      with E1 -> printf "Captured\n"; raise E1)
  with E1 -> printf "Cleared to top. Now check that prompts are cleared\n";
  assert (not (is_prompt_set p))
;;

Gc.compact();;

debug_status "before test9'''";;

let test9''' = 
  let () = printf "\ntest9''': test exceptions\n" in
  let p = new_prompt () in
  shouldbe "test9'''" int 21
    (push_prompt p (fun () -> 
      10 +
	(try push_prompt p (fun () -> debug_status "in test9'''";
	                              (raise E1) + 800)
	 with E1 -> shift p (fun sk -> sk (sk 1)))))
;;

(*
 (reset (let ((x (shift f (cons 'a (f '()))))) (shift g x))))
  ==> '(a)
*)

let testls = 
  let () = printf "\ntestls\n" in
  let p = new_prompt () in
  shouldbe "list length" int 1
    (List.length
       (push_prompt p (fun () -> 
	 let x = shift p (fun f -> "a" :: f [])
	 in abort p x)))
;;

let testlc = 
  let () = printf "\ntestlc\n" in
  let p = new_prompt () in
  shouldbe "list length"  int 0
    (List.length
       (push_prompt p (fun () -> 
	 let x = control p (fun f -> "a" :: f [])
	 in abort p x)))
;;

let testlc' = 
  let () = printf "\ntestlc'\n" in
  let p = new_prompt () in
  shouldbe "list length" int 1
    (List.length
       (push_prompt p (fun () -> 
	 let x = control p (fun f -> "a" :: f [])
	 in control p (fun g -> g x))))
;;

(* traversing puzzle by Olivier Danvy *)
let traverse op lst =
  let p = new_prompt () in
  let rec visit = 
    function [] -> [] 
      | (h::t) -> visit (op p (fun f -> h :: (f t))) in
  push_prompt p (fun () -> visit lst)
;;

let test_t1 = 
  let () = printf "\ntest_t1\n" in
  let res = traverse shift [1;2;3;4;5] in
  List.iter (fun e -> printf " %d" e) res; printf "\n"
;;
(* 1 2 3 4 5 *)

let test_t2 = 
  let () = printf "\ntest_t2\n" in
  let res = traverse control [1;2;3;4;5] in
  List.iter (fun e -> printf " %d" e) res; printf "\n"
;;
(* 5 4 3 2 1 *)
