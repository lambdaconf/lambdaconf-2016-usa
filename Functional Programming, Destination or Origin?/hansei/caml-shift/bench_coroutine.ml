(*
 A benchmark of delimcc: comparing ocaml-callcc (undelimited continuations)
 with delimcc (delimited continuation library).
 The main goal is to compare performance; we can't help but notice the
 difference in the comprehensibility of the code.
*)

(* The following is a slightly modified test 'coroutines'
   from the file test.ml (version 1.2) of the ocaml-callcc distribution.
   Xavier Leroy, Oct 2005.
   The modification is to parameterize by nmax and to add a flag to switch
   off the printing.
*)

open Callcc;;

let coroutines_cc debug nmax =
  callcc (fun init_k ->
    let curr_k = ref init_k in
    let communicate x = 
      callcc (fun k -> let old_k = !curr_k in curr_k := k; throw old_k x) in
    let rec process1 n =
      if n >= nmax then begin
        if debug then 
	  begin print_string "1: finished"; print_newline() end;
	100
      end else begin
        if debug then
	  begin print_string "1: received "; print_int n; print_newline()
	end;
        process1(communicate(n+1))
      end
    and process2 n =
      if n >= nmax-10 then begin
        if debug then
	  begin print_string "2: finished"; print_newline() end;
	nmax / 2
      end else begin
        if debug then
	  begin print_string "2: received "; print_int n; print_newline()
	end;
        process2(communicate(n+1))
      end in
    process1(callcc(fun start1 ->
      process2(callcc(fun start2 ->
        curr_k := start2; throw start1 0)))))
;;

(* Exercise to the reader: try to predict the result of the following
   expression.
*)
(*
coroutines_cc true 30;;
*)

let 100 = coroutines_cc false 30;;

(* Execute a thunk at a particular stack depth *)
let rec at_depth n thunk =
  if n = 0 then thunk ()
  else 1 + at_depth (n-1) thunk (* Not tail recursive! *)
;;

(* An implementation using delimited continuations. There are no longer
   any mutations. The code is much easier to understand. The code
   for process1 and process2 is taken verbatim from 
   coroutines_cc code above.
*)

open Delimcc;;

type process = 
  | Done of int 
  | Ret  of int
  | Suspend of int *  (int -> process)
;;


let coroutines_dc debug nmax =
  let p = new_prompt () in
  let communicate x = shift0 p (fun k -> Suspend (x,k))
  in
    (* begin code taken verbatim from coroutines_cc *)
    let rec process1 n =
      if n >= nmax then begin
        if debug then 
	  begin print_string "1: finished"; print_newline() end;
	100
      end else begin
        if debug then
	  begin print_string "1: received "; print_int n; print_newline()
	end;
        process1(communicate(n+1))
      end
    and process2 n =
      if n >= nmax-10 then begin
        if debug then
	  begin print_string "2: finished"; print_newline() end;
	nmax / 2
      end else begin
        if debug then
	  begin print_string "2: received "; print_int n; print_newline()
	end;
        process2(communicate(n+1))
      end in  (* end code copied verbatim from coroutines_cc *)
    (* The main loop *)
    let rec loop curr = function 
      | Suspend (x,k) -> loop k (curr x)
      | Ret x         -> loop curr (curr x)
      | Done x        -> x
    in
    loop (fun x -> push_prompt p (fun () -> Ret (process2 x)) )
       (push_prompt p (fun () -> Done (process1 0)))
;;

let 100 = coroutines_dc false 30;;

(* testing
coroutines_cc true 20;;
coroutines_dc true 20;;

at_depth 5 (fun () -> coroutines_cc true 20);;
at_depth 5 (fun () -> coroutines_dc true 20);;

at_depth 5 (fun () -> coroutines_cc true 30);;
at_depth 5 (fun () -> coroutines_dc true 30);;
*)

(*  Holdover
let coroutines_dc_nt debug nmax =
  let p = new_prompt () in
  let communicate x =
    shift0 p (fun k -> Suspend (x,k))
  in
    let rec process1 n =
      if n >= nmax then 100
      else begin
        if debug then
	  begin print_string "1: received "; print_int n; print_newline()
	end;
        let res = process1(communicate(n+1)) in
        if debug then 
	  begin print_string "1: finished"; print_newline() end;
        res
      end
    and process2 n =
      if n >= nmax-10 then nmax / 2
      else begin
        if debug then
	  begin print_string "2: received "; print_int n; print_newline()
	end;
        let res = process2(communicate(n+1)) in
        if debug then
	  begin print_string "2: finished"; print_newline() end;
        res
      end in
    let rec loop curr = function 
      | Suspend (x,k) -> loop k (curr x)
      | Ret x         -> loop curr (curr x)
      | Done x        -> x
    in
    loop (fun x -> push_prompt p (fun () ->
      Ret (process2 x)) )
       (push_prompt p (fun () -> Done (process1 0)))
;;

let 100 = coroutines_dc_nt false 30;;
*)


(* Running the benchmark *)

(* Time the execution *)
let timeit thunk =
  let time_start = Sys.time () in
  let r = thunk () in
  (r, Sys.time () -. time_start)
;;

let bench testf =
    let nmax = 5 in
    let rec loop res acc = function
      | 0 -> Printf.printf "median timing %g sec\n" 
	    (List.nth (List.sort compare acc) (nmax/2));
	    res
      | n -> 
	    let (r,timing) = timeit testf in
	    begin
	      match res with
	      | None -> ()
	      | Some x -> if not (r = x) then
		  failwith "Results of different runs differ"
            end;
	    loop (Some r) (timing::acc) (n-1)
    in loop None [] nmax
;;


bench (fun () -> coroutines_cc false 100000);;
(* 0.37 sec *)

bench (fun () -> coroutines_dc false 100000);;
(* 1.14 sec *)


(*
bench (fun () -> at_depth 100 (fun () -> coroutines_cc false 100000));;
(* 7.1 sec *)


bench (fun () -> at_depth 100 (fun () -> coroutines_dc false 100000));;
(* 1.15 sec *)
*)
(*
for i = 0 to 10 do
  let d = i * 10 in
  match bench (fun () -> at_depth d (fun () -> coroutines_cc false 100000)) with
    Some x -> Printf.printf "Depth %d result %d\n" d x
done
;;

median timing 0.560035 sec
Depth 0 result 100
median timing 0.676042 sec
Depth 10 result 110
median timing 2.65217 sec
Depth 20 result 120
median timing 4.05225 sec
Depth 30 result 130
median timing 4.7283 sec
Depth 40 result 140
median timing 5.39234 sec
Depth 50 result 150
median timing 6.06438 sec
Depth 60 result 160
median timing 6.73642 sec
Depth 70 result 170
median timing 7.42046 sec
Depth 80 result 180
median timing 8.09251 sec
Depth 90 result 190
median timing 8.74855 sec
Depth 100 result 200


for i = 0 to 15 do
  let d = i * 10 in
  match bench (fun () -> at_depth d (fun () -> coroutines_dc false 100000)) with
    Some x -> Printf.printf "Depth %d result %d\n" d x
done
;;

median timing 1.13607 sec
Depth 0 result 100
median timing 1.14407 sec
Depth 10 result 110
median timing 1.14007 sec
Depth 20 result 120
median timing 1.14407 sec
Depth 30 result 130
median timing 1.14007 sec
Depth 40 result 140
median timing 1.14407 sec
Depth 50 result 150
median timing 1.14407 sec
Depth 60 result 160
median timing 1.14807 sec
Depth 70 result 170
median timing 1.14407 sec
Depth 80 result 180
median timing 1.14807 sec
Depth 90 result 190
median timing 1.14807 sec
Depth 100 result 200
median timing 1.14807 sec
Depth 110 result 210
median timing 1.14407 sec
Depth 120 result 220
median timing 1.15207 sec
Depth 130 result 230
median timing 1.14807 sec
Depth 140 result 240
median timing 1.15207 sec
Depth 150 result 250
*)
