(* Testing the serialization of captured delimited continuations *)
(* If the code is given an argument, we load the previously serialized
   continuation from the file
*)

open Delimcc

open Printf

let () = prerr_endline "\nInitializing tests0\n"

let save_state_file fname k =
  let oc = open_out fname in
  let () = output_delim_value oc k in
  close_out oc
;;

let load_state_file fname =
  let ic = open_in fname in
  let v = Marshal.from_channel ic in
  let () = close_in ic in
  v
;;


(* establish global data *)
type 'a k = Done of 'a | K of Obj.t;;
let p0 : (int k) prompt = new_prompt ();;

let kempty = Obj.repr
  (push_prompt p0 (fun () -> take_subcont p0 (fun sk () -> Obj.magic sk)));;

(* let () = show_val 5 kempty *)

	(* Define the point for relativitization*)
let () = init_global_closure kempty

(*
let kempty1 = Obj.repr
  (push_prompt p0 (fun () -> shift p0 (fun f -> Obj.magic f)));;
let krel = relativitize kempty1 true;;
*)

(* End of global data *)

(* Old test
let () =
  let n = 1 in
  let m = 2 in
  let fn = ref (fun (x,y) u -> 
    prerr_int (m+n+u); prerr_float x; prerr_endline y) in
  let f2 x = prerr_endline "f2"; x in
  let f3 () = prerr_endline "f3"; n in
  let () = Gc.minor () in
  let fv = ((fun x -> fun y -> !fn (x,y)), f2, f3) in
  let fvr = Obj.repr fv in
  let () = prerr_endline "simple_test" in
  let () = show_val 5 fvr in
  let fv' = relativitize fvr true in
  let () = show_val 5 fv' in
  let () = show_val 5 (Obj.repr fv) in
  let (fvn,f2n,f3n) = (Obj.obj fv') in
  let () = fvn (7.0) "OK" 10; f2n (); f3n () in
  let (fvn,f2n,f3n) = fv in
  fvn (7.0) "OK" 10; f2n (); f3n (); ()
  (* ; exit 0 *)
*)



(*
let krel = relativitize kempty true;;
let () = show_val 5 krel;;

let kempty = 
  let p = new_prompt () in
  push_prompt p (fun () ->
    take_subcont p (fun sk () -> Obj.repr sk)
      );;

let kempty = 
  push_prompt p0 (fun () -> shift p0 (fun f -> Obj.repr f));;

let krel = relativitize kempty false;;

let () = save_state_file "/tmp/k0" krel;
         print_endline "saved"
*)

let () = print_endline "shift test\n"

let appk1 k = let Done v = ((Obj.obj k):(int->int k)) 5
    in v;;

let create_and_save () =
 let k1 = let K v = push_prompt p0 (fun () -> 
  Done (shift p0 (fun (f: int -> int k) -> K (Obj.repr f)) 
	  + 10))
    in v
 in
 let () = show_val 5 k1 in
(* 
 let k1r = relativitize k1 false in 
 let () = show_val 5 k1r in
 let () = print_endline "trying an application";
   printf "the result is %d\n\n" (appk1 k1r) in
*)
 let () = save_state_file "/tmp/k1" k1;
          print_endline "saved" in
 let () = print_endline "trying an application after saving";
   printf "the result is %d\n\n" (appk1 k1) in
 ()

(*
let krest = 
  Obj.obj (absolutize (Obj.repr k1r))
;;
let () = show_val 5 krest;;
*)

let (need_saving,fname) = match Sys.argv with
 |  [|_|] -> (true,"/tmp/k1")
 |  [|_;fname|] -> printf "Loading from %s\n" fname; (false, fname)

let () = if need_saving then create_and_save () else ()

let () = print_endline "loading back\n"

let krest = 
  let v = load_state_file "/tmp/k1" in
  let () = show_val 5 v in
  Obj.obj (absolutize (Obj.repr v))
;;

let () = print_endline "trying an application";
  printf "the result is %d\n\n" (appk1 krest)
;;

