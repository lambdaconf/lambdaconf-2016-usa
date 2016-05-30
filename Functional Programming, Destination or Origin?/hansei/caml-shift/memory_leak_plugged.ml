(*
  Illustration of the plugged memory leak, based on the code kindly
  submitted by Gleb Alexeev on May 5, 2008.

  Successively stored continuations have the constant size of 473 bytes 
  (Oct 2009 version: 387 bytes).

  Compile this file and run the executable several times. First time,
  it creates the file /tmp/kont.k with the serialized suspension.
  Run next time, the code resumes the stored suspension, and stores
  a new one. The code prints the iteration counter, to show the 
  iteration progression.

*)

open Delimcc

let contfilename = "/tmp/kont.k"


type k_t = (unit,unit) subcont

let save_cont k =
  let oc = open_out contfilename in
    begin
      output_delim_value oc k;
      close_out oc;
      () (* show_val 1 (Obj.repr k) *)
    end

let suspend p () =
  take_subcont p (fun sk () -> save_cont sk; Printf.eprintf "exit\n")

let resume_saved_cont () =
  let ic = open_in contfilename in
  let (sk : k_t)  = Obj.obj (absolutize (Marshal.from_channel ic)) in
    begin
      close_in ic;
      push_subcont sk (fun () -> ())
    end


let rec main_loop p i =
  let () = Printf.eprintf "*** iteration %d\n" i in
  let () = suspend p () in
    push_prompt p (fun () -> main_loop p (i+1))

let main p () =
  try resume_saved_cont ()
  with Sys_error _ -> main_loop p 1

let kempty = 
  let p = new_prompt () in Obj.repr
  (push_prompt p (fun () -> take_subcont p (fun sk () -> Obj.magic sk)));;

let () = init_global_closure kempty

let () =
  begin
    (* init_global_closure 
    (main, resume_saved_cont, main_loop, suspend, save_cont, contfilename); *)
    let p = new_prompt () in
    (push_prompt p (main p))
  end

