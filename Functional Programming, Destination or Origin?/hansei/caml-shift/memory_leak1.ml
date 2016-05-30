(* Check for memory leaks when capturing continuations

 This code kindly submitted by Christophe Deleuze on Jul 4, 2008
 showed memory leak in the previous version of the delimcc library.
 This code should run continuously in constant memory. And now, it does.
 Previously, it ran out of memory after about 6000 iterations.

 The problem is easier to see in a desugared version:

open Delimcc;;
type state = Done | Pause   of (unit, state) subcont;;
let p = new_prompt ();;

let pause () = take_subcont p (fun sk () -> Pause sk);;
let proc () = while true do pause () done; Done;;


let Pause sk = push_prompt p proc;;

let rec sched_loop sk =
   let Pause sk = push_prompt p (fun () -> push_subcont sk (fun () -> ()))
   in sched_loop sk
;;

# sched_loop sk;;

Reallocating OCaml stack!
caml_stack_low       0x28266000
caml_stack_high      0x2826a000
caml_stack_threshold 0x28266400
caml_extern_sp       0x28269ea4
caml_trapsp          0x28269ec4

Reallocating OCaml stack!
...


One can use (show_val sk) to see that sk grows and grows...

let rec sched_loop sk =
   let Pause sk = push_prompt p (fun () -> push_subcont sk (fun () -> ()))
   let () = show_val 3 (Obj.repr sk) in
   let Pause sk = push_prompt p (fun () -> push_subcont sk (fun () -> ()))
   let () = show_val 3 (Obj.repr sk) in
   ()
;;

 When the captured continuation 'sk', after resumption,
 captures the continuation again, the captured continuation includes
 push_subcont and the previous 'sk' on the stack. That is, push_subcont
 is not being treated as a tail call. Therefore, the captured continuation
 grows by 13 stack slots on each capture, and eventually runs out of memory.
 
 The solution is to replace
     push_prompt prompt (fun () -> push_subcont sk v)
 with the optimized primitive push_prompt_subcont. The primitive pushed
 the captured continuation, placing the delimiter at the end. The primitive
 is used in the implementation of shift.

*)


open Delimcc;;

type 'a rproc = 'a -> state
and state = 
  | Done
  | Pause   of unit rproc

let runq   = ref ([] : unit rproc list)
and pauseq = ref ([] : unit rproc list)

let prompt = new_prompt ()

let pause () = shift prompt (fun sk -> Pause sk)

let resume sk a = sk a

let put_in_runq p = runq := p :: !runq

let instant = ref 1

let next_instant () =
  runq:=!pauseq; pauseq:=[];
  incr instant;;

let rec sched () =
  match !runq with
  | [] -> 
      next_instant ();
      Printf.printf "eoi %i\n" !instant; flush stdout; (*input_char stdin;*)
      if !runq=[] then () else sched ()
          
  | k::t -> begin runq := t; match resume k () with

    | Done  -> sched ()

    | Pause k -> 
        pauseq:=k::!pauseq;
        sched ()
  end


let spawn proc = runq := proc :: !runq;;

let start pl =
  List.iter spawn pl;
  sched ()
;;

let rec pr1 () =
  print_string "coucou\n";
  pause ();
  pr1 ();;

let pri1 () =
  while true do
    print_string "coucou\n";
    pause ()
  done;
  Done;;


(*input_char stdin;*)
start [ (fun () -> push_prompt prompt (fun ()-> pri1 ())) ];;

