(* Light-weight concurrency library with MVAR as the synchronization primitive

   The library is a slight modification of the code originally written and
   kindly shared by Christophe Deleuze, July 18, 2012.
*)

(* A task to execute concurrently *)
type task = unit -> unit

(* A process that executes the task
   The process wraps the task in a prompt and the termination handler
 *)
type process = unit -> unit


open Delimcc

(* The process prompt *)
let pp = new_prompt ()

(* The queue of runnable processes *)
let runq = Queue.create ()
let enqueue p  = Queue.push p runq
let dequeue () = Queue.take runq

(* Spawn a new task and continue with the parent process *)
let spawn : task -> unit = fun t ->
  let p () =                            (* build a process *)
    try 
      push_prompt pp t                  (* return when finished or blocked *)
    with e ->
      Printf.printf "task terminated abnormally with %s\n"
        (Printexc.to_string e)
  in enqueue p
;;
      
(* voluntarily switch to another process *)
let yield : unit -> unit = fun () ->
  shift0 pp enqueue
;;

(* Stop the system: reboot *)
exception Stop
let stop () = raise Stop

(* Scheduling loop *)
let start () =
  try
    while true do
      dequeue () ()
    done
  with Queue.Empty | Stop -> ()

(* An implementation of Haskell's MVar: a synchronized queue of length 1 *)
type 'a mvar = { 
    mutable v: 'a option; 
    mutable readq:  ('a -> unit) list;  (* cont of processes blocked on take *)
    mutable writeq: (process * 'a) list (* processes blocked on put          *)
  }

let make_mvar () = { v=None; readq=[]; writeq=[] }

(* Put a value in a queue; block if the MVar is full *)
let put_mvar : 'a mvar -> 'a -> unit = fun mv vnew ->
  match mv with
  | {v=None; readq=[]}     -> mv.v <- Some vnew
  | {v=None; readq=rd::rq} ->    (* unblock a reader *)
      mv.readq <- rq;
      enqueue (fun () -> rd vnew)
  | {v=Some _; writeq=pq} -> 
      shift0 pp (fun p -> mv.writeq <- (p,vnew) :: pq)

(* Take a value from a queue; block if empty *)
let take_mvar : 'a mvar -> 'a = function
  | {v=Some v'; writeq=[]} as mv         -> mv.v <- None; v'
  | {v=Some v'; writeq=(p,vp)::wq} as mv ->  (* wake up a writer *)
      mv.v <- Some vp;
      mv.writeq <- wq;
      enqueue p;
      v'
  | {v=None; readq=rq} as mv ->
      shift0 pp (fun k -> mv.readq <- k::rq)
