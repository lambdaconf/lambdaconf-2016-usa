(* Light-weight concurrency library with the Haskell-like MVAR, 
   a synchronized queue of length 1

   The library is a slight modification of the code originally written and
   kindly shared by Christophe Deleuze, July 18, 2012.
*)

(* A task to execute concurrently *)
type task = unit -> unit

(* Spawn a new task and continue with the parent process *)
val spawn : task -> unit

(* voluntarily switch to another process *)
val yield : unit -> unit


(* Start the concurrency system: schedule processes until
   all are finished or blocked, or when the system stopped.
*)
val start : unit -> unit

(* Stop the system: reboot *)
val stop : unit -> 'a

(* A 1-element synchronized queue for data of type 'a *)
type 'a mvar                            (* abstract *)

(* Make an empty MVAR *)
val make_mvar : unit -> 'a mvar

(* Put a value in a queue; block if the MVar is full *)
val put_mvar : 'a mvar -> 'a -> unit

(* Take a value from a queue; block if empty *)
val take_mvar : 'a mvar -> 'a

