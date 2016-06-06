(**
	delimcc: Control operators for delimited continuations

 This library implements the variety of delimited control operators
 for byte-code OCaml. The code implements the superset of the interface
 proposed by Dybvig, Sabry, and Peyton-Jones.

 The library also supports serialization of captured delimited
 continuations.

 $Id: delimcc.mli,v 1.3 2006/02/02 01:29:28 oleg Exp $

*)

type 'a prompt
type ('a,'b) subcont


val new_prompt   : unit -> 'a prompt

val push_prompt  : 'a prompt -> (unit -> 'a) -> 'a
val take_subcont : 'b prompt -> (('a,'b) subcont -> unit -> 'b) -> 'a
val push_subcont : ('a,'b) subcont -> (unit -> 'a) -> 'b
val push_delim_subcont : ('a,'b) subcont -> (unit -> 'a) -> 'b

(*
 * Less spartan interface: definition of the commonly used
   delimited control operators, and a more efficient implementation of
   abort. Although abort is easily expressible via take_subcont,
   the implementation below is essentially as efficient as OCaml's raise
   (since we do not need to capture any continuation).
 *)

val shift   : 'a prompt -> (('b -> 'a) -> 'a) -> 'b
val shift0  : 'a prompt -> (('b -> 'a) -> 'a) -> 'b
val control : 'a prompt -> (('b -> 'a) -> 'a) -> 'b
val abort   : 'a prompt -> 'a -> 'b

val is_prompt_set : 'a prompt -> bool


(* Various debugging and inspection functions *)
val debug_status : string -> unit

(* Describe an OCaml value up to the specified nesting depth 
   (the first argument), writing the result to stderr.
*)
val show_val : int -> Obj.t -> unit

(* Serialization of captured delimited continuations *)

(* The first argument is an arbitrary data structure to act as a reference
   point: it is traversed and any old closures referred to in it are
   treated as global. Usually the argument should be a suitable (empty)
   delimited continuation captured at the start-up, after all global data
   within the program have been established.
*)
val init_global_closure : 'a -> unit
val register_global_closure : ('a -> 'b) -> unit

val output_delim_value : out_channel -> 'a -> unit

val relativitize : Obj.t -> bool -> Obj.t
val absolutize : Obj.t -> Obj.t
