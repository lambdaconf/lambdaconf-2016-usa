(**
		Control operators for delimited continuations

 This code implements the variety of delimited control operators
 for byte- and native-code OCaml. The code implements the superset of 
 the interface proposed by Dybvig, Sabry, and Peyton-Jones.

 This code was originally based on CC_Ref.hs -- a template for
 implementing delimited continuations on a system permitting no
 inspection of the stack. We may copy a part of the stack but we cannot
 check the stack for prompt marks. The code underwent several
 optimizations and a revision.

 This is a ``direct'' implementation: it does access the 
 OCaml stack and copies fragments of it. We only copy the needed
 prefix of the stack. Stack access is supported by several external
 functions in the file stacks.c or stacks-native.c. The former code was
 heavily influenced by the stack-copying code of OCaml (OCaml does copy
 the stack to resize it) and by the implementation of callcc by Xavier
 Leroy. Again, the stack cannot be inspected, so we have to implement
 a `parallel' stack to  maintain the data on active prompts. 
 Stack copying for the native code compiler is quite different;
 see the comments in stacks-native.c for details.

 This implementation is based on OCaml exceptions. We copy and reinstall
 portions of stack strictly between two exception frames. As a `side-effect',
 exception frames are captured and reinstated by delimited continuations.
 In fact, each prompt corresponds to an exception frame.

 For future work, consider the segmented-like stack implementation described
 in ``Representing Control in the Presence of First-Class Continuations.''
 by Hieb, Dybvig and Bruggeman.

 This code should avoid using print_endline and any operations that
 directly or indirectly use OCaml's i/o (including exit, which executes
 flush_all as one of the at_exit hooks). Otherwise, the captured continuation
 will include stdout or stderr channels and so will be unserializable.


 Note on serialization of captured continuations

 The captured continuation appears to be relatively compact. For
 example, the essentially identity continuation, captured and returned
 by the following code

   let krepr = 
     let p = new_prompt () in
     push_prompt p (fun () -> take_subcont p (fun sk () -> Obj.repr sk));;

 has the largest component (the fragment of the stack, saved in the
 subcont_ek field of subcont data structure) of only 18 words. That corresponds
 to an exception-handling part of the try block at the end of the
 take_subcont function below. 

 If we examine these 19 stack words using the show_val procedure
 below, we realize how staggeringly large the captured continuation
 is. One of these 19 words is a closure with "mbox_receive p",
 containing `mbox_receive' and `p' in its environment. The procedure
 `mbox_receive', defined in this file, is also a closure, which contains,
 as its environment, *all* of the procedures in this module. Some of
 the procedures include `prerr_string' and other such pervasive
 functions -- which are themselves closures in the Pervasives module,
 containing all of the Pervasives environment. If we naively serialize the
 captured continuation with Marshal.to_string, we will be serializing all
 the data in this module and all of the data in Pervasives -- essentially
 all of global data. There is another, fatal, problem: the closure
 prerr_string contains stderr as part of its environment. The latter is
 a value of the type channel -- which is a Custom value with no
 custom serialization. That precludes serialization of the captured
 continuation.

 The moment of reflection shows that we are not interested in serializing
 any of the global environment. When Marshal.to_string encounters a
 code pointer, it emits just the pointer (actually, its offset from the
 beginning of the code area) rather than attempting to serialize the whole
 code. Why can't we do the same with global data? First, there is a technical
 problem: in bytecode, it is not easy to tell if a value is
 part of global data (loaded from the corresponding section of the executable
 file) or created in the process of running the code. The byte-code
 executable loads all of the global data into OCaml heap. The related,
 and more serious, problem is that whereas code is immutable and offsets
 in the code for the same executable always have the same meaning, the values
 in heap can be removed or moved by GC. 

 Our solution is to `relativitize' the captured continuation before
 serializing it, and `absolutize' it after deserializing. The relativitization
 procedure replaces references to closures in seemingly global data
 with `relative indices', in the global array `global_data'. The array
 contains references to global data themselves, and thus prevents reclaiming
 the data (which is unlikely) and, mainly, makes the indices invariant
 with regard to GC moving values around the heap. We offer a function
 to register `global' data in the array. The `relative index' replacing
 the closure is a custom data type, with a serialization routine that
 merely writes the index. The absolutize procedure replaces the relative
 indices with the corresponding closures.

 It goes without saying that a continuation serialized relative to
 a particular contents of global_data must be deserialized using exactly
 the same contents. We should record a hash of the global_data along
 with the serialized continuation to check for that (although currently
 we don't do that).
 The serialized delimited continuation is thus twice
 delimited: with respect to the whole continuation (the whole stack) and
 with respect to the global environment.

 The relativitized value must be quickly serialized, and then restored
 (absolutized), because we might have disturbed the closures that are are not
 only captured in continuations but also used elsewhere.

 How do we determine what is the global environment to relativitize against?
 It seems reasonable that we determine this automatically: we capture
 an identity continuation and relativitize it, in a _recording_ mode.
 Every seemingly global closure gets recorded. We should execute GC.minor()
 before that, to oldify data structures and so create a checkpoint.
 We should do this procedure right at the program start-up, when
 we have established all the global data.
 In the recoding mode, we do not do any updates.

 Note on the top-level: calling serialization routines from the top-level
 is problematic, because byte-code in use at top-level is not contiguous
 in memory and there is no way to tell a code pointer from a pointer
 to an out-of-OCaml data structure. OCaml's own marshalling routine gets
 confused! When top-level starts, the corresponding bytecode is in the
 area delimited by caml_start_code and caml_start_code+caml_code_size
 (those values are defined in fix_code.h). That is how OCaml's marshalling
 can tell the code pointer from other non-heap pointer. However,
 when the top-level loads .cmo modules specified on the command line
 or via the #load directive, the top-level loads the code into
 a different place in memory, related neither to heap nor to the main
 code area. We can see that clearly from examining the function load_compunit
 in the code toplevel/topdirs.ml. The function has the line
       let code = Meta.static_alloc code_size
 Here, Meta.static_alloc is C malloc. So, the allocated space for the
 module's code has no relationship to OCaml's data structures, and so
 OCaml's marshalling routine treats a code pointer in that area as
 a foreign pointer, and refuses to serialize any data with that pointer.
 The same goes for closures created by the top-level: the code pointer
 of those closures is not within the caml_code area, and so OCaml's
 own marshalling cannot deal with them. The easiest way to see that is to
 execute at the top level:
   # Marshal.to_string (fun () -> ()) [Marshal.Closures];;
   Exception: Invalid_argument "output_value: abstract value (outside heap)".
 We can't serialize even the most trivial closure.
 There is nothing we can do about it; so we should not serialize continuations
 within top-level; at the very least, we should compile a custom top-level
 with all needed modules.


 $Id: delimcc.ml,v 1.7 2006/02/02 01:29:28 oleg Exp $

*)

(*------------------------------- The scAPI to access control stack *)
(* It is mostly supported by external functions, see stacks.c *)

module EK :
    sig
      (** The pointer to an exception frame. It corresponds to caml_trapsp,
          but relative to the top of the stack.
          DelimCCE (see below) has no type parameters, so ek doesn't have
          either. 
      *)
      type ek

      (* The copy of the stack, between two ek 
	 (including the most recent exception frame and excluding the other) 
      *)
      type ekfragment

      val get_ek    : unit -> ek
      val reset_ek  : ek -> exn -> 'a
      val rebase_ek : ek -> ek -> ek -> ek

      val copy_stack_fragment : ek -> ekfragment
      val push_stack_fragment : ekfragment -> exn -> 'a
      val size_stack_fragment : ekfragment -> int  (* in words *)

      val dbg_print_ek    : ek -> unit
      val dbg_print_ek_simple : string -> ek -> unit

      val dbg_fatal_error : string -> 'a
      val dbg_note : string -> unit
   end 
   =
   struct
     (* ek is int from the OCaml point of view and ptrdiff_t from the 
	C point of view.
	The pointer difference is in the units of `value'. Because `value'
	has a sufficient size to hold a pointer, ptrdiff_t in units of value
	will definitely fit within 'value' even taking the tag bit into account.
        See stacks.c
	If you make any changes to stacks.c, be sure to check if the
	following needs adjusting!
     *)
     type ek = int
     type ekfragment			(* Abstract *)
     external get_ek   : unit -> ek = "get_trapsp"
     external reset_ek : ek -> exn -> 'a = "reset_trapsp"
     external dbg_print_ek          : ek -> unit = "dbg_print_trapsp"
     external dbg_print_ek_simple   : string -> ek -> unit 
	 = "dbg_print_trapsp_simple"
     external copy_stack_fragment : ek -> ekfragment
	 = "copy_stack_fragment"
     external push_stack_fragment : ekfragment -> exn -> 'a
	 = "push_stack_fragment"
     external size_stack_fragment : ekfragment -> int
	 = "size_stack_fragment"
     
     external dbg_fatal_error : string -> 'a
	 = "dbg_fatal_error"
     external dbg_note : string -> unit
	 = "dbg_note"
	
     let rebase_ek ek ekbase ekbasen = 
       assert (ek >= ekbase); (ek - ekbase) + ekbasen
   end

open EK



(*------------------------------- Types *)


(** We manipulate portions of the stack between two exception frames:
    The following is the exception type for the frames
    that correspond to push_prompt
*)
exception DelimCCE

type 'a prompt = {mbox : (unit -> 'a) ref;
		  mark  : unit ref}

(** A frame of the parallel stack, associated with each active prompt.
    The frame refers to the prompt indirectly, by pointing to the
    mark field of the prompt. Different prompts have different marks.
    Therefore, although prompts generally have different types, all pframes
    have the same type and can be placed into the same list.
    A pframe also points to an exception frame
    on the control stack of the interpreter (in the pfr_ek field).
    That exception frame is created by push_prompt, see below.
*)
type pframe = {pfr_mark : unit ref; (* The same ref as in the prompt *)
	       pfr_ek   : ek}
type pstack = pframe list ref       (* The parallel stack *)

(** The context between two exception frames
    It includes the copy of the OCaml stack (ekfragment) and the
    corresponding copy of the parallel stack. The latter is a list
    of pframes in inverse order.
    The field pfr_ek of pframes in subcont_ps is no longer a real
    pointer. One should not use the value in that field in isolation.
    Rather, we should compute the difference between pfr_ek and
    subcont_bs. The difference identifies the exception frame
    in ekfragment, as the offset from the base of ekfragment.
*)
type ('a,'b) subcont = {subcont_ek : ekfragment;
			subcont_pa : 'a prompt;
			subcont_pb : 'b prompt;
			subcont_ps : pframe list;
			subcont_bs : ek
                        }



(*------------------------------- The global State *)

let ptop : pstack = ref []		(* The top of the parallel stack *)

(*------------------------------- Utilities *)

let mbox_empty () = failwith "Empty mbox"

let mbox_receive p =
  let k = !(p.mbox) in
  let () = p.mbox := mbox_empty in
  k ()

(** Split the parallel stack at the given mark, remove the prefix
    (up to but not including the marked frame) and return it in
    the inverse frame order. The frame that used to be at the top of pstack
    is now at the bottom of the returned list.
    The other two returned values are the marked frame and the
    rest of pstack (which contains the marked frame at the top).
*)

let rec unwind acc mark = function
  | []   -> failwith "No prompt was set" 
  | h::t as s -> if h.pfr_mark == mark (* Physical equality ! *)
                 then (h,s,acc) else unwind (h::acc) mark t

(* The same as above, but the removed frames are disregarded *)
let rec unwind_abort mark = function
  | []   -> failwith "No prompt was set" 
  | h::t as s -> if h.pfr_mark == mark (* Physical equality ! *)
                 then (h,s) else unwind_abort mark t

(*------------------------------- Exported operations *)

let new_prompt () : 'a prompt = 
  {mbox = ref mbox_empty; mark = ref ()}

(* The wrapper for the body; captured in the continuation *)
let push_prompt_aux (p : 'a prompt) (body : unit -> 'a) : 'any =
  let () = ptop := {pfr_mark = p.mark; pfr_ek = get_ek ()} :: (!ptop) in
  let res = body () in
  p.mbox := (fun () -> res);
  raise DelimCCE

let push_prompt (p : 'a prompt) (body : unit -> 'a) : 'a =
  try
    push_prompt_aux p body
  with
    (* whatever the exception, we remove the pframe: drop the
       continuation delimiter
    *)
  | DelimCCE -> (match !ptop with
    | h::t -> assert (h.pfr_mark == p.mark); ptop := t; mbox_receive p
    | _ -> dbg_fatal_error "push_prompt: empty pstack on DelimCCE")
  | e -> match !ptop with
    | h::t -> assert (h.pfr_mark == p.mark); ptop := t; 
	dbg_note "propagating exc"; raise e
    | _ -> dbg_fatal_error "push_prompt: empty pstack on other exc"

(*
(* ------- Begin expository version *)
(* Like push_prompt but without mentioning the prompt in pstack: push
   an inactive prompt so to speak, used only as the mailbox.
   This corresponds to fresh prompts p' and p'' of the machine \Mdci
*)

let push_prompt_simple (p : 'a prompt) (body : unit -> unit) : 'a =
  try body (); raise DelimCCE
  with
  | DelimCCE -> mbox_receive p
  | Out_of_memory -> dbg_fatal_error "take_subcont: out of memory"
  | e -> dbg_note "propagating exc"; raise e

let take_subcont (p : 'b prompt) (f : ('a,'b) subcont -> unit -> 'b) : 'a =
  let p' = new_prompt () in
  push_prompt_simple p'
    (fun () ->
      let (h,s,subcontchain) = unwind [] p.mark !ptop in
      let () = ptop := s in
      let ek = h.pfr_ek in
      let ekfrag = copy_stack_fragment ek in
      p.mbox := 
      f {subcont_ek = ekfrag; subcont_pa = p';
	 subcont_pb = p; subcont_ps = subcontchain;
         subcont_bs = ek};
      reset_ek ek DelimCCE)

let push_subcont (sk : ('a,'b) subcont) (m : unit -> 'a) : 'b =
  let p'' = new_prompt () in
  push_prompt_simple p'' (fun () ->
   try
    let base = sk.subcont_bs in
    let ek = get_ek () in
    List.iter (fun pframe ->
       ptop := {pframe with pfr_ek = rebase_ek pframe.pfr_ek base ek} ::
	    !ptop) sk.subcont_ps;
    sk.subcont_pa.mbox := m;
    push_stack_fragment sk.subcont_ek DelimCCE
   with DelimCCE -> 
     let v = mbox_receive sk.subcont_pb in
         p''.mbox := fun () -> v)

let push_delim_subcont (sk : ('a,'b) subcont) (m : unit -> 'a) : 'b =
      push_prompt sk.subcont_pb (fun () -> push_subcont sk m)


(* ------- End expository version. The following is a bit optimized
           code after some inlining.
 *)
*)


(* The reason for take_subcont_aux and push_subcont_aux is to force
   ocamlopt to generate a frame table for the trap frame
   introduced by the try form.
*)

let take_subcont_aux (p : 'b prompt) (f : ('a,'b) subcont -> unit -> 'b) 
    (pa : 'a prompt) ek subcontchain  =
  let ekfrag = copy_stack_fragment ek in
  p.mbox := 
    f {subcont_ek = ekfrag; subcont_pa = pa;
       subcont_pb = p; subcont_ps = subcontchain; subcont_bs = ek};
  reset_ek ek DelimCCE		(* That will remove the prompt...*)


let take_subcont (p : 'b prompt) (f : ('a,'b) subcont -> unit -> 'b) : 'a =
  let (h,s,subcontchain) = unwind [] p.mark !ptop in
  let () = ptop := s in
  let pa = new_prompt () in
  try
    take_subcont_aux p f pa h.pfr_ek subcontchain (* does not return *)
  with 
  | DelimCCE -> mbox_receive pa
  | Out_of_memory -> dbg_fatal_error "take_subcont: out of memory"
  | e -> dbg_fatal_error "take_subcont: can't happen"


let push_subcont_aux (sk : ('a,'b) subcont) (m : unit -> 'a)  =
  let base = sk.subcont_bs in
  let ek = get_ek () in
  List.iter (fun pframe ->
    ptop := {pframe with pfr_ek = rebase_ek pframe.pfr_ek base ek} ::
      !ptop)
    sk.subcont_ps;
  sk.subcont_pa.mbox := m;
  push_stack_fragment sk.subcont_ek DelimCCE  (* does not return *)

let push_subcont (sk : ('a,'b) subcont) (m : unit -> 'a) : 'b =
  try
    push_subcont_aux sk m 			(* does not return *)
  with 
  | DelimCCE -> mbox_receive sk.subcont_pb
  | e -> dbg_note "propagating exc1"; raise e


(* Another optimization: push the _delimited_ continuation.
   This is the optimization of the pattern
      push_prompt sk.subcont_pb (fun () -> push_subcont sk v)
   corresponding to pushing the continuation captured by shift/shift0. 
   The latter continuation always has the delimiter at the end.
   Indeed shift can be implemented more efficiently as a primitive
   rather than via push_prompt/control combination...
*)

let push_delim_subcont_aux (sk : ('a,'b) subcont) (m : unit -> 'a) : 'any =
  let pb = sk.subcont_pb in
  let base = sk.subcont_bs in
  let ek = get_ek () in
  ptop := {pfr_mark = pb.mark; pfr_ek = ek} :: (!ptop);
  List.iter (fun pframe ->
    ptop := {pframe with pfr_ek = rebase_ek pframe.pfr_ek base ek} ::
         !ptop)
      sk.subcont_ps;
  sk.subcont_pa.mbox := m;
  push_stack_fragment sk.subcont_ek DelimCCE (* does not return *)

let push_delim_subcont (sk : ('a,'b) subcont) (m : unit -> 'a) : 'b =
  try
    push_delim_subcont_aux sk m
  with
  | DelimCCE -> (match !ptop with
    | h::t -> assert (h.pfr_mark == sk.subcont_pb.mark); ptop := t; 
	mbox_receive sk.subcont_pb
    | _ -> dbg_fatal_error "push_delim_subcont: empty pstack on DelimCCE")
  | e -> match !ptop with
    | h::t -> assert (h.pfr_mark == sk.subcont_pb.mark); ptop := t; 
	dbg_note "propagating exc2"; raise e
    | _ -> dbg_fatal_error "push_delim_subcont: empty pstack on other exc"


(* A more efficient variation of take_subcont, which does not capture
   any continuation.
   This code makes it clear that abort is essentially raise.
*)
let abort (p : 'b prompt) (x : 'b) : 'a =
  let (h,s) = unwind_abort p.mark !ptop in
  ptop := s;
  p.mbox := (fun () -> x);
  reset_ek h.pfr_ek DelimCCE


(* Check to see if a prompt is set *)
let is_prompt_set p = 
  let rec loop = function 
    | []   -> false
    | h::t -> h.pfr_mark == p.mark || loop t
  in loop !ptop

(* Common delimited control operators *)
let control p f = take_subcont p (fun sk () ->
  push_prompt p (fun () -> (f (fun c -> push_subcont sk (fun () -> c)))))

let shift p f = take_subcont p (fun sk () ->
  push_prompt p (fun () -> (f (fun c -> 
    push_delim_subcont sk (fun () -> c)))))

let shift0 p f = take_subcont p (fun sk () ->
  f (fun c -> push_delim_subcont sk (fun () -> c)))


(*------------------------------- Debugging and information *)


(* Print out various internal information, for debugging purposes *)
let debug_status title =
   let print_pfr pframe = dbg_print_ek_simple " " pframe.pfr_ek in
   begin
   prerr_endline "\n*** Delimcc status and statistics";
   prerr_endline title;
   prerr_endline "The parallel stack";
   List.iter (fun pframe -> print_pfr pframe) !ptop;
   prerr_newline ();
   dbg_print_ek (get_ek ());
   prerr_newline ();
end


(* Describe an OCaml value v up to the specified nesting depth max_depth,
   writing the result to stderr.
   We print just about everything we can glean from the run-time
   representation of the value and its components, if any.
*)

external describe_value : Obj.t -> bool = "describe_value" (* Helper *)

let show_val max_depth (v : Obj.t) =
  let rec show indent v =
    prerr_string indent; flush stderr;
    if String.length indent > 2*max_depth then prerr_string "...\n" else
    let traversible = describe_value v in
    if traversible then
      for i=0 to Obj.size v - 1 do
	show (indent ^ "  ") (Obj.field v i)
    done
  in show "" v


(*------------------------------- Serialization *)

(* Relativitizing captured continuation relative to global data *)
(* The real work is done by the function relativitize, which
   is written in a restricted C so it does not use any memory allocation
   operation and does not trigger GC. Thus atomicity of relativitization
   is assured. Since relativitize cannot allocate memory, we have
   to pre-allocate everything, including all of the used global_data_idx
   data
*)


(* Global index *)

type global_data_idx			(* custom *)

external global_data_ix_make : int -> global_data_idx =
  "global_data_ix_make"

external global_data_register_custom_ops : unit -> unit  =
  "global_data_register_custom_ops"

let () = global_data_register_custom_ops ()

(* preallocate the array and the global_data_idx custom data *)
let global_data_empty_ref = Obj.repr 0
let global_data : (Obj.t * global_data_idx) array = 
  Array.init 40 
    (fun i -> (global_data_empty_ref, global_data_ix_make i))

let register_global_closure (clo : 'a -> 'b) : unit =
 let v = Obj.repr clo in
 let rec loop arr i =
   if i >= Array.length arr then failwith "No memory" else
   if fst (arr.(i)) == v then () else
   if fst (arr.(i)) == global_data_empty_ref then
     arr.(i) <- (v,snd arr.(i))
   else loop arr (succ i)
 in loop global_data 0


external relativitize_h : 
    (Obj.t * global_data_idx) array -> bool -> Obj.t -> Obj.t
 = "relativitize"

let relativitize v flag = relativitize_h global_data flag v

external absolutize_h : 
    (Obj.t * global_data_idx) array -> Obj.t -> Obj.t
 = "absolutize"

let absolutize v = absolutize_h global_data v

(*
let () =  
  Gc.minor ();			(* oldify the global data up to this point*)
  let p0 = new_prompt () in
  let _ = Obj.repr
      (push_prompt p0 (fun () -> shift p0 (fun f -> 
	relativitize (Obj.repr f) true)))
  in prerr_endline "Serialization initialized"
*)
    (* Essentially, make sure all of the functions in here are considered
       `global', so they won't ever get serialized *)
let init_global_closure reference_data = 
  Gc.minor ();			(* oldify the global data up to this point*)
  ignore( relativitize (Obj.repr ptop) true ); (***)
  ignore( relativitize (Obj.repr mbox_receive) true );
  ignore( relativitize (Obj.repr push_prompt_aux) true );
  ignore( relativitize (Obj.repr push_prompt) true );
  ignore( relativitize (Obj.repr take_subcont) true );
  ignore( relativitize (Obj.repr push_subcont) true );
  ignore( relativitize (Obj.repr push_delim_subcont_aux) true );
  ignore( relativitize (Obj.repr push_delim_subcont) true );
  ignore( relativitize (Obj.repr shift) true );
  ignore( relativitize (Obj.repr abort) true );
  ignore( relativitize (Obj.repr relativitize) true );
  ignore( relativitize (Obj.repr absolutize) true );
  ignore( relativitize (Obj.repr reference_data) true );
  (*
  let v = (Obj.repr push_prompt) in
   for i = 1 to Obj.size v do
     ignore( relativitize (Obj.field v i) true );
  done;
  *)
  let p0 = new_prompt () in
  let _ = push_prompt p0 (fun () -> 
            shift p0 (fun f -> 
	      let _ = relativitize (Obj.repr f) true in
	      f ());
            shift p0 (fun f -> relativitize (Obj.repr f) true); 
            abort p0 (Obj.repr 0)) in
  prerr_endline "Serialization initialized"

(*
*)


external output_delim_value_h :
    (Obj.t * global_data_idx) array -> out_channel -> Obj.t -> 
     Marshal.extern_flags list -> unit =
 "output_delim_value"

let output_delim_value chan v =
  output_delim_value_h global_data chan (Obj.repr v) [Marshal.Closures]

(*
let init_global_closure () = 
  prerr_endline "hole";
  show_val 2 (Obj.repr new_hole);
  show_val 2 (Obj.repr apply_hole);
  show_val 2 (Obj.repr get_ek);
  prerr_endline "pstack";
  prerr_endline "prompt";
  show_val 2 (Obj.repr prompt_push);
  prerr_endline "EK";
  show_val 4 (Obj.repr dbg_fatal_error);
  show_val 4 (Obj.repr copy_stack_fragment);
  register_global_closure take_subcont;
  register_global_closure push_prompt;
*)


(* Some old code, saved for the sake of reference

external set_global_data : (Obj.t * Obj.t) array -> unit =
  "set_global_data"

external get_global_fn : int -> Obj.t = 
  "get_global_fn"

let () = 
  let global_data =
    Array.init 20 
      (fun i -> (global_data_empty_ref,  
		 Obj.repr (fun x -> Obj.obj (get_global_fn i) x)))
  in set_global_data global_data;
  show_val 7 (Obj.repr global_data)
*)
