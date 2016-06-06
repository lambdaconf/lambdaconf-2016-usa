(*	HANSEI: probabilistic embedded domain specific language

  This library lets the user define first-order and nested
  probabilistic computations and perform exact and approximate inference 
  (rejection and importance sampling).
  The library expresses probabilistic computations in direct style,
  so that deterministic computations `run at full speed', and
  lets the programmer reify and reflect probabilistic `effects'.
*)

open Ptypes
open Inference

(* Two basic library functions for probabilistic programming *)
val dist : 'a dist -> 'a
val fail : unit -> 'a

(* The basic reification function, converting a probabilistic
   computation to a lazy search tree *)
val reify0 : (unit -> 'a) -> 'a pV

(* Derived functions to express various discrete distributions *)
val flip : prob -> bool		(* Binary boolean choice with probability p *)

val uniform : int -> int	(* Uniform choice from [0..(n-1)] *)

val uniformly : 'a array	(* Uniform choice from an array *)
    -> 'a

				(* Uniform choice of an integer from 
				   [low..high] *)
val uniform_range :
   int ->				(* lower boundary           *)
   int ->				(* upper boundary           *)
   int

val geometric : 		(* Geometric distribution with prob p: *)
    prob -> int                 (* 0 with the prob p,               *)
	                        (* 1 with the prob (1-p)*p, etc.    *)

val geometric_bounded :		(* Bounded geometric distribution   *)
    int ->			(* The upper bound		    *)
    prob ->			(* prob parameter p                 *)
    int


(* Other convenient derived functions *)
val reflect : 'a pV -> 'a	(* reflect reified choices,         *)
				(* the `inverse' of reify0          *)

				(* Assert the present evidence *)
val observe : ('a -> bool) -> (unit -> 'a) -> 'a 

				(* Variable elimination optimization *)
val variable_elim :		(* a transformation on stochastic function *)
    ('a -> 'b) ->
    ('a -> 'b)

(* Selectors, used by approximate inference procedures *)
val random_selector :			(* Use the random number generator *)
    int -> 				(* Random seed *)
    'a selector
val dist_selector : 'a selector		(* Use our non-determinism *)


(*    End-user inference procedure: compute the probability distribution
      for a given non-deterministic program *)

				(* Perform the exact inference *)
val exact_reify : (unit -> 'a) -> 'a pV

				(* Naive rejection sampling *)
val sample_rejection :
    'a vc selector ->			(* selector procedure *)
    int ->				(* number of samples *)
    (unit -> 'a) ->			(* computation *)
    'a pV

				(* Importance sampling *)
val sample_importance : 
    'a pV selector ->			(* selector procedure *)
     int ->				(* number of samples *)
     (unit -> 'a) ->			(* computation *)
     'a pV


	(* call-by-need *)
val letlazy : 
    (unit -> 'a) ->			(* binding expression *)
    (unit -> 'a)			(* lazy variable *)

val letlazy_nesting : 			(* Safe to use even for nested infer *)
    (unit -> 'a) ->			(* binding expression *)
    (unit -> 'a)			(* lazy variable *)

	(* memoization: a general case of letlazy *)
val memo : 
    ('a -> 'b) ->			(* source function   *)
    ('a -> 'b)				(* memoized function *)

