(* The library to support a probabilistic embedded domain specific language *)
(* Type definitions *)

type prob = float			(* Probability data type *)

type 'a dist  = (prob * 'a) list	(* Discrete probability  *)
type 'a cdist = (prob * 'a) list	(* Discrete conditional probability,
					   probs not necessarily sum to 1 *)

(* Representations of (reified) probabilistic computations and values:
   The lazy search tree *)

type 'a vc = V of 'a 			       (* leaf *)
           | C of (unit -> 'a pV)              (* unexpanded branch *)
and  'a pV = (prob * 'a vc) list	(* computation *)
;;

(* Selecting from a conditional distribution, returning the selected
   sample and the cumulative probability mass. *)
type 'a selector = 'a cdist -> (prob * 'a)
