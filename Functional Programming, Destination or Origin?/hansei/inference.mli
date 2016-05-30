(* The library to support a probabilistic embedded domain specific language *)
(*            Probabilistic inference procedures                            *)
(* These procedures implement different exploration strategies over a
   lazy search tree, transforming a search tree into a flatter form         *)

open Ptypes

			(* Explore and flatten the tree: 
			   perform exact inference to the given depth *)
val explore :
    int option ->			(* max depth; if None, no limit *)
    'a pV ->				(* Input lazy search tree  *)
    'a pV				(* Output lazy search tree *)

			(* Partially explore but do not flatten the tree: *)
	                (* Pre-computing choices as an optimization*)
val shallow_explore :
    int ->				(* max depth *)
    'a pV ->				(* Input lazy search tree  *)
    'a pV				(* Output lazy search tree *)

			(* Explore the tree till we find the first success -- 
	                   the first leaf   (V v) -- and return the resulting
			   tree. Return the empty tree otherwise. *)
val first_success: 
    'a pV -> 'a pV

			(* Compute the bounds on the probability of *)
			(* evidence                                 *)
val bounded_explore :
    int ->                              (* max size of the queue   *)
    unit pV ->				(* Input lazy search tree  *)
    prob * prob				(* Probability bounds      *)

			(* Given a sampler, a function 'seed->'seed, 
			   run it a certain number of times and return the
                           resulting seed and the number of runs *)
type sample_runner = 
    {sample_runner : 'seed. 'seed -> ('seed -> 'seed) -> 'seed * int}

			(* Approximate inference: sampling *)
val rejection_sample_dist :
    'a vc selector ->			(* selector among the branches *)
    int ->				(* Number of iterations    *)
    'a pV ->				(* Input lazy search tree  *)
    'a pV				(* Output lazy search tree *)

val sample_dist :			(* Explore with look-ahead sampling *)
    'a pV selector ->			(* selector among the branches *)
    sample_runner -> 		        (* Do a few samples *)
    'a pV -> 				(* Input lazy search tree  *)
    'a pV				(* Output lazy search tree *)


(*			Useful utility functions			*)


	(* Compute the statistics of sampling, over various seeds *)
val statistics : 
    int * int ->			(* The range of random seed values *)
    (int -> 'a pV) ->			(* Sampling procedure, takes seed  *)
    ('a * float * float) list		(* Value, mean, standard deviation *)


val timeit : (unit -> 'a) -> 'a		(* Time the execution *)

val normalize :				(* Normalize the distribution *)
    'a cdist ->				(* Input distribution         *)
     prob *				(* Normalization constant, total prob *)
      'a dist				(* Normalized distribution    *)
