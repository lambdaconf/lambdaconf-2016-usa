(* Eratosthenes sieve: an example of a concurrent program
   where the concurrency primitives are implemented with delimcc.

   The example is an elaboration of the code kindly sent by
   Christophe Deleuze, July 18, 2012.
*)

open Lwc

(* Send a stream of integers [m..n] on the channel out *)
(* It is a task and hence creates a thunk *)
let iota : int mvar -> int -> int -> task = fun out m n () ->
  for i = m to n do
    put_mvar out i
  done
;;

(* A task to print the values read from the stream *)
let output : int mvar -> task = fun inp () ->
  while true do
    let v = take_mvar inp in
    Printf.printf "%i " v
  done
;;

(* The key step in the Eratosthenes sieve: copy inp to out but replace
   every n-th element with 0
*)
let filter : int -> int mvar -> int mvar -> task = fun n inp out () ->
  let rec loop i =
    let v = take_mvar inp in
    if i <= 1 then 
      (put_mvar out 0; loop n)
    else 
      (put_mvar out v; loop (i-1))
  in loop n
;;

(* The main sieving task: move prime numbers from inp to out 
   by composing filters *)
let rec sift : int mvar -> int mvar -> task = fun inp out () ->
  let n = take_mvar inp in
  if n = 0 then sift inp out ()
  else begin
    put_mvar out n;
    let mid = make_mvar () in
    spawn (filter n inp mid);
    sift mid out ()
  end
;;

(* Start up the task of the sieving job, with n being the upper limit *)
let sieve : int -> task = fun n () ->
  let mi = make_mvar () in
  let mo = make_mvar () in
  spawn (iota mi 2 n);
  spawn (sift mi mo);
  spawn (output mo)
;;

(* Print the statistics of executing a thunk *)
let perf : (unit -> 'a) -> 'a = fun th ->
  let start_time = Unix.gettimeofday () in
  let r = th () in
  let wall_clock = Unix.gettimeofday () -. start_time in
  let { Unix.tms_utime  = user; Unix.tms_stime  = sys;
	Unix.tms_cutime = _;    Unix.tms_cstime = _   } = Unix.times ()
  in
  Gc.full_major ();
  let gc = Gc.quick_stat () in
  Printf.printf 
    "\nuser %f sys %f real %f top %i mw %.0f pw %.0f Mw %.0f mc %i Mc %i lw %i fw %i\n" 
    user sys wall_clock
    gc.Gc.top_heap_words gc.Gc.minor_words gc.Gc.promoted_words
    gc.Gc.major_words gc.Gc.minor_collections gc.Gc.major_collections
    gc.Gc.live_words gc.Gc.free_words;
 r
;;

(* spawn (sieve 10);;
*)
spawn (sieve 5003);
perf start;;


(*
ASUS EePC 701
bytecode:

./sieve
2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 
...
4937 4943 4951 4957 4967 4969 4973 4987 4993 4999 5003 

user 15.896993 sys 0.008000 real 16.081793 top 253952 mw 79419389 pw 61519780 Mw 61544950 mc 2423 Mc 1087 lw 0 fw 0

native
user 7.356459 sys 0.004000 real 7.457425 top 126976 mw 61870851 pw 24580929 Mw 27298298 mc 1888 Mc 730 lw 0 fw 0

*)
