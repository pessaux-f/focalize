(*  Copyright 2004 INRIA  *)
(*  $Id: cache.mli,v 1.4 2007-07-25 19:41:39 doligez Exp $  *)

type reference;;

val init : string -> string -> string -> unit;;
(* [init base version1 version2]
   [base] is the name of the current .zv file
   [version1] is the version string of zvtov
   [version2] is the version string of zenon
*)

val close : unit -> unit;;
(* save the newly added proofs to the cache *)

val add : string -> string -> string -> unit;;
(* [add key prooffile errfile]
   [key] is the statement
   [prooffile] is the name of the proof file
   [errfile] is the name of the errors/warnings file
*)

val find : string -> string -> string -> bool;;
(* [find key destfile desterr]
   [key] is the statement
   [destfile] is the file where the proof will be put
   [desterr] is the file where the errors/warnings will be put
   the result is true if the proof was found and copied to destfile
*)
