(*  Copyright 2004 INRIA  *)
(*  $Id: cache.mli,v 1.1 2004-10-11 16:07:39 doligez Exp $  *)

type reference;;

val active : bool ref;;

val init : string -> unit;;
(* [init base]
   [base] is the name of the current .zv file
*)

val close : unit -> unit;;
(* save the newly added proofs to the cache *)

val add : string -> string -> unit;;
(* [add key filename]
   [key] is the statement
   [filename] is the name of the proof file
*)

val find : string -> string -> bool;;
(* [find key destfile]
   [key] is the statement
   [destfile] is the file where the data will be put
   the result is true if the proof was found and copied to destfile
*)
