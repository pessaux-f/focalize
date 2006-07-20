(*  Copyright 2006 INRIA  *)
(*  $Id: misc.mli,v 1.2 2006-07-20 13:19:21 doligez Exp $  *)

exception Error of string;;
val error : string -> 'a;;
