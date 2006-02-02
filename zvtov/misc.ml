(*  Copyright 2006 INRIA  *)
(*  $Id: misc.ml,v 1.1 2006-02-02 13:30:03 doligez Exp $  *)

exception Error of string;;
let error msg = raise (Error msg);;
