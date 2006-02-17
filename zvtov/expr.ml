(*  Copyright 2006 INRIA  *)
(*  $Id: expr.ml,v 1.1 2006-02-17 15:36:33 lk Exp $  *)

type expr =
  | Evar of string 
  | Eapp of string * expr list 

  | Enot of expr 
  | Eand of expr * expr 
  | Eor of expr * expr 
  | Eimply of expr * expr 
  | Eequiv of expr * expr 
  | Etrue
  | Efalse

  | Eall of string list * expr 
  | Eex of string list * expr 


type phrase = 
  |Hyp of expr
  |Def of string * string list * expr

type but =
  |But of phrase * expr  
;;

