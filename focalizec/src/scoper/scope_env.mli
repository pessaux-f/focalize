(* $Id: scope_env.mli,v 1.1 2007-07-27 13:54:19 pessaux Exp $ *) 
type t

type scope_binding_info =
  | SBI_file of Parsetree.fname  (* The ident is at toplevel in a file. *)
  | SBI_method_of_self        (* The ident is a method implicitely of self. *)
  | SBI_local         (* The ident is a locally bound indentifier (let or function parameter. *)

val empty : unit -> t

val add_value : Parsetree.vname -> scope_binding_info -> t -> t
val find_constructor : 'a -> 'b -> 'c
