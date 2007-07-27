(* $Id: scope_env.ml,v 1.1 2007-07-27 13:54:19 pessaux Exp $ *)

(* ********************************************************************* *)
(* type scope_binding_info                                               *)
(** {b Descr} : Tag each binding in the scopping environment in order to
            know if the [ident] is currently bound to a global toplevel
            definition inside a file or if it's a method found in the
            current species inheritance tree (including itself).
            Note that there is no need to add another case like a
            'SBI_method_of_specie' because if a method has to be called
            from a particular species other than [Self], then it has to
            be syntactically explicitely written !

    {b Rem} : Not exported outside this module.                          *)
(* ********************************************************************* *)
type scope_binding_info =
  | SBI_file of Parsetree.fname  (* The ident is at toplevel in a file. *)
  | SBI_method_of_self        (* The ident is a method implicitely of self. *)
  | SBI_local         (* The ident is a locally bound indentifier (let or function parameter. *)
;;


type t = {
  constructors : (Parsetree.constr_name * Parsetree.fname) list ;
  (* types : (Types.tname * Parsetree.fname) list ; *)
  values : (Parsetree.vname * scope_binding_info) list
} ;;



let empty () =
  { constructors = [] ; (* types  = [] ;*) values = [] }
;;


let add_value ident scope_binding_info env =
  { env with values = (ident, scope_binding_info) :: env.values }
;;


let find_constructor _ _ = assert false ;;
