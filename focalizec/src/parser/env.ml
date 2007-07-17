(* $Id: env.ml,v 1.5 2007-07-17 15:44:27 pessaux Exp $ *)

(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)


exception Unbound_constructor of Parsetree.vname ;;
exception Invalid_constructor_identifier of Parsetree.ident ;;
exception Unbound_label of Types.label_name ;;
exception Unbound_module of Parsetree.fname ;;
exception Unbound_identifier of Parsetree.vname ;;
exception Unbound_type of Types.tname ;;


type species_param =
  | SPAR_in of (Parsetree.vname * Types.simple_type)    (* Entity param. *)
  | SPAR_is of (Parsetree.vname * Types.simple_type)    (* Collection param. *)
;;

type species_description = {
  spe_sig_params : species_param list ;
  spe_sig_inher : Types.species_type list ;
  spe_sig_methods :  (** Method's name, type and body if defined. *)
      (string * Types.simple_type * (Parsetree.expr option)) list
} ;;


type collections_sig = (string * Types.simple_type) list ;; (* To refine. *)

type constructor_arity = CA_zero | CA_one ;;


type constructor_description = {
  (** Arity : 0 or 1 (many = 1 type tuple), (1 = type, not a 1 tuple). *)
  cstr_arity : constructor_arity ;
  (** Full type scheme for this constructor, i.e (args ->) ty result. *)
  cstr_scheme : Types.types_scheme ;
} ;;


type field_mutability = FM_mutable | FM_immutable ;;


type label_description = {
  field_mut : field_mutability ;    (** Mutability for this field. *)
  (** Full type scheme for this field, i.e arg -> ty result. *)
  field_scheme : Types.types_scheme
  } 
;;


type type_kind =
  | TK_abstract  (** Abstract types and type abbreviations. *)
  | TK_variant of    (** Sum types. *)
      (Parsetree.constr_name * Types.types_scheme) list
  | TK_record of  (** Record types: list of labels. *)
      (Types.label_name * field_mutability * Types.types_scheme) list
;;


type type_description = {
  type_kind : type_kind ;             (** Kind of the type definition. *)
  (** The type scheme representing to what this type is equal to. For
      instance in type 'a t = 'a list, t is TK_abstract with [type_identity]
      representing 'a list.
      If the type is a pure abstract like in type t, then t is TK_abstract
      with [type_identity] representing the type ST_construct ("t", []). *)
  type_identity : Types.types_scheme ;
  type_arity : int          (** Number of parameters of the type. *)
} ;;



type t = {
  constructors : (Parsetree.constr_name * constructor_description) list ;
  labels : (Types.label_name * label_description) list ;
  types : (Types.tname * type_description) list ;
  (** [idents] Contains functions methods and more generally any let-bound
      identifiers. *)
  idents : (Parsetree.vname * Types.types_scheme) list ;
  species : (Types.sname * species_description) list ;
  collections : (Types.cname * collections_sig) list
} ;;



(* ***************************************************************** *)
(* Parsetree.fname option -> 'a -> 'a                                *)
(** {b Descr} : Wrapper to lookup inside an external interface file.

    {b Rem} : Not exported outside this module.                      *)
(* ***************************************************************** *)
let find_module =
  let buffered = ref [] in
  (fun fname_opt env ->
    match fname_opt with
     | None -> env
     | Some fname ->
	 (begin
	 try List.assoc fname !buffered
	 with Not_found ->
	   (* The interface was not already loaded... *)
	   let fo_name = Files.fo_filename_from_module_name fname in
	   try
	     (* Try to open the interface file. *)
	     let in_file = Files.open_in_from_lib_paths fo_name in
	     (* Just ensure it's really an interface file. *)
	     if Files.check_magic in_file Files.fo_magic then
	       (begin
	       let file_envt = input_value in_file in
	       close_in in_file ;
	       (* If the interface was found, buferize it for further uses. *)
	       buffered := (fname, file_envt) :: !buffered ;
	       file_envt
	       end)
	     else
	       (begin
	       close_in in_file ;
	       raise (Files.Corrupted_fo fname)
	       end)
	   with Files.Cant_access_file _ -> raise (Unbound_module fname)
	 end))
;;



let rec find_constructor cstr_ident env =
  match cstr_ident.Parsetree.ast_desc with
   | Parsetree.I_local vname -> find_constructor_vname vname env
   | Parsetree.I_global (opt_scope, vname) ->
       let env' = find_module opt_scope env in
       find_constructor_vname vname env'
   | Parsetree.I_method (_, _) ->
       (* Don't know what it means if the           *)
       (* constructor seems to be in fact a method. *)
       raise (Invalid_constructor_identifier cstr_ident)

and find_constructor_vname vname env =
  try List.assoc vname env.constructors with
  | Not_found -> raise (Unbound_constructor vname)
;;


let find_label lbl_name env =
  try List.assoc lbl_name env.labels with
  | Not_found -> raise (Unbound_label lbl_name)
;;


let add_ident ident ty_scheme env =
  { env with idents = (ident, ty_scheme) :: env.idents }
;;


let rec find_ident ident_ident env =
  match ident_ident.Parsetree.ast_desc with
   | Parsetree.I_local vname -> find_ident_vname vname env
   | Parsetree.I_global (opt_scope, vname) ->
       let env' = find_module opt_scope env in
       find_ident_vname vname env'
   | Parsetree.I_method (_, _) -> failwith "todo"

and find_ident_vname vname env =
  try List.assoc vname env.idents with
  | Not_found -> raise (Unbound_identifier vname)
;;



let rec find_type type_ident env =
  match type_ident.Parsetree.ast_desc with
   | Parsetree.I_local vname -> find_type_vname vname env
   | Parsetree.I_global (opt_scope, vname) ->
       let env' = find_module opt_scope env in
       find_type_vname vname env'
   | Parsetree.I_method (_, _) ->
       (* Type identifiers should never be methods ! *)
       assert false

and find_type_vname vname env =
  let tname = Parsetree_utils.string_of_vname vname in
  try List.assoc tname env.types with
  | Not_found -> raise (Unbound_type tname)
;;
