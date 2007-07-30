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

(* $Id: env.mli,v 1.6 2007-07-30 08:07:44 weis Exp $ *)

exception Unbound_constructor of Parsetree.vname
;;
exception Invalid_constructor_identifier of Parsetree.ident
;;
exception Unbound_label of Types.label_name
;;
exception Unbound_identifier of Parsetree.vname
;;
exception Unbound_type of Types.type_name
;;
exception Unbound_module of Parsetree.fname
;;


type species_param =
  | SPAR_in of Parsetree.vname * Types.type_simple
  | SPAR_is of Parsetree.vname * Types.type_simple

type species_description = {
  spe_sig_params : species_param list;
  spe_sig_inher : Types.type_species list;
  spe_sig_methods : (string * Types.type_simple * (Parsetree.expr option)) list;
}

type collections_sig = (string * Types.type_simple) list
;;
type constructor_arity =
  | CA_zero | CA_one
;;
type field_mutability =
  | FM_mutable | FM_immutable
;;

type constructor_description = {
  cstr_arity : constructor_arity;
  cstr_scheme : Types.type_scheme;
}
;;

type label_description = {
  field_mut : field_mutability;
  field_scheme : Types.type_scheme;
}
;;

type type_kind =
  | TK_abstract
  | TK_variant of (Parsetree.constr_name * Types.type_scheme) list
  | TK_record of
      (Types.label_name * field_mutability * Types.type_scheme) list
;;

type type_description = {
  type_kind : type_kind ;
  type_identity : Types.type_scheme ;
  type_arity : int;
}
;;

type t
;;

val empty : unit -> t
;;
val pervasives : unit -> t
;;

val add_constructor :
  Parsetree.constr_name -> constructor_description -> t -> t
;;
val find_constructor : Parsetree.ident -> t -> constructor_description
;;

val add_label : Types.label_name -> label_description -> t -> t
;;
val find_label : Types.label_name -> t -> label_description
;;

val add_value : Parsetree.vname -> Types.type_scheme -> t -> t
;;
val find_value : Parsetree.ident -> t -> Types.type_scheme
;;

val add_type : Types.type_name -> type_description -> t -> t
;;
val find_type :  Parsetree.ident -> t -> type_description
;;
