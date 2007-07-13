(* $Id: env.mli,v 1.2 2007-07-13 15:16:38 pessaux Exp $ *)
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


exception Unbound_constructor of Parsetree.vname
exception Invalid_constructor_identifier of Parsetree.ident
exception Unbound_label of Types.label_name

type species_param =
  | SPAR_in of (Parsetree.vname * Types.simple_type)
  | SPAR_is of (Parsetree.vname * Types.simple_type)

type species_description = {
  spe_sig_params : species_param list ;
  spe_sig_inher : Types.species_type list ;
  spe_sig_methods : (string * Types.simple_type * (Parsetree.expr option)) list
}

type collections_sig = (string * Types.simple_type) list
type constructor_arity = CA_zero | CA_one
type field_mutability = FM_mutable | FM_immutable

type constructor_description = {
  cstr_arity : constructor_arity;
  cstr_scheme : Types.types_scheme;
}

type label_description = {
  field_mut : field_mutability;
  field_scheme : Types.types_scheme;
}

type type_kind =
    TK_abstract
  | TK_variant of (Parsetree.constr_name * Types.types_scheme) list
  | TK_record of
      (Types.label_name * field_mutability * Types.types_scheme) list

type type_description = {
  type_kind : type_kind;
  type_identity : Types.types_scheme option;
  type_arity : int;
}

type t

val find_constructor : Parsetree.ident -> t -> constructor_description
val find_label : Types.label_name -> t -> label_description
val add_ident : Parsetree.vname -> Types.types_scheme -> t -> t
val find_ident : Parsetree.ident -> t -> Types.types_scheme
