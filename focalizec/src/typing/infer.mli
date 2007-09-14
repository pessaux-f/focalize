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


(* $Id: infer.mli,v 1.11 2007-09-14 14:32:32 pessaux Exp $ *)

exception Method_multiply_defined of (Parsetree.vname * Types.species_name)
exception Unbound_type_variable of string
exception Bad_sum_type_constructor_arity of
  (Parsetree.ident * Env.TypeInformation.constructor_arity)
exception Bad_type_arity of (Parsetree.ident * int * int)
exception Rep_multiply_defined of Location.t
exception Parameterized_species_arity_mismatch of string

exception Not_subspecies_conflicting_field of
  (Types.type_collection * Types.type_collection * Parsetree.vname *
   Types.type_simple * Types.type_simple * Location.t)
exception Not_subspecies_circular_field of
  (Types.type_collection * Types.type_collection * Parsetree.vname *
   Types.type_simple * Types.type_simple * Location.t)
exception Not_subspecies_arity_mismatch of
  (Types.type_collection * Types.type_collection * Parsetree.vname *
   Types.type_name * int * int * Location.t)
exception Not_subspecies_missing_field of
  (Types.type_collection * Types.type_collection * Parsetree.vname *
   Location.t)
exception Collection_not_fully_defined of (Types.species_name * Parsetree.vname)

type please_compile_me =
  | PCM_no_matter
  | PCM_external
  | PCM_species of
      (Parsetree.species_def * Env.TypeInformation.species_description *
       (Dep_analysis.name_node list))
  | PCM_collection of
      (Parsetree.coll_def * Env.TypeInformation.species_description)
  | PCM_type
  | PCM_let_def of Parsetree.let_def
  | PCM_theorem of Parsetree.theorem_def
  | PCM_expr of Parsetree.expr

val typecheck_file :
  current_unit: Types.fname -> Parsetree.file ->
    (Env.TypingEnv.t * (please_compile_me list))
