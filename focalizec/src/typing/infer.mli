(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            Fran�ois Pessaux                                                *)
(*            Pierre Weis                                                     *)
(*            Damien Doligez                                                  *)
(*                                                                            *)
(*               LIP6  --  INRIA Rocquencourt -- ENSTA ParisTech              *)
(*                                                                            *)
(*  Copyright 2007 - 2012 LIP6 and INRIA                                      *)
(*            2012 ENSTA ParisTech                                            *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)

exception Wrong_type_by_inheritance of
  (Location.t * Parsetree.vname * Types.type_simple *
   Types.type_simple * Env.from_history * Env.from_history)
exception Logical_statements_mismatch of
  (Parsetree.vname * Parsetree.qualified_species *
   Location.t * Parsetree.qualified_species * Location.t)
exception Proof_of_multiply_defined of
  (Location.t * Parsetree.vname * Location.t)
exception Proof_of_unknown_property of
  (Location.t * Parsetree.qualified_species * Parsetree.vname)
exception Method_multiply_defined of
  (Parsetree.vname * Parsetree.qualified_species)
exception Unbound_type_variable of (Location.t * Parsetree.vname)
exception Bad_sum_type_constructor_arity of
  (Parsetree.constructor_ident * Env.TypeInformation.constructor_arity)
exception Bad_type_arity of (Location.t * Parsetree.ident * int * int)
exception Rep_multiply_defined of Location.t
exception Rep_multiply_defined_by_multiple_inheritance of
  (Types.type_simple * Types.type_simple * Location.t)
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
   Env.from_history * Location.t)

type non_defined_method =
  | NDMK_prototype of Parsetree.vname
  | NDMK_termination_proof of Parsetree.vname

exception Collection_not_fully_defined of
  (Parsetree.qualified_species * (non_defined_method list))
exception Scheme_contains_type_vars of
  (Parsetree.vname * Types.type_scheme * Location.t)
exception Invalid_parameter_in_delayed_proof_termination of
  (Location.t * Parsetree.vname)
exception No_mix_between_logical_defs of (Location.t * Parsetree.vname)
exception No_redef_final_let of (Location.t * Parsetree.vname)

type please_compile_me =
  | PCM_annotation_title
  | PCM_use of (Location.t * Parsetree.module_name)
  | PCM_open of (Location.t * Parsetree.module_name)
  | PCM_coq_require of Parsetree.module_name
  | PCM_species of
      (Parsetree.species_def * Env.TypeInformation.species_description *
       (DepGraphData.name_node list) *
       (Parsetree.vname * Env.TypeInformation.field_abstraction_info) list)
  | PCM_collection of
      (Parsetree.collection_def * Env.TypeInformation.species_description *
       (DepGraphData.name_node list))
  | PCM_testing of Parsetree.testing_def
  | PCM_type of (Parsetree.vname * Env.TypeInformation.type_description)
  | PCM_let_def of (Parsetree.let_def * (Types.type_scheme list))
  | PCM_theorem of
      (Parsetree.theorem_def * ((Parsetree.vname * Types.type_simple) list))
  | PCM_expr of Parsetree.expr

val typecheck_file :
  current_unit: Types.fname -> Parsetree.file ->
    (Env.TypingEnv.t * (please_compile_me list))
