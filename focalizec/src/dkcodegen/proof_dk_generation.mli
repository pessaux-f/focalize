(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            Raphaël Cauderlier                                              *)
(*                                                                            *)
(*               LIP6  --  INRIA Rocquencourt -- ENSTA ParisTech              *)
(*                                                                            *)
(*  Copyright 2007 - ... LIP6 and INRIA                                       *)
(*            2012 - ... ENSTA ParisTech                                      *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)


exception Attempt_proof_by_def_of_species_param of
  (Location.t * Parsetree.expr_ident)

exception Attempt_proof_by_def_of_declared_method_of_self of
  (Location.t * Parsetree.expr_ident)

exception Attempt_proof_by_def_of_local_ident of
  (Location.t * Parsetree.expr_ident)

exception Attempt_proof_by_prop_of_local_ident of
  (Location.t * Parsetree.expr_ident)

exception Attempt_proof_by_unknown_hypothesis of
  (Location.t * Parsetree.vname)

exception Attempt_proof_by_unknown_step of
  (Location.t * Parsetree.node_label)

val generate_defined_method_proto_postlude :
  Context.species_compil_context ->
  Dk_pprint.dk_print_context ->
  Env.DkGenEnv.t ->
  self_manifest:Types.type_simple option ->
  Parsetree.vname list ->
  Types.type_scheme -> Parsetree.binding_body option -> unit

val generate_theorem_body :
  Context.species_compil_context ->
  Dk_pprint.dk_print_context ->
  Env.DkGenEnv.t ->
  Env.TypeInformation.min_dk_env_element list ->
  self_manifest:Types.type_simple option ->
  Parsetree.vname list ->
  (Env.TypeInformation.species_param *
     Env.ordered_methods_from_params)
    list ->
  Misc_common.compiled_species_fields list ->
  Parsetree.vname ->
  Parsetree.logical_expr -> Parsetree.proof -> unit

val toplevel_theorem_compile :
  Context.species_compil_context -> Env.DkGenEnv.t ->
    Parsetree.theorem_def -> unit
