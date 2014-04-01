(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                                                                     *)
(*                 LIP6  --  INRIA Rocquencourt  -- ENSTA              *)
(*                                                                     *)
(*  Copyright 2007 - 2012 LIP6 and INRIA                               *)
(*            2012 ENSTA ParisTech                                     *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

type min_coq_env_method =
    MCEM_Declared_carrier
  | MCEM_Defined_carrier of Types.type_scheme
  | MCEM_Declared_computational of (Parsetree.vname * Types.type_scheme)
  | MCEM_Defined_computational of
      (Env.from_history * Env.CoqGenInformation.rec_status * Parsetree.vname *
       (Parsetree.vname list) * Types.type_scheme * Parsetree.binding_body)
  | MCEM_Declared_logical of (Parsetree.vname * Parsetree.logical_expr)
  | MCEM_Defined_logical of
      (Env.from_history * Parsetree.vname * Parsetree.logical_expr)

type min_coq_env_reason =
  | MCER_only_logical
  | MCER_even_comput

type min_coq_env_element = (min_coq_env_reason * min_coq_env_method)

val find_coq_env_element_by_name :
  Parsetree.vname -> min_coq_env_element list -> min_coq_env_element

val minimal_typing_environment :
  VisUniverse.in_the_universe_because VisUniverse.Universe.t ->
  Env.TypeInformation.species_field list -> min_coq_env_element list
