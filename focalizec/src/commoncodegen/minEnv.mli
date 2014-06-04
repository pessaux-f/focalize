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

type min_coq_env_element =
    MCEE_Declared_carrier
  | MCEE_Defined_carrier of Types.type_scheme
  | MCEE_Declared_computational of (Parsetree.vname * Types.type_scheme)
  | MCEE_Defined_computational of
      (Env.from_history * Env.CoqGenInformation.rec_status * Parsetree.vname *
       (Parsetree.vname list) * Types.type_scheme * Parsetree.binding_body)
  | MCEE_Declared_logical of (Parsetree.vname * Parsetree.logical_expr)
  | MCEE_Defined_logical of
      (Env.from_history * Parsetree.vname * Parsetree.logical_expr)

val find_coq_env_element_by_name :
  Parsetree.vname -> min_coq_env_element list -> min_coq_env_element

val minimal_coq_typing_environment :
  VisUniverse.in_the_universe_because VisUniverse.Universe.t ->
  Env.TypeInformation.species_field list -> min_coq_env_element list

  
type min_dk_env_element =
    MDEE_Declared_carrier
  | MDEE_Defined_carrier of Types.type_scheme
  | MDEE_Declared_computational of (Parsetree.vname * Types.type_scheme)
  | MDEE_Defined_computational of
      (Env.from_history * Env.DkGenInformation.rec_status * Parsetree.vname *
       (Parsetree.vname list) * Types.type_scheme * Parsetree.binding_body)
  | MDEE_Declared_logical of (Parsetree.vname * Parsetree.logical_expr)
  | MDEE_Defined_logical of
      (Env.from_history * Parsetree.vname * Parsetree.logical_expr)

val find_dk_env_element_by_name :
  Parsetree.vname -> min_dk_env_element list -> min_dk_env_element

val minimal_dk_typing_environment :
  VisUniverse.in_the_universe_because VisUniverse.Universe.t ->
  Env.TypeInformation.species_field list -> min_dk_env_element list
