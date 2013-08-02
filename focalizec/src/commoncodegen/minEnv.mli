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

(* $Id: minEnv.mli,v 1.11 2012-03-01 17:23:32 pessaux Exp $ *)

type rec_proof_kind =
  | RPK_struct of Parsetree.vname
  | RPK_other

type rec_status =
  | RC_non_rec
  | RC_rec of rec_proof_kind

type min_coq_env_element =
    MCEE_Declared_carrier
  | MCEE_Defined_carrier of Types.type_scheme
  | MCEE_Declared_computational of (Parsetree.vname * Types.type_scheme)
  | MCEE_Defined_computational of
      (Env.from_history * rec_status * Parsetree.vname *
       (Parsetree.vname list) * Types.type_scheme * Parsetree.binding_body)
  | MCEE_Declared_logical of (Parsetree.vname * Parsetree.logical_expr)
  | MCEE_Defined_logical of
      (Env.from_history * Parsetree.vname * Parsetree.logical_expr)

val find_coq_env_element_by_name :
  Parsetree.vname -> min_coq_env_element list -> min_coq_env_element

val minimal_typing_environment :
  VisUniverse.in_the_universe_because VisUniverse.Universe.t ->
  Env.TypeInformation.species_field list -> min_coq_env_element list
