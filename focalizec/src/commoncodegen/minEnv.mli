(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                                                                     *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007, 2008 LIP6 and INRIA                                *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: minEnv.mli,v 1.7 2009-02-10 14:51:27 pessaux Exp $ *)

type min_coq_env_element =
    MCEE_Declared_carrier
  | MCEE_Defined_carrier of Types.type_scheme
  | MCEE_Declared_computational of (Parsetree.vname * Types.type_scheme)
  | MCEE_Defined_computational of
      (Env.from_history * bool * Parsetree.vname * (Parsetree.vname list) *
       Types.type_scheme * Parsetree.binding_body)
  | MCEE_Declared_logical of (Parsetree.vname * Parsetree.logical_expr)
  | MCEE_Defined_logical of
      (Env.from_history * Parsetree.vname * Parsetree.logical_expr)
;;

val find_coq_env_element_by_name :
  Parsetree.vname -> min_coq_env_element list -> min_coq_env_element
;;

val minimal_typing_environment :
  VisUniverse.in_the_universe_because VisUniverse.Universe.t ->
  Env.TypeInformation.species_field list -> min_coq_env_element list
;;
