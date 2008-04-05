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

(* $Id: minEnv.mli,v 1.3 2008-04-05 18:52:44 weis Exp $ *) 

type min_coq_env_element =
    MCEE_Declared_carrier
  | MCEE_Defined_carrier of Types.type_scheme
  | MCEE_Declared_computational of (Parsetree.vname * Types.type_scheme)
  | MCEE_Defined_computational of
      (Parsetree.qualified_species * Parsetree.vname * Types.type_scheme)
  | MCEE_Declared_logical of (Parsetree.vname * Parsetree.logical_expr)
  | MCEE_Defined_logical of
      (Parsetree.qualified_species * Parsetree.vname * Parsetree.logical_expr)

val minimal_typing_environment :
  VisUniverse.in_the_universe_because VisUniverse.Universe.t ->
  Env.TypeInformation.species_field list -> min_coq_env_element list
