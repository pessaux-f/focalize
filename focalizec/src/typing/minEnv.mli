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


val find_coq_env_element_by_name :
  Parsetree.vname -> Env.TypeInformation.min_coq_env_element list ->
    Env.TypeInformation.min_coq_env_element

val minimal_coq_typing_environment :
  VisUniverse.in_the_universe_because VisUniverse.Universe.t ->
  Env.TypeInformation.species_field list ->
    Env.TypeInformation.min_coq_env_element list

val find_dk_env_element_by_name :
  Parsetree.vname -> Env.TypeInformation.min_dk_env_element list ->
    Env.TypeInformation.min_dk_env_element

val minimal_dk_typing_environment :
  VisUniverse.in_the_universe_because VisUniverse.Universe.t ->
  Env.TypeInformation.species_field list ->
    Env.TypeInformation.min_dk_env_element list
