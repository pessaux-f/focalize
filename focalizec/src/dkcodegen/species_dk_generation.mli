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

val species_compile :
  Env.DkGenEnv.t -> current_unit: Types.fname -> Format.formatter ->
    Parsetree.species_def_desc Parsetree.ast ->
      Env.TypeInformation.species_description ->
        DepGraphData.name_node list ->
          (Parsetree.vname * Env.TypeInformation.field_abstraction_info) list ->
          (Env.TypeInformation.species_param list *
           Env.DkGenInformation.method_info list *
           (Env.DkGenInformation.collection_generator_info option) *
           Env.collection_or_species)

val collection_compile :
  Env.DkGenEnv.t -> current_unit: Types.fname -> Format.formatter ->
    Parsetree.collection_def_desc Parsetree.ast ->
      Env.TypeInformation.species_description ->
        DepGraphData.name_node list ->
          Env.DkGenInformation.method_info list
