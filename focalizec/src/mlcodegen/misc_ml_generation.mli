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

(* $Id: misc_ml_generation.mli,v 1.10 2008-01-25 15:21:10 pessaux Exp $ *)



val pp_to_ocaml_label_ident : Format.formatter -> Parsetree.label_ident -> unit

type reduced_compil_context = {
  rcc_current_unit : Types.fname ;
  rcc_species_parameters_names : Parsetree.vname list ;
  rcc_collections_carrier_mapping : (Types.type_collection * string) list ;
  rcc_lambda_lift_params_mapping :
    (Parsetree.vname * ((string * Types.type_simple )list)) list ;
  rcc_out_fmter : Format.formatter
}

val bind_parameters_to_types_from_type_scheme :
  Types.type_scheme option -> Parsetree.vname list ->
    (((Parsetree.vname * Types.type_simple option) list) *
      (Types.type_simple option) *
      (Types.type_simple list))

type field_body_kind =
  | FBK_expr of Parsetree.expr
  | FBK_prop of Parsetree.prop

val compute_lambda_liftings_for_field :
  current_species: Parsetree.qualified_species -> Parsetree.vname list ->
    Dep_analysis.name_node list -> Parsetree.vname ->
      field_body_kind ->
        ((Parsetree.vname * Parsetree_utils.DepNameSet.t) list *
         (Dep_analysis.name_node * Dep_analysis.dependency_kind) list *
         (Dep_analysis.name_node * Dep_analysis.dependency_kind) list *
         (string * Types.type_simple) list)
