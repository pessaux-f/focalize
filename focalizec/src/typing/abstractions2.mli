type field_body_kind =
    FBK_expr of Parsetree.expr
  | FBK_logical_expr of Parsetree.logical_expr
  | FBK_proof of Parsetree.proof option
type field_type_kind =
    FTK_computational of Types.type_simple
  | FTK_logical of Parsetree.logical_expr
type abstractions_comput_context = {
  acc_current_unit : Types.fname;
  acc_current_species : Parsetree.qualified_species;
  acc_dependency_graph_nodes : DepGraphData.name_node list;
  acc_species_parameters_names : Env.TypeInformation.species_param list;
}
val make_empty_param_deps :
  Env.TypeInformation.species_param list ->
    (Env.TypeInformation.species_param * Parsetree_utils.ParamDepSet.t) list

val compute_abstractions_for_fields :
  Env.TypingEnv.t ->
  abstractions_comput_context ->
  Env.TypeInformation.species_field list ->
  (VisUniverse.Universe.key * Env.TypeInformation.field_abstraction_info)
  list
val compute_abstractions_for_toplevel_theorem :
  abstractions_comput_context ->
  Parsetree.theorem_def_desc Parsetree.ast ->
  Env.TypeInformation.field_abstraction_info
