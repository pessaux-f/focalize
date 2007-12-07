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

(* $Id: species_gen_basics.mli,v 1.2 2007-12-07 15:19:37 pessaux Exp $ *)


(* *********************************************************************** *)
(* {b Descr}: Describes in the [scc_collections_carrier_mapping] the kind
     of species parameter.
     It can either be a "is" parameter and then we keep its species
     expression in order to be able to generate the Coq type expression
     annotating this parameter in the hosting species's record type.
     Otherwise, it is a "in" parameter or not at all a parameter and the
     type expression that will annotate this parameter (if it appears to be
     one) in the hosting species's record type is straightly the type
     (as a [Types.collection_type]) of this parameter. And if it is not a
     parameter, then in case of need to annotate, the type will be shaped
     exactly the same way.

   {b Rem} : Not exported outside this module.                             *)
(* *********************************************************************** *)
type collection_carrier_mapping_info =
    (** The parameter is a "is" parameter whose species expression follows. *)
  | CCMI_is of Parsetree.species_expr
    (** The parameter is a "in" parameter or is not a parameter. *)
  | CCMI_in_or_not_param
;;



(* ********************************************************************* *)
(** {b Descr} : Data structure to record the various stuff needed to
          generate the Coq code for a species definition. Passing this
          structure prevents from recursively passing a bunch of
          parameters to the functions. Instead, one pass only one and
          functions use the fields they need. This is mostly to preserve
          the stack and to make the code more readable. In fact,
          information recorded in this structure is semantically pretty
          un-interesting to understand the compilation process: it is
           more utilities.

    {b Rem} Not exported outside this module.                            *)
(* ********************************************************************* *)
type species_compil_context = {
  (** The name of the currently analysed compilation unit. *)
  scc_current_unit : Types.fname ;
  (** The name of the current species. *)
  scc_current_species : Parsetree.qualified_species ;
  (** The nodes of the current species's dependency graph. *)
  scc_dependency_graph_nodes : Dep_analysis.name_node list ;
  (** The list of the current species species parameters if we are in the
      scope of a species and if it has some parameters. *)
  scc_species_parameters_names : Parsetree.vname list ;
  (** The current correspondance between collection parameters names and
      the names they are mapped onto in the Coq code and their kind. *)
  scc_collections_carrier_mapping :
    (Types.type_collection * (string * collection_carrier_mapping_info)) list ;
  (** The current correspondance between method names of Self and their
      extra parameters they must be applied to because of the lambda-lifting
      process. This info is used when generating the Coq code of a
      method, hence it is only relevant in case of recursive methods to know
      in their own body what they must be applied to in addition to their
      explicit arguments (those given by the FoCaL programmer). *)
  scc_lambda_lift_params_mapping : (Parsetree.vname * (string list)) list ;
  (** The current output formatter where to send the generated code. *)
  scc_out_fmter : Format.formatter
} ;;
