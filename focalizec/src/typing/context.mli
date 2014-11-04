(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            Fran�ois Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                                                                     *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007, 2008 LIP6 and INRIA                                *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: context.mli,v 1.10 2012-02-08 16:35:29 pessaux Exp $ *)


(* ************************************************************************** *)
(** {b Descr} : Data structure to record the various stuff needed to generate
      the Caml/Coq/Dedukti code for a species definition. Passing this structure
      prevents from recursively passing a bunch of parameters to the functions.
      Instead, one pass only one and functions use the fields they need. This
      is mostly to preserve the stack and to make the code more readable. In
      fact, information recorded in this structure is semantically pretty
      un-interesting to understand the compilation process: it is
      more utilities.

    {b Attention}: This structure is intended to be built once in the
      [toplevel_compile] function and never modified or updated during code
      generation recursions. Hence, please do not change this assumption since
      nobody pass a modified context to a recursive call. Please, keep the
      usage of this structure simple this way.

    {b Visibility} Not exported outside this module.                          *)
(* ************************************************************************** *)
type species_compil_context = {
  (** The name of the currently analysed compilation unit. *)
  scc_current_unit : Types.fname ;
  (** The name of the current species. *)
  scc_current_species : Parsetree.qualified_species ;
  (** The nodes of the current species's dependency graph. *)
  scc_dependency_graph_nodes : DepGraphData.name_node list ;
  (** The list of the current species species parameters if we are in the
      scope of a species and if it has some parameters. We record for
      each parameter it's kind (i.e. "in" or "is"). For "is" parameters, the
      name is in [Env.TypeInformation.SPAR_is ((_, n), _, _)].
      For "in" parameters, the name is [Env.TypeInformation.SPAR_is (n, _)]. *)
  scc_species_parameters_names : Env.TypeInformation.species_param list ;
  (** The current correspondance between collection parameters names and
      the names they are mapped onto in the Coq/Dedukti code and their kind. *)
  scc_collections_carrier_mapping : Types.collection_carrier_mapping ;
  (** The current correspondance between method names of Self and their
      extra parameters they must be applied to because of the lambda-lifting
      process. This info is used when generating the code of a
      method, hence it is only relevant in case of recursive methods to know
      in their own body what they must be applied to in addition to their
      explicit arguments (those given by the FoCaL programmer). *)
  scc_lambda_lift_params_mapping :
    (Parsetree.vname * (string list)) list ;
  (** The current output formatter where to send the generated code. *)
  scc_out_fmter : Format.formatter
} ;;



(* ********************************************************************** *)
(** {b Descr} : Reduced data structure to record the various stuff needed
          to generate Caml/Coq/Dedukti code for various constructs. Passing this
          structure prevents from recursively passing a bunch of
          parameters to the functions. Instead, one pass only one and
          functions use the fields they need. This is mostly to preserve
          the stack and to make the code more readable. In fact,
          information recorded in this structure is semantically pretty
          uninteresting to understand the compilation process: it is more
          utilities.

    {b Rem} Exported outside this module.                                 *)
(* ********************************************************************** *)
type reduced_compil_context = {
  (** The name of the currently analysed compilation unit. *)
  rcc_current_unit : Types.fname ;
  (** The list of the current species species parameters if we are in the
      scope of a species and if it has some parameters. We record for each
      parameter it's kind (i.e. "in" or "is"). For "is" parameters, the
      name is in [Env.TypeInformation.SPAR_is ((_, n), _, _)].
      For "in" parameters, the name is [Env.TypeInformation.SPAR_is (n, _)]. *)
  rcc_species_parameters_names : Env.TypeInformation.species_param list ;
  (** The current correspondance between collection types and type variable
      names representing the carrier of a species type in the OCaml code. *)
  rcc_collections_carrier_mapping : Types.collection_carrier_mapping ;
  (** The current correspondance between method names of Self and their
      extra parameters they must be applied to because of the lambda-lifting
      process. This info is used when generating the code of a
      method, hence it is only relevant in case of recursive methods to know
      in their own body what they must be applied to in addition to their
      explicit arguments (those given by the FoCaL programmer). *)
  rcc_lambda_lift_params_mapping :
   (Parsetree.vname * (string list)) list ;
  (** The current output formatter where to send the generated code. *)
  rcc_out_fmter : Format.formatter
} ;;
