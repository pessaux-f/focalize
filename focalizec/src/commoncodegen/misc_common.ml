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

(* $Id: misc_common.ml,v 1.3 2008-06-25 10:42:54 pessaux Exp $ *)



(* ************************************************************************** *)
(** {b Descr} : Lower-level species field (relevant for collection generator)
        description recording information about dependency and extra
        parameters already computed while generating the methods and that
        will be re-used while generating the collection generator.
        This avoids computing several the same things and ensure that the
        information is formated in the same way everywhere (in other words
        that the extra parameters discovered will appear in the same order
        between method declaration and method application).

    {b Rem} : Exported outside this module.                                   *)
(* ************************************************************************** *)
type compiled_field_memory = {
  (** Where the method comes from via inheritance history. *)
  cfm_from_species : Env.from_history ;
  (** The method's name. *)
  cfm_method_name : Parsetree.vname ;
  (* The positionnal list of species parameter carriers appearing in the
     type of the method. They lead to extra arguments of type "Set" and
     must be instanciated by the correct type when applying the method's
     generator. This is mostly due to the fact that in Coq polymorphism
     is explicit and lead to a dependant type.
     NEVER USED FOR OCAML CODE GENERATION. *)
  cfm_used_species_parameter_tys : Parsetree.vname list ;
  (** The list mapping for each species parameter, the methods the current
      method depends on. By lambda-lifting, these methods induce extra
      parameters named as "_p_" +  species parameter name + "_" + called
      method's name we depend on. The first component of each couple is the
      parameter's name and kind and the second is the set of methods the
      current method depends on from this species parameter. This contains
      ALL the dependencies found via definition 72 p 153 in Virgile Prevosto's
      PhD. *)
  cfm_dependencies_from_parameters :
    (Env.TypeInformation.species_param * Parsetree_utils.DepNameSet.t) list ;
  (** The positional list of method names appearing in the minimal Coq typing
      environment. *)
  cfm_coq_min_typ_env_names : Parsetree.vname list
} ;;



(* ************************************************************************ *)
(** {b Descr} : Type of the fields significant for the collection generator
       creation, once the methods of the species have been generated. It
       remove all the fields that are not relevant for the collection
       generator.

    {b Rem} : Exported outside this module.                                 *)
(* ************************************************************************ *)
type compiled_species_fields =
  | CSF_sig of compiled_field_memory
  | CSF_let of compiled_field_memory
  | CSF_let_rec of compiled_field_memory list
  | CSF_theorem of compiled_field_memory
  | CSF_property of compiled_field_memory
;;



type collection_effective_arguments =
  | CEA_collection_name_for_is of Parsetree.qualified_vname
  | CEA_value_expr_for_in of Parsetree.expr
;;



(* ************************************************************************* *)
(* Parsetree.species_param list ->                                           *)
(*   (Parsetree.vname * Env.ScopeInformation.species_parameter_kind) list    *)
(*     collection_effective_arguments list                                   *)
(** {b Descr} : Extract the collections names used in an "implements" clause
       as arguments of the species that it used to make the collection.
       The parsetree encodes these parameters [Parsetree.expr]s but this
       is a too large structure for the actual legal parameters expressions.
       Then we extracts here just the names of effective collections hidden
       in these [Parsetree.expr]s.

    {b Rem} : Exported outside this module.                                  *)
(* ************************************************************************* *)
let get_implements_effectives species_params_exprs species_formals_info =
  List.map2
    (fun param_expr (_, param_kind) ->
      let Parsetree.SP expr = param_expr.Parsetree.ast_desc in
      match param_kind with
       | Env.ScopeInformation.SPK_is ->
           (begin
           match expr.Parsetree.ast_desc with
            | Parsetree.E_constr (cstr_ident, []) ->
                let Parsetree.CI effective_species_name =
                  cstr_ident.Parsetree.ast_desc in
                CEA_collection_name_for_is effective_species_name
            | _ ->
                (* Collections expressions used as parameters of an      *)
                (* "implements" clause should always be represented by   *)
                (* a sum-type value and because these collections are    *)
                (* not parametrized, this value should have no argument. *)
                (* If it's not the case here, then we missed something   *)
                (* before during the analyses !                          *)
                assert false
           end)
       | Env.ScopeInformation.SPK_in ->
           (* For an entity parameter, all first-class expressions are legal. *)
           CEA_value_expr_for_in expr)
    species_params_exprs
    species_formals_info
;;
