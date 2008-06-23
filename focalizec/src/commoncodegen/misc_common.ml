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

(* $Id: misc_common.ml,v 1.2 2008-06-23 16:26:25 pessaux Exp $ *)



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


(* *********************************************************************** *)
(* Format.formatter -> compiled_species_fields list ->                     *)
(*  (Parsetree.vname * Parsetree_utils.DepNameSet.t) list                  *)
(** {b Descr} : Dumps as OCaml code the parameters required to the
         collection generator in order to make them bound in the
         collection generator's body. These parameters come from
         the methods of the species parameters that some of our methods
         depend on. This means that a closed species with no species
         parameters will have NO extra parameters in its collection
         generator.

         This function must UNIQUELY find the names of all the extra
         parameters the methods will need to make them arguments of the
         collection generator and record then in a precise order that must
         be made public for the guys who want to instanciate the collection.

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let dump_collection_generator_arguments_for_params_methods out_fmter
    compiled_species_fields =
  (* Let's create an assoc list mapping for each species paramater name *)
  (* the set of methods names from it that needed to be lambda-lifted,  *)
  (* hence that will lead to parameters of the collection generator.    *)
  let species_param_names_and_methods =
    ref ([] : (Parsetree.vname * Parsetree_utils.DepNameSet.t ref) list) in
  (* ************************************************************************ *)
  (** {b Descr} :  Local function to process only one [compiled_field_memory].
         Handy to factorize the code in both the cases of [CSF_let] and
         [CSF_let_rec]. This function effectivly accumulates by side effect
         for each species parameter the set of methods we depend on.

      { b Rem} : Local to the enclosing [dump_collection_generator_arguments]
               function. Not exported.                                        *)
  (* ************************************************************************ *)
  let rec process_one_field_memory field_memory =
    List.iter
      (fun (spe_param, meths_set) ->
        (* Recover the species parameter's name. *)
        let spe_param_name =
          match spe_param with
           | Env.TypeInformation.SPAR_in (n, _) -> n
           | Env.TypeInformation.SPAR_is ((_, n), _, _) ->
               Parsetree.Vuident n in
        (* Get or create for this species parameter name, the bucket *)
        (* recording all the methods someone depends on.             *)
        (* We don't care here about whether the species parameters is   *)
        (* "in" or "is".                                                *)
        let spe_param_bucket =
          (try List.assoc spe_param_name !species_param_names_and_methods
          with Not_found ->
            let bucket = ref Parsetree_utils.DepNameSet.empty in
            species_param_names_and_methods :=
              (spe_param_name, bucket) :: !species_param_names_and_methods ;
            bucket) in
        (* And now, union the current methods we depend on with *)
        (* the already previously recorded.                     *)
        spe_param_bucket :=
          Parsetree_utils.DepNameSet.union meths_set !spe_param_bucket)
      field_memory.cfm_dependencies_from_parameters in

  (* ********************************************************** *)
  (* Now, really work, building by side effect for each species *)
  (* parameter the set of methods we depend on.                 *)
  List.iter
    (function
      | CSF_sig _ | CSF_property _ | CSF_theorem _ -> ()
      | CSF_let field_memory -> process_one_field_memory field_memory
      | CSF_let_rec l -> List.iter process_one_field_memory l)
    compiled_species_fields ;
  (* Now we get the assoc list complete, we can dump the parameters of the  *)
  (* collection generator. To make them correct with their usage inside the *)
  (* local functions of the collection generator, we must give them a name  *)
  (* shaped in the same way, i.e:                                           *)
  (* "_p_" + species parameter name + "_" + called method name.             *)
  List.iter
    (fun (species_param_name, meths_set) ->
      let prefix =
        "_p_" ^ (Parsetree_utils.name_of_vname species_param_name) ^
        "_" in
      Parsetree_utils.DepNameSet.iter
        (fun (meth, _) ->
          (* Don't print the type to prevent being too verbose. *)
          Format.fprintf out_fmter "@ %s%a"
            prefix Parsetree_utils.pp_vname_with_operators_expanded meth)
        !meths_set)
  !species_param_names_and_methods ;
  (* Finally, make this parameters information public by returning it. By     *)
  (* the way, the ref on the inner set is not anymore needed, then remove it. *)
  List.map
    (fun (sp_par_name, meths_set) -> (sp_par_name, !meths_set))
    !species_param_names_and_methods
;;
