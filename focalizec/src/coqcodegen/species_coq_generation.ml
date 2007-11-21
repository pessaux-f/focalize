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

(* $Id: species_coq_generation.ml,v 1.1 2007-11-21 16:34:15 pessaux Exp $ *)


(* *************************************************************** *)
(** {b Descr} : This module performs the compilation from FoCaL to
              Coq of FoCaL's collections and species.            *)
(* *************************************************************** *)



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



(* ************************************************************************ *)
(* current_unit: Parsetree.modname -> Format.formatter ->                   *)
(*   Parsetree.qualified_vname -> unit                                      *)
(** {b Descr}: Pretty prints a [Parsetree.qualified_vname] as a Coq regular
      identifier. If the identifier has a qualification that is different
      of the current compilation unit, then we use the dot-notation.
      Otherwise no qualification is printed.
      No transformation is performed on the ident (no abstraction stuff,
      no change, no prefix) : the name is directly generated as it is.
      If the ident , in fact, has no qualification, then the scoping
      process may have failed earlier because any qualified name must have
      and explicit qualification after the scoping pass.

    { Rem}: Not exported outside this module.                               *)
(* ************************************************************************ *)
let simply_pp_to_coq_qualified_vname ~current_unit ppf = function
  | Parsetree.Vname _ ->
      (* In this case, may be there is some scoping process missing. *)
      assert false
  | Parsetree.Qualified (modname, vname) ->
      if modname <> current_unit then
        Format.fprintf ppf "%s." modname ;
      Parsetree_utils.pp_vname_with_operators_expanded ppf vname
;;



(* [Unsure]
let pp_to_coq_expr_ident ~current_unit ~in_record_type: _ ppf ident =
  match ident.Parsetree.ast_desc with
   | Parsetree.EI_local vname ->
       (* Thanks to the scoping pass, identifiers remaining "local" are *)
       (* really let-bound in the context of the expression, hence have *)
       (* a direrct mapping between FoCaL and Coq code.               *)
       Parsetree_utils.pp_vname_with_operators_expanded ppf vname
   | Parsetree.EI_global qualified_vname ->
       
   | Parsetree.EI_method (coll_specifier_opt, vname) ->
       (begin
       match coll_specifier_opt with
        | None
        | Some (Parsetree.Vname (Parsetree.Vuident "Self")) ->
            (begin
            (* Method call from the current species. This corresponds to *)
            (* a call to the corresponding lambda-lifted method that is  *)
            (* represented as an extra parameter of the OCaml function.  *)
            Format.fprintf ppf "abst_%a"
              Parsetree_utils.pp_vname_with_operators_expanded vname
            end)
       end)
;;
*)


(* [Unsure] *)
let generate_constant ctx cst =
  match cst.Parsetree.ast_desc with
   | Parsetree.C_int _str -> Format.fprintf ctx.scc_out_fmter "C_int"
   | Parsetree.C_float _str -> Format.fprintf ctx.scc_out_fmter "C_float"
   | Parsetree.C_bool _str -> Format.fprintf ctx.scc_out_fmter "C_bool"
   | Parsetree.C_string _str -> Format.fprintf ctx.scc_out_fmter "C_string"
   | Parsetree.C_char _c -> Format.fprintf ctx.scc_out_fmter "C_char"
;;


(* [Unsure] *)
let generate_expr ctx ~in_record_type:_ initial_expression =
  let out_fmter = ctx.scc_out_fmter in
  let rec rec_generate_expr expression = 
    (* The wrapper surrounding the expression by Coq's *)
    (* "Is_true" if the expression's type is [bool].   *)
    let is_bool =
      (match expression.Parsetree.ast_type with
       | Parsetree.ANTI_type ty -> Types.is_bool_type ty
       | Parsetree.ANTI_none
       | Parsetree.ANTI_non_relevant
       | Parsetree.ANTI_scheme _ ->
           (* Note that expression never has a type scheme, but only a type. *)
           assert false) in
    if is_bool then Format.fprintf out_fmter "@[<2>Is_true (" ;
    (* Now, dissecate the expression core. *)
    (match expression.Parsetree.ast_desc with
     | Parsetree.E_self -> Format.fprintf out_fmter "E_self"
     | Parsetree.E_const cst -> generate_constant ctx cst
     | Parsetree.E_fun (_vnames, _body) ->
         Format.fprintf out_fmter "E_fun"
     | Parsetree.E_var _ident ->
(* [Unsure]
         pp_to_coq_expr_ident
           ~current_unit: ctx.scc_current_unit ~in_record_type out_fmter ident
*)
         Format.fprintf out_fmter "E_var"
     | Parsetree.E_app (func_expr, args) ->
         Format.fprintf out_fmter "@[<2>(" ;
         rec_generate_expr func_expr ;
         Format.fprintf out_fmter "@ " ;
         rec_generate_exprs_list args ;
         Format.fprintf out_fmter ")@]"
     | Parsetree.E_constr (_cstr_ident, _args) ->
         Format.fprintf out_fmter "E_constr"
     | Parsetree.E_match (_expr, _pats_exprs) ->
         Format.fprintf out_fmter "E_match"
     | Parsetree.E_if (_expr1, _expr2, _expr3) ->
         Format.fprintf out_fmter "E_if"
     | Parsetree.E_let (_let_def, _in_expr) ->
         Format.fprintf out_fmter "E_let"
     | Parsetree.E_record _labs_exprs ->
         Format.fprintf out_fmter "E_record"
     | Parsetree.E_record_access (_expr, _label) ->
         Format.fprintf out_fmter "E_record_access"
     | Parsetree.E_record_with (_expr, _labels_exprs) ->
         Format.fprintf out_fmter "E_record_with"
     | Parsetree.E_tuple _exprs ->
         Format.fprintf out_fmter "E_tuple"
     | Parsetree.E_external _ext_expr ->
         Format.fprintf out_fmter "E_external"
     | Parsetree.E_paren expr ->
         Format.fprintf out_fmter "@[<1>(" ;
         rec_generate_expr expr ;
         Format.fprintf out_fmter ")@]") ;
    (* The end of the wrapper surrounding the expression if it has type bool. *)
    if is_bool then Format.fprintf out_fmter ")@]"



  and rec_generate_exprs_list = function
    | [] -> ()
    | [last] -> rec_generate_expr last
    | h :: q ->
        rec_generate_expr h ;
        Format.fprintf out_fmter "@ " ;
        rec_generate_exprs_list q in


  (* ************************************************ *)
  (* Now, let's really do the job of [generate_expr]. *)
  rec_generate_expr initial_expression
;;



let generate_prop ctx ~in_record_type initial_proposition =
  let out_fmter = ctx.scc_out_fmter in
  let rec rec_generate_prop proposition =
    match proposition.Parsetree.ast_desc with
     | Parsetree.Pr_forall (vnames, ty_expr, prop)
     | Parsetree.Pr_exists (vnames, ty_expr, prop) ->
         (begin
         (* Create the coq type print context. *)
         let (species_modname, species_vname) = ctx.scc_current_species in
         let print_ctx = {
           Types.cpc_current_unit = ctx.scc_current_unit ;
           Types.cpc_current_species =
           Some
             (species_modname, (Parsetree_utils.name_of_vname species_vname)) ;
           Types.cpc_collections_carrier_mapping =
           (* Throw the [collection_carrier_mapping_info] *)
           (* in the printing context.                    *)
           List.map
             (fun (ctype, (mapped_name, _)) -> (ctype, mapped_name))
             ctx.scc_collections_carrier_mapping } in
         (* Recover the type annotating the type expression. *)
         let ty =
           (match ty_expr.Parsetree.ast_type with
            | Parsetree.ANTI_none
            | Parsetree.ANTI_non_relevant
            | Parsetree.ANTI_scheme _ -> assert false
            | Parsetree.ANTI_type t -> t) in
         let binder =
           (match proposition.Parsetree.ast_desc with
            | Parsetree.Pr_forall (_, _, _) -> "forall"
            | Parsetree.Pr_exists (_, _, _) -> "exists"
            | _ -> assert false) in
         Format.fprintf out_fmter "@[<2>%s@ %a :@ %a,@ "
           binder
           (Handy.pp_generic_separated_list
              " "
              (fun ppf vn ->
                Format.fprintf ppf "%s"
                  (Parsetree_utils.vname_as_string_with_operators_expanded vn)))
           vnames
           (Types.pp_type_simple_to_coq print_ctx) ty ;
         rec_generate_prop prop ;
         Format.fprintf out_fmter "@]"
         end)
     | Parsetree.Pr_imply (prop1, prop2) ->
         Format.fprintf out_fmter "@[<2>" ;
         rec_generate_prop prop1 ;
         Format.fprintf out_fmter " ->@ " ;
         rec_generate_prop prop2 ;
         Format.fprintf out_fmter "@]"
     | Parsetree.Pr_or  (prop1, prop2) ->
         Format.fprintf out_fmter "@[<2>" ;
         rec_generate_prop prop1 ;
         Format.fprintf out_fmter " \\/@ " ;
         rec_generate_prop prop2 ;
         Format.fprintf out_fmter "@]"
     | Parsetree.Pr_and  (prop1, prop2) ->
         Format.fprintf out_fmter "@[<2>" ;
         rec_generate_prop prop1 ;
         Format.fprintf out_fmter " /\\@ " ;
         rec_generate_prop prop2 ;
         Format.fprintf out_fmter "@]"
     | Parsetree.Pr_equiv (prop1, prop2) ->
         Format.fprintf out_fmter "@[<2>" ;
         rec_generate_prop prop1 ;
         Format.fprintf out_fmter " <->@ " ;
         rec_generate_prop prop2 ;
         Format.fprintf out_fmter "@]"
     | Parsetree.Pr_not prop ->
         Format.fprintf out_fmter "@[<2>" ;
         Format.fprintf out_fmter "~" ;
         rec_generate_prop prop ;
         Format.fprintf out_fmter "@]"
     | Parsetree.Pr_expr expr -> generate_expr ctx ~in_record_type expr
     | Parsetree.Pr_paren prop ->
         Format.fprintf out_fmter "@[<1>(" ;
         rec_generate_prop prop ;
         Format.fprintf out_fmter ")@]" in

  (* ************************************************ *)
  (* Now, let's really do the job of [generate_prop]. *)
  rec_generate_prop initial_proposition
;;



(* ************************************************************************ *)
(* current_unit: Parsetree.modname -> Format.formatter -> Parsetree.expr -> *)
(*   unit                                                                   *)
(** {b Descr} : Translate an [expr] expected to be a species parameter
      expression into a Coq type.
      Because species names are capitalized, they must appear as sum
      constructors [expr]s. We also allow to have parentheses surrounding
      the expression. Hence, thsi function only handles these 2 kinds of
      [expr]s.

    {b Rem}: Not exported outside this module.                              *)
(* ************************************************************************ *)
let rec generate_expr_as_species_parameter_expression ~current_unit ppf expr =
  match expr.Parsetree.ast_desc with
   | Parsetree.E_constr (constr_ident, exprs) ->
       (* Remember that species names are capitalized. Hence the only legal *)
       (* core expression denoting species are sum type constructors.       *)
       let Parsetree.CI cstr_qual_name = constr_ident.Parsetree.ast_desc in
       Format.fprintf ppf "%a"
         (simply_pp_to_coq_qualified_vname ~current_unit) cstr_qual_name ;
       if exprs <> [] then
         Format.fprintf ppf "@[<2>@ (%a)@]"
           (Handy.pp_generic_separated_list ";"
              (generate_expr_as_species_parameter_expression ~current_unit))
           exprs
   | Parsetree.E_paren expr' ->
       generate_expr_as_species_parameter_expression ~current_unit ppf expr'
   | _ -> assert false
;;



(* ********************************************************************** *)
(* current_unit: Parsetree.modname -> Format.formatter ->                 *)
(*   Parsetree.species_expr_desc Parsetree.ast -> unit                    *)
(** {b Descr}: Generate a species parameter type constraint as Coq code
       from the [Parsetree.expr] representing the species expression.
       Because this species expression is plugged into the wider
       [Parsetree.expr] datatype, thi function expects to only find
       inductively sum constructors possibly encapsulated in parentheses.

    {b Rem} : Not exported outside this module.                           *)
(* ********************************************************************** *)
let generate_parameter_species_expr ~current_unit ppf species_expr =
  let expr_desc = species_expr.Parsetree.ast_desc in
  (match expr_desc.Parsetree.se_name.Parsetree.ast_desc with
   | Parsetree.I_local vname ->
       Parsetree_utils.pp_vname_with_operators_expanded ppf vname
   | Parsetree.I_global qident ->
       simply_pp_to_coq_qualified_vname ~current_unit ppf qident) ;
  if expr_desc.Parsetree.se_params <> [] then
    (begin
    Format.fprintf ppf "@[<1>(" ;
    List.iter
      (fun param ->
        let Parsetree.SP expr = param.Parsetree.ast_desc in
        generate_expr_as_species_parameter_expression ~current_unit ppf expr)
      expr_desc.Parsetree.se_params ;
    Format.fprintf ppf ")@]"
    end)
;;



(* ********************************************************************** *)
(* species_compil_context -> unit                                         *)
(** {b Descr}: Generate the Coq code of a species parameters. It outputs
      both the parameters names and their type as a Coq expression.
      Either the parameter is a "is" parameter and then it's type will be
      rebuilt from its species expression.
      Or it is a "in" parameter and then it's type is a collection type,
      and is directly the one recorded in the first couple of the
      collections_carrier_mapping binding for this parameter.

    {b Rem} : Not exported outside this module.                           *)
(* ********************************************************************** *)
let generate_record_type_parameters ctx =
  let ppf = ctx.scc_out_fmter in
  List.iter
    (fun ((param_ty_mod, param_ty_coll), (param_name, param_kind)) ->
      Format.fprintf ppf "@[<1>(%s :@ " param_name ;
      (match param_kind with
       | CCMI_is parameter_expr ->
           generate_parameter_species_expr
             ~current_unit: ctx.scc_current_unit ppf parameter_expr
       | CCMI_in_or_not_param ->
           if param_ty_mod <> ctx.scc_current_unit then
             Format.fprintf ppf "%s." param_ty_mod ;
           Format.fprintf ppf "%s" param_ty_coll) ;
      Format.fprintf ppf ")@ @]")
    ctx.scc_collections_carrier_mapping
;;



(* ************************************************************************* *)
(* species_compil_context -> Env.TypeInformation.species_description -> unit *)
(** {b Descr} : Generate the record type representing a species. This type
          contains a field per method. This type is named as the species.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let generate_record_type ctx species_descr =
  let out_fmter = ctx.scc_out_fmter in
  let collections_carrier_mapping = ctx.scc_collections_carrier_mapping in
  (* The header of the Coq record definition for the species. *)
  let (my_fname, my_species_name) = ctx.scc_current_species in
  (* Directly trasform into a string, that's easier. *)
  let my_species_name = Parsetree_utils.name_of_vname my_species_name in
  Format.fprintf out_fmter "@[<2>Record %s " my_species_name ;
  (* Generate the record parameters mapping the species parameters. *)
  generate_record_type_parameters ctx ;
  (* Print the type of the record and it's constructor. *)
  Format.fprintf out_fmter ": Type :=@ mk_%s {@\n" my_species_name  ;
  (* We now extend the collections_carrier_mapping with ourselve known.  *)
  (* Hence, if we refer to our "rep" we will be directly mapped onto the *)
  (* species's name + "_T" without needing to re-construct this name     *)
  (* each time. *)
  let collections_carrier_mapping =
    ((my_fname, my_species_name),
     ((my_species_name ^ "_T"), CCMI_in_or_not_param)) ::
    collections_carrier_mapping in
  (* Create the coq type print context. *)
  let (species_modname, species_vname) = ctx.scc_current_species in
  let print_ctx = {
    Types.cpc_current_unit = ctx.scc_current_unit ;
    Types.cpc_current_species =
      Some (species_modname, (Parsetree_utils.name_of_vname species_vname)) ;
    Types.cpc_collections_carrier_mapping =
      (* Throw the [collection_carrier_mapping_info] in the printing context. *)
      List.map
        (fun (ctype, (mapped_name, _)) -> (ctype, mapped_name))
        collections_carrier_mapping } in
  (* Always generate the "rep" coercion. *)
  Format.fprintf out_fmter "@[<2>%s_T :> Set" my_species_name ;
  (* Put a trailing semi only if there is other fields to generate. *)
  (match species_descr.Env.TypeInformation.spe_sig_methods with
   | [] -> ()
   | [Env.TypeInformation.SF_sig (_, n, _)] ->
       if (Parsetree_utils.name_of_vname n) <> "rep" then
         ()   (* Case where there was only 1 field and that field was "rep". *)
       else Format.fprintf out_fmter " ;"
   | _ -> Format.fprintf out_fmter " ;") ;
  Format.fprintf out_fmter "@]@\n" ;
  (* We must now generate the record's fields types. *)
  let output_one_field ~semi = function
    | Env.TypeInformation.SF_sig (from, n, sch)
    | Env.TypeInformation.SF_let (from, n, _, sch, _) ->
        (begin
        (* Skip "rep", because it is processed just above. *)
        if (Parsetree_utils.name_of_vname n) <> "rep" then
          (begin
          let ty = Types.specialize sch in
          Format.fprintf out_fmter "(* From species %a. *)@\n"
            Sourcify.pp_qualified_species from ;
          (* Field is prefixed by the species name for sake of unicity. *)
          Format.fprintf out_fmter "@[<2>%s_%a : %a"
            my_species_name
            Parsetree_utils.pp_vname_with_operators_expanded n
            (Types.pp_type_simple_to_coq print_ctx) ty ;
          if semi then Format.fprintf out_fmter " ;" ;
          Format.fprintf out_fmter "@]@\n"
          end)
        end)
    | Env.TypeInformation.SF_let_rec l ->
        List.iter
          (fun (from, n, _, sch, _) ->
            let ty = Types.specialize sch in
            Format.fprintf out_fmter "(* From species %a. *)@\n"
              Sourcify.pp_qualified_species from ;
            (* Field is prefixed by the species name for sake of unicity. *)
            Format.fprintf out_fmter "%s_%a : %a"
              my_species_name
              Parsetree_utils.pp_vname_with_operators_expanded n
              (Types.pp_type_simple_to_coq print_ctx) ty ;
            if semi then Format.fprintf out_fmter " ;" ;
            Format.fprintf out_fmter "@]@\n")
          l
    | Env.TypeInformation.SF_theorem  (from, n, _, prop, _)
    | Env.TypeInformation.SF_property (from, n, _, prop) ->
        (* In the record type, theorems and      *)
        (* properties are displayed in same way. *)
        Format.fprintf out_fmter "(* From species %a. *)@\n"
          Sourcify.pp_qualified_species from ;
        (* Field is prefixed by the species name for sake of unicity. *)
        Format.fprintf out_fmter "@[<2>%s_%a :@ "
          my_species_name Parsetree_utils.pp_vname_with_operators_expanded n ;
        (* Generate the Coq code representing the proposition. *)
        generate_prop ctx ~in_record_type: true prop ;
        if semi then Format.fprintf out_fmter " ;" ;
        Format.fprintf out_fmter "@]@\n" in
  (* Coq syntax required not semi after the last field. That's why   *)
  (* a simple [List.iter] of [output_one_field]'s body doesn't work. *)
  (* One must separate the case of the last element of the list.     *)
  let rec iter_semi_separated = function
    | [] -> ()
    | [last] -> output_one_field ~semi: false last
    | h :: q ->
        output_one_field ~semi: true h ;
        iter_semi_separated q in
  iter_semi_separated species_descr.Env.TypeInformation.spe_sig_methods ;
  Format.fprintf out_fmter "@]}.@\n"
;;



(* ************************************************************************** *)
(** {b Descr} : Lower-level species field (relevant for collection generator)
        description recording information about dependency and extra
        parameters already computed while generating the methods and that
        will be re-used while generating the collection generator.
        This avoids computing several the same things and ensure that the
        information is formated in the same way everywhere (in other words
        that the extra parameters discovered will appear in the same order
        between method declaration and method application).
    {b Rem} : Not exported outside this module.                               *)
(* ************************************************************************** *)
type compiled_field_memory = {
  (** Where the method comes from (the most recent in inheritance). *)
  cfm_from_species : Parsetree.qualified_species ;
  (** The method's name. *)
  cfm_method_name : Parsetree.vname ;
  (** The method's body. *)
  cfm_method_body  : Parsetree.expr ;
  (** The list mapping for each species parameter, the methods the current
      method depends on. By lambda-lifting, these methods induce extra
      parameters named as "_p_" +  species parameter name + "_" + called
      method's name we depend on. The first component of each couple is the
      parameter's name and the second is the set of methods the current
      method depends on from this species parameter.*)
  cfm_dependencies_from_parameters :
    (Parsetree.vname * Parsetree_utils.VnameSet.t) list ;
  (** The methods of our inheritance tree the method depends on. *)
  cfm_decl_children :
    (Dep_analysis.name_node * Dep_analysis.dependency_kind) list ;
} ;;



(* ************************************************************************ *)
(* current_unit: Types.fname -> Env.TypeInformation.species_description ->  *)
(*   (Types.type_collection * string) list                                  *)
(** {b Descr} : Create the correspondance between the collection type of
              the species definition parameters and the names to be used
              later during the Coq translation.
              For a species parameter [A is/in ... ], the name that will be
              used is directly the name of the species parameter + an int
              unique in this type.
              We need to add an extra int (a stamp) to prevent a same name
              variable from appearing several time in the tricky case where
              a IN and a IS parameters wear the same lowercased name. For
              instance: "species A (F is B, f in F)" where "F" and "f" will
              lead to a same Coq name : "f"

    {b Rem} : Not exported outside this module.                             *)
(* ************************************************************************ *)
let build_collections_carrier_mapping ~current_unit species_descr =
  let cnt = ref 0 in
  List.map
    (function
      | Env.TypeInformation.SPAR_is (n, _, param_expr) ->
          let n_as_string = Parsetree_utils.name_of_vname n in
          (* Build the name that will represent this *)
          (* species parameter seen from Coq.        *)
          let carrier_name =
            (Parsetree_utils.name_of_vname n) ^ (string_of_int !cnt) in
          incr cnt ;
          (* Now, build the "collection type" this name will be bound to. *)
          (* According to how the "collection type" of parameters are     *)
          (* built, this will be the couple of the current compilation    *)
          (* unit and the name of the parameter.                          *)
          let type_coll = (current_unit, n_as_string) in
          (* And now create the binding... Record that the parameter is a *)
          (* "is" parameter whose species expr is [param_expr] that will  *)
          (* be used to create the Coq type expression annotating this    *)
          (* parameter in the hosting species record type.                *)
          (type_coll, (carrier_name, (CCMI_is param_expr)))
      | Env.TypeInformation.SPAR_in (n, type_coll) ->
          (* Build the name that will represent this parameter's *)
          (* carrier seen from Coq.                              *)
          let carrier_name =
            (Parsetree_utils.name_of_vname n) ^ (string_of_int !cnt) in
          incr cnt ;
          (* Record that the parameter is a "in" parameter. Then we don't    *)
          (* need any species expression to annotate this parameter in the   *)
          (* Coq type expression annotating this parameter in the hosting    *)
          (* species record type: it will simply be of the type [type_coll]. *)
          (type_coll, (carrier_name, CCMI_in_or_not_param)))
    species_descr.Env.TypeInformation.spe_sig_params
;;



let species_compile ~current_unit out_fmter species_def species_descr
    dep_graph =
  let species_def_desc = species_def.Parsetree.ast_desc in
  let species_name = species_def_desc.Parsetree.sd_name in
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf "Generating Coq code for species %a@."
      Sourcify.pp_vname species_name ;
  (* Always import Coq booleans and integers. *)
  Format.fprintf out_fmter
    "Require Export Bool.@\nRequire Export ZArith.@\n@\n" ;
  (* Start the chapter encapsulating the species representation. *)
  let chapter_name =
    String.capitalize (Parsetree_utils.name_of_vname species_name) in
  Format.fprintf out_fmter "@[<2>Chapter %s.@\n" chapter_name ;
  (* Now, establish the mapping between collections available *)
  (* and the names representing their carrier.                *)
  let collections_carrier_mapping =
    build_collections_carrier_mapping ~current_unit species_descr in
  (* Create the initial compilation context for this species. *)
  let ctx = {
    scc_current_unit = current_unit ;
    scc_current_species = (current_unit, species_name) ;
    scc_dependency_graph_nodes = dep_graph ;
    scc_collections_carrier_mapping = collections_carrier_mapping ;
    scc_lambda_lift_params_mapping = [] ;
    scc_out_fmter = out_fmter } in
  (* The record type representing the species' type. *)
  generate_record_type ctx species_descr ;

  Format.fprintf out_fmter "@]End %s.@\n@." chapter_name
;;
