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

(* $Id: species_record_type_generation.ml,v 1.2 2007-12-04 16:51:14 pessaux Exp $ *)



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



let generate_expr_ident_for_E_var ctx ident =
  let out_fmter = ctx.Species_gen_basics.scc_out_fmter in
  match ident.Parsetree.ast_desc with
   | Parsetree.EI_local vname ->
      (* [Unsure] : incomplet au possible. le code ci dessous gère uniquement
	 les idents qui sont vraiment des variables locales ou des appels
	 récursifs. Si l'identificateur est un paramètre en "in" ça foire
	 complètement, bien sûr !!! *)
       Parsetree_utils.pp_vname_with_operators_expanded out_fmter vname
   | Parsetree.EI_global (Parsetree.Vname _) ->
       (* In this case, may be there is some scoping process missing. *)
       assert false
   | Parsetree.EI_global (Parsetree.Qualified (mod_name, vname)) ->
       (* Call the Coq corresponding identifier in the corresponding   *)
       (* module (i.e. the [mod_name]). If the module is the currently *)
       (* compiled one, then do not qualify the identifier.            *)
       if mod_name <> ctx.Species_gen_basics.scc_current_unit then
         Format.fprintf out_fmter "%s.%a"
           mod_name Parsetree_utils.pp_vname_with_operators_expanded vname
       else
         Format.fprintf out_fmter "%a"
           Parsetree_utils.pp_vname_with_operators_expanded vname
   | Parsetree.EI_method (coll_specifier_opt, vname) ->
       (begin
       match coll_specifier_opt with
        | None
        | Some (Parsetree.Vname (Parsetree.Vuident "Self")) ->
            (begin
            (* Method call from the current species. Mapped onto the current *)
	    (* species' method, i.e. the species name + "_" + method name.   *)
            Format.fprintf out_fmter "%a_%a"
	      Parsetree_utils.pp_vname_with_operators_expanded
	        (snd ctx.Species_gen_basics.scc_current_species)
              Parsetree_utils.pp_vname_with_operators_expanded vname
            end)
| _ -> (* [Unsure]*) Format.fprintf out_fmter "todo"
       end)
;;






(* [Unsure] *)
let generate_constant ctx cst =
  match cst.Parsetree.ast_desc with
   | Parsetree.C_int _str ->
       Format.fprintf ctx.Species_gen_basics.scc_out_fmter "C_int"
   | Parsetree.C_float _str -> Format.fprintf ctx.Species_gen_basics.scc_out_fmter "C_float"
   | Parsetree.C_bool _str -> Format.fprintf ctx.Species_gen_basics.scc_out_fmter "C_bool"
   | Parsetree.C_string _str -> Format.fprintf ctx.Species_gen_basics.scc_out_fmter "C_string"
   | Parsetree.C_char _c -> Format.fprintf ctx.Species_gen_basics.scc_out_fmter "C_char"
;;


(* [Unsure] *)
let generate_expr ctx initial_expression =
  let out_fmter = ctx.Species_gen_basics.scc_out_fmter in
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
     | Parsetree.E_self ->
         (* [Unsure] D'ailleurs, est-ce possible en fait ? *)
         Format.eprintf "generate_expr E_self TODO@."
     | Parsetree.E_const cst -> generate_constant ctx cst
     | Parsetree.E_fun (_vnames, _body) ->
         Format.fprintf out_fmter "E_fun"
     | Parsetree.E_var ident -> generate_expr_ident_for_E_var ctx ident
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



let generate_prop ctx initial_proposition =
  let out_fmter = ctx.Species_gen_basics.scc_out_fmter in
  let rec rec_generate_prop proposition =
    match proposition.Parsetree.ast_desc with
     | Parsetree.Pr_forall (vnames, ty_expr, prop)
     | Parsetree.Pr_exists (vnames, ty_expr, prop) ->
         (begin
         (* Create the coq type print context. *)
         let (species_modname, species_vname) =
	   ctx.Species_gen_basics.scc_current_species in
         let print_ctx = {
           Types.cpc_current_unit = ctx.Species_gen_basics.scc_current_unit ;
           Types.cpc_current_species =
           Some
             (species_modname, (Parsetree_utils.name_of_vname species_vname)) ;
           Types.cpc_collections_carrier_mapping =
           (* Throw the [collection_carrier_mapping_info] *)
           (* in the printing context.                    *)
           List.map
             (fun (ctype, (mapped_name, _)) -> (ctype, mapped_name))
             ctx.Species_gen_basics.scc_collections_carrier_mapping } in
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
     | Parsetree.Pr_expr expr -> generate_expr ctx expr
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
  let ppf = ctx.Species_gen_basics.scc_out_fmter in
  List.iter
    (fun ((param_ty_mod, param_ty_coll), (param_name, param_kind)) ->
      Format.fprintf ppf "@[<1>(%s :@ " param_name ;
      (match param_kind with
       | Species_gen_basics.CCMI_is parameter_expr ->
           generate_parameter_species_expr
             ~current_unit: ctx.Species_gen_basics.scc_current_unit
	     ppf parameter_expr
       | Species_gen_basics.CCMI_in_or_not_param ->
           if param_ty_mod <> ctx.Species_gen_basics.scc_current_unit then
             Format.fprintf ppf "%s." param_ty_mod ;
           Format.fprintf ppf "%s" param_ty_coll) ;
      Format.fprintf ppf ")@ @]")
    ctx.Species_gen_basics.scc_collections_carrier_mapping
;;



(* ************************************************************************* *)
(* species_compil_context -> Env.TypeInformation.species_description -> unit *)
(** {b Descr} : Generate the record type representing a species. This type
          contains a field per method. This type is named as the species.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let generate_record_type ctx species_descr =
  let out_fmter = ctx.Species_gen_basics.scc_out_fmter in
  let collections_carrier_mapping =
    ctx.Species_gen_basics.scc_collections_carrier_mapping in
  (* The header of the Coq record definition for the species. *)
  let (my_fname, my_species_name) =
    ctx.Species_gen_basics.scc_current_species in
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
     ((my_species_name ^ "_T"), Species_gen_basics.CCMI_in_or_not_param)) ::
    collections_carrier_mapping in
  (* Create the coq type print context. *)
  let (species_modname, species_vname) =
    ctx.Species_gen_basics.scc_current_species in
  let print_ctx = {
    Types.cpc_current_unit = ctx.Species_gen_basics.scc_current_unit ;
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
        generate_prop ctx prop ;
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
