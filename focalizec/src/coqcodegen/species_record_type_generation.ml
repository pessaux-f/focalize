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

(* $Id: species_record_type_generation.ml,v 1.6 2007-12-10 17:00:21 pessaux Exp $ *)



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



let generate_expr_ident_for_E_var ctx ~local_idents ident =
  let out_fmter = ctx.Species_gen_basics.scc_out_fmter in
  match ident.Parsetree.ast_desc with
   | Parsetree.EI_local vname ->
       (begin
       (* Thanks to the scoping pass, identifiers remaining "local" are *)
       (* either really let-bound in the context of the expression,     *)
       (* hence have a direct mapping between FoCaL and OCaml code, or  *)
       (* species "in"-parameters and then must be mapped onto the      *)
       (* lambda-lifted parameter introduced for it in the context of   *)
       (* the current species.                                          *)
       (* Be careful, because a recursive method called in its body is  *)
       (* scoped AS LOCAL ! And because recursive methods do not have   *)
       (* dependencies together, there is no way to recover the extra   *)
       (* parameters to apply to them in this configuration. Hence, to  *)
       (* avoid forgetting these extra arguments, we must use here the  *)
       (* information recorded in the context, i.e. the extra arguments *)
       (* of the recursive functions.                                   *)
       (* To check if a "smelling local" identifier is really local or  *)
       (* a "in"-parameter of the species, we use the same reasoning    *)
       (* that in [param_dep_analysis.ml]. Check justification over     *)
       (* there ! *)
       if (List.mem vname
             ctx.Species_gen_basics.scc_species_parameters_names) &&
         (not (List.mem vname local_idents)) then
         (begin
         (* In fact, a species "in"-parameter. This parameter was of the *)
         (* form "foo in C". Then it's naming scheme will be "_p_" +     *)
         (* the species parameter's name + the method's name that is     *)
         (* trivially the parameter's name again (because this last one  *)
         (* is computed as the "stuff" a dependency was found on, and in *)
         (* the case of a "in"-parameter, the dependency can only be on  *)
         (* the parameter's value itself, not on any method since there  *)
         (* is none !).                                                  *)
         Format.fprintf out_fmter "_p_%a_%a"
           Parsetree_utils.pp_vname_with_operators_expanded vname
           Parsetree_utils.pp_vname_with_operators_expanded vname
         end)
       else
         (begin
         (* Really a local identifier or a call to a recursive method. *)
         Format.fprintf out_fmter "%a"
           Parsetree_utils.pp_vname_with_operators_expanded vname ;
         (* Because this method can be recursive, we must apply it to *)
         (* its extra parameters if it has some.                      *)
         try
           let extra_args =
             List.assoc
               vname ctx.Species_gen_basics.scc_lambda_lift_params_mapping in
           List.iter (fun s -> Format.fprintf out_fmter "@ %s" s) extra_args
            with Not_found -> ()
         end)
       end)
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
        | Some coll_specifier ->
            (begin
            match coll_specifier with
             | Parsetree.Vname coll_name ->
                 (begin
                 (* Method call from a species that is not the current but  *)
                 (* is implicitely in the current compilation unit. May be  *)
                 (* either a paramater or a toplevel defined collection.    *)
                 if List.mem
                     coll_name
                     ctx.Species_gen_basics.scc_species_parameters_names then
                   (begin
                   (* It comes from a parameter. To retrieve the related *)
                   (* method name we build it the same way we built it   *)
                   (* while generating the extra Coq function's          *)
                   (* parameters due to depdencencies coming from the    *)
                   (* species parameter. I.e: "_p_", followed by the     *)
                   (* species parameter name, followed by "_", followed  *)
                   (* by the method's name.                              *)
                   let prefix =
                     "_p_" ^ (Parsetree_utils.name_of_vname coll_name) ^
                     "_" in
                   Format.fprintf out_fmter "%s%a"
                     prefix
                     Parsetree_utils.pp_vname_with_operators_expanded vname
                   end)
                 else
                   (begin
                   (* It comes from a toplevel stuff, hence not abstracted *)
                   (* by lambda-lifting.                                   *)
                   Format.fprintf out_fmter
                     "%a.effective_collection.%a.%a"
                     Parsetree_utils.pp_vname_with_operators_expanded coll_name
                     Parsetree_utils.pp_vname_with_operators_expanded coll_name
                     Parsetree_utils.pp_vname_with_operators_expanded vname
                   end)
                 end)
             | Parsetree.Qualified (module_name, coll_name) ->
                 (begin
                 if module_name = ctx.Species_gen_basics.scc_current_unit then
                   (begin
                   (* Exactly like when it is method call from a species that *)
                   (* is not the current but is implicitely in the current    *)
                   (* compilation unit : the call is performed to a method    *)
                   (* a species that is EXPLICITELY in the current            *)
                   (* compilation unit.                                       *)
                   if List.mem
                       coll_name
                       ctx.Species_gen_basics.scc_species_parameters_names then
                     (begin
                     let prefix =
                       "_p_" ^ (Parsetree_utils.name_of_vname coll_name) ^
                       "_" in
                     Format.fprintf out_fmter "%s%a"
                       prefix Parsetree_utils.pp_vname_with_operators_expanded
                       vname
                       end)
                   else
                     (begin
                     Format.fprintf out_fmter
                       "%a.effective_collection.%a.%a"
                       Parsetree_utils.pp_vname_with_operators_expanded
                       coll_name
                       Parsetree_utils.pp_vname_with_operators_expanded
                       coll_name
                       Parsetree_utils.pp_vname_with_operators_expanded
                       vname
                     end)
                   end)
                 else
                   (begin
                   (* The called method belongs to a species that is not    *)
                   (* ourselves and moreover belongs to another compilation *)
                   (* unit. May be a species from the toplevel of another   *)
                   (* FoCaL source file.                                    *)
                   failwith "generate_expr_ident_for_E_var foreign species's module TODO"
                   end)
                 end)
            end)
       end)
;;




let generate_constant ctx cst =
  match cst.Parsetree.ast_desc with
   | Parsetree.C_int str ->
       (* Integers are directly mapped in Coq. *)
       Format.fprintf ctx.Species_gen_basics.scc_out_fmter "%s" str
   | Parsetree.C_float _str ->
       (* [Unsure] *)
       Format.fprintf ctx.Species_gen_basics.scc_out_fmter "C_float"
   | Parsetree.C_bool str ->
       (* [true] maps on Coq "true". [false] maps on Coq "false". *)
       Format.fprintf ctx.Species_gen_basics.scc_out_fmter "%s" str
   | Parsetree.C_string _str ->
       (* [Unsure] *)
       Format.fprintf ctx.Species_gen_basics.scc_out_fmter "C_string"
   | Parsetree.C_char _c ->
       (* [Unsure] *)
       Format.fprintf ctx.Species_gen_basics.scc_out_fmter "C_char"
;;



(** Quasiment identique à OCaml. *)
let generate_constructor_ident_for_method_generator ctx (*env*) cstr_expr =
(*
  try
    let mapping_info =
      Env.MlGenEnv.find_constructor
        ~loc: cstr_expr.Parsetree.ast_loc
        ~current_unit: ctx.Misc_ml_generation.rcc_current_unit
        cstr_expr env in
    let (_, ocaml_binding) =
      try
        List.find
          (function
            | (Parsetree.EL_Caml, _) -> true
            | (Parsetree.EL_Coq, _)
            | ((Parsetree.EL_external _), _) -> false)
          mapping_info
      with Not_found ->
        (* No OCam mapping found. *)
        raise
          (Externals_ml_generation.No_external_constructor_caml_def
             cstr_expr) in
    (* Now directly generate the name the constructor is mapped onto. *)
    Format.fprintf ctx.Misc_ml_generation.rcc_out_fmter "%s" ocaml_binding
  with
  | Env.Unbound_constructor (_, _) ->
*)
      (begin
      match cstr_expr.Parsetree.ast_desc with
       | Parsetree.CI (Parsetree.Vname name) ->
           Format.fprintf ctx.Species_gen_basics.scc_out_fmter "%a"
             Parsetree_utils.pp_vname_with_operators_expanded name
       | Parsetree.CI (Parsetree.Qualified (fname, name)) ->
           (* If the constructor belongs to the current      *)
           (* compilation unit then one must not qualify it. *)
           if fname <> ctx.Species_gen_basics.scc_current_unit then
             Format.fprintf ctx.Species_gen_basics.scc_out_fmter "%s.%a"
               fname          (* No module name capitalization in Coq. *)
               Parsetree_utils.pp_vname_with_operators_expanded name
           else
             Format.fprintf ctx.Species_gen_basics.scc_out_fmter "%a"
               Parsetree_utils.pp_vname_with_operators_expanded name
      end)
;;



(** Identique à OCaml. *)
let generate_pattern ctx (*env*) pattern =
  let out_fmter = ctx.Species_gen_basics.scc_out_fmter in
  let rec rec_gen_pat pat =
    match pat.Parsetree.ast_desc with
     | Parsetree.P_const constant -> generate_constant ctx constant
     | Parsetree.P_var name ->
         Format.fprintf out_fmter "%a"
           Parsetree_utils.pp_vname_with_operators_expanded name
     | Parsetree.P_as (p, name) ->
         Format.fprintf out_fmter "(" ;
         rec_gen_pat p ;
         Format.fprintf out_fmter "as@ %a)"
           Parsetree_utils.pp_vname_with_operators_expanded name
     | Parsetree.P_wild -> Format.fprintf out_fmter "_"
     | Parsetree.P_constr (ident, pats) ->
         (begin
         generate_constructor_ident_for_method_generator ctx (*env*) ident ;
         (* Discriminate on the umber of arguments *)
         (* to know if parens are needed.          *)
         match pats with
          | [] -> ()
          | _ ->
              Format.fprintf out_fmter " (" ;
              rec_generate_pats_list pats ;
              Format.fprintf out_fmter ")"
         end)
     | Parsetree.P_record _labs_pats ->
         Format.eprintf "generate_pattern P_record TODO@."
     | Parsetree.P_tuple pats ->
         Format.fprintf out_fmter "(@[<1>" ;
         rec_generate_pats_list pats ;
         Format.fprintf out_fmter ")@]"
     | Parsetree.P_paren p ->
         Format.fprintf out_fmter "(@[<1>" ;
         rec_gen_pat p ;
         Format.fprintf out_fmter ")@]"

(** Identique à OCaml. *)
  and rec_generate_pats_list = function
    | [] -> ()
    | [last] -> rec_gen_pat last
    | h :: q ->
        rec_gen_pat h ;
        Format.fprintf out_fmter ",@ " ;
        rec_generate_pats_list q in
  (* ********************** *)
  (* Now, let's do the job. *)
  rec_gen_pat pattern
;;




(*
and let_def_compile ctx ~local_idents env let_def opt_bound_schemes =
  let out_fmter = ctx.Misc_ml_generation.rcc_out_fmter in
  (* Generates the binder ("rec" or non-"rec"). *)
  Format.fprintf out_fmter "@[<2>let%s@ "
    (match let_def.Parsetree.ast_desc.Parsetree.ld_rec with
     | Parsetree.RF_no_rec -> ""
     | Parsetree.RF_rec -> " rec"   (* NON-breakable space in front. *)) ;
  (* Now generate each bound definition. *)
  (* Because we are in a toplevel [let_def], we have no species       *)
  (* parameter in the scope. Hence the collections carrier mapping is *)
  (* empty.                                                           *)
  (match (let_def.Parsetree.ast_desc.Parsetree.ld_bindings,
          opt_bound_schemes) with
   | ([], []) ->
       (* The let construct should always at least bind one identifier ! *)
       assert false
   | ([one_bnd], [one_scheme]) ->
       let_binding_compile ctx (*~local_idents env*) [] one_bnd one_scheme
   | ((first_bnd :: next_bnds), (first_scheme :: next_schemes)) ->
       let_binding_compile ctx (*~local_idents env*) [] first_bnd first_scheme ;
       List.iter2
         (fun binding scheme ->
           Format.fprintf out_fmter "@]@\n@[<2>and " ;
           let_binding_compile ctx (*~local_idents env*) [] binding scheme)
         next_bnds
         next_schemes
   | (_, _) ->
       (* Because the FoCaL has been parsed and typechecked, we must never    *)
       (* have a different number of bound identifiers and bound-identifiers' *)
       (* type schemes. If this arise, then we have a serious bug somewhere.  *)
       assert false) ;
  Format.fprintf out_fmter "@]"
*)




let generate_expr ctx ~local_idents initial_expression =
  let out_fmter = ctx.Species_gen_basics.scc_out_fmter in
  (* Create the coq type print context. *)
  let (species_modname, species_vname) =
    ctx.Species_gen_basics.scc_current_species in
  let print_ctx = {
    Types.cpc_current_unit = ctx.Species_gen_basics.scc_current_unit ;
    Types.cpc_current_species =
      Some (species_modname, (Parsetree_utils.name_of_vname species_vname)) ;
    Types.cpc_collections_carrier_mapping =
      (* Throw the [collection_carrier_mapping_info] *)
      (* in the printing context.                    *)
      List.map
        (fun (ctype, (mapped_name, _)) -> (ctype, mapped_name))
        ctx.Species_gen_basics.scc_collections_carrier_mapping } in

  let rec rec_generate_expr loc_idents expression = 
    (* Now, dissecate the expression core. *)
    match expression.Parsetree.ast_desc with
     | Parsetree.E_self ->
         (* [Unsure] D'ailleurs, est-ce possible en fait ? *)
         failwith "generate_expr E_self TODO"
     | Parsetree.E_const cst -> generate_constant ctx cst
     | Parsetree.E_fun (vnames, body) ->
         (* Get the type of the function. *)
         let fun_ty =
           (match expression.Parsetree.ast_type with
            | Parsetree.ANTI_none | Parsetree.ANTI_non_relevant
            | Parsetree.ANTI_scheme _ -> assert false
            | Parsetree.ANTI_type t -> t) in
         Format.fprintf out_fmter "@[<2>fun " ;
         (* Now, print each parameter with it's type until we arrive to *)
         (* the return type of the function. DO NOT fold_right !        *)
         ignore
           (List.fold_left
              (fun accu_ty arg_name ->
                let arg_ty = Types.extract_fun_ty_arg accu_ty in
                let res_ty = Types.extract_fun_ty_result accu_ty in
                Format.fprintf out_fmter "(%a :@ %a)@ "
                  Parsetree_utils.pp_vname_with_operators_expanded arg_name
                  (Types.pp_type_simple_to_coq print_ctx) arg_ty ;
                (* Return the remainder of the type to continue. *)
                res_ty)
              fun_ty
              vnames) ;
         Format.fprintf out_fmter "=>@ " ;
         rec_generate_expr loc_idents body ;
         Format.fprintf out_fmter "@]" ;
     | Parsetree.E_var ident ->
         generate_expr_ident_for_E_var ctx ~local_idents: loc_idents ident
     | Parsetree.E_app (func_expr, args) ->
         Format.fprintf out_fmter "@[<2>(" ;
         rec_generate_expr loc_idents func_expr ;
         Format.fprintf out_fmter "@ " ;
         rec_generate_exprs_list  ~comma: false loc_idents args ;
         Format.fprintf out_fmter ")@]"
     | Parsetree.E_constr (_cstr_ident, _args) ->
         (* [Unsure] *)
         Format.fprintf out_fmter "E_constr"
     | Parsetree.E_match (expr, pats_exprs) ->
         (begin
         Format.fprintf out_fmter "@[<1>match " ;
         rec_generate_expr loc_idents expr ;
         Format.fprintf out_fmter " with" ;
         List.iter
           (fun (pattern, expr) ->
             (* My indentation style: indent of 4 between *)
             (* the pattern and its related processing.   *)
             Format.fprintf out_fmter "@\n@[<4>| " ;
             generate_pattern ctx (*env*) pattern ;
             Format.fprintf out_fmter " =>@\n" ;
             (* Here, each name of the pattern may mask a "in"-parameter. *)
             let loc_idents' =
               (Parsetree_utils.get_local_idents_from_pattern pattern) @
               loc_idents in
             rec_generate_expr loc_idents' expr ;
             Format.fprintf out_fmter "@]")
           pats_exprs ;
         Format.fprintf out_fmter "@\nend@]"
         end)
     | Parsetree.E_if (expr1, expr2, expr3) ->
         Format.fprintf out_fmter "@[<2>if@ " ;
         rec_generate_expr loc_idents expr1 ;
         Format.fprintf out_fmter "@ @[<2>then@ @]" ;
         rec_generate_expr loc_idents expr2 ;
         Format.fprintf out_fmter "@ @[<2>else@ @]" ;
         rec_generate_expr loc_idents expr3 ;
         Format.fprintf out_fmter "@]"
     | Parsetree.E_let (_let_def, _in_expr) ->
         (* [Unsure] *)
         Format.fprintf out_fmter "E_let"
     | Parsetree.E_record _labs_exprs ->
         (* [Unsure] *)
         Format.fprintf out_fmter "E_record"
     | Parsetree.E_record_access (_expr, _label) ->
         Format.fprintf out_fmter "E_record_access"
     | Parsetree.E_record_with (_expr, _labels_exprs) ->
         Format.fprintf out_fmter "E_record_with"
     | Parsetree.E_tuple exprs ->
         (begin
         match exprs with
          | [] -> assert false
          | [one] -> rec_generate_expr loc_idents one
          | _ ->
              Format.fprintf out_fmter "@[<1>(" ;
              rec_generate_exprs_list ~comma: true loc_idents exprs ;
              Format.fprintf out_fmter ")@]"
         end)
     | Parsetree.E_external _ext_expr ->
         (* [Unsure] *)
         Format.fprintf out_fmter "E_external"
     | Parsetree.E_paren expr ->
         Format.fprintf out_fmter "@[<1>(" ;
         rec_generate_expr loc_idents expr ;
         Format.fprintf out_fmter ")@]"


  and rec_generate_exprs_list  ~comma loc_idents = function
    | [] -> ()
    | [last] -> rec_generate_expr loc_idents last
    | h :: q ->
        rec_generate_expr loc_idents h ;
        if comma then Format.fprintf out_fmter ",@ "
        else Format.fprintf out_fmter "@ " ;
        rec_generate_exprs_list ~comma loc_idents q in


  (* ************************************************ *)
  (* Now, let's really do the job of [generate_expr]. *)
  rec_generate_expr local_idents initial_expression
;;



let generate_prop ctx ~local_idents initial_proposition =
  let out_fmter = ctx.Species_gen_basics.scc_out_fmter in
  (* Create the coq type print context. *)
  let (species_modname, species_vname) =
    ctx.Species_gen_basics.scc_current_species in
  let print_ctx = {
    Types.cpc_current_unit = ctx.Species_gen_basics.scc_current_unit ;
    Types.cpc_current_species =
      Some (species_modname, (Parsetree_utils.name_of_vname species_vname)) ;
    Types.cpc_collections_carrier_mapping =
      (* Throw the [collection_carrier_mapping_info] *)
      (* in the printing context.                    *)
      List.map
        (fun (ctype, (mapped_name, _)) -> (ctype, mapped_name))
        ctx.Species_gen_basics.scc_collections_carrier_mapping } in
  let rec rec_generate_prop loc_idents proposition =
    match proposition.Parsetree.ast_desc with
     | Parsetree.Pr_forall (vnames, ty_expr, prop)
     | Parsetree.Pr_exists (vnames, ty_expr, prop) ->
         (begin
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
         (* Here, the bound variables name may mask a "in"-parameter. *)
         let loc_idents' = vnames @ loc_idents in
         rec_generate_prop loc_idents' prop ;
         Format.fprintf out_fmter "@]"
         end)
     | Parsetree.Pr_imply (prop1, prop2) ->
         Format.fprintf out_fmter "@[<2>" ;
         rec_generate_prop loc_idents prop1 ;
         Format.fprintf out_fmter " ->@ " ;
         rec_generate_prop loc_idents prop2 ;
         Format.fprintf out_fmter "@]"
     | Parsetree.Pr_or  (prop1, prop2) ->
         Format.fprintf out_fmter "@[<2>" ;
         rec_generate_prop loc_idents prop1 ;
         Format.fprintf out_fmter " \\/@ " ;
         rec_generate_prop loc_idents prop2 ;
         Format.fprintf out_fmter "@]"
     | Parsetree.Pr_and  (prop1, prop2) ->
         Format.fprintf out_fmter "@[<2>" ;
         rec_generate_prop loc_idents prop1 ;
         Format.fprintf out_fmter " /\\@ " ;
         rec_generate_prop loc_idents prop2 ;
         Format.fprintf out_fmter "@]"
     | Parsetree.Pr_equiv (prop1, prop2) ->
         Format.fprintf out_fmter "@[<2>" ;
         rec_generate_prop loc_idents prop1 ;
         Format.fprintf out_fmter " <->@ " ;
         rec_generate_prop loc_idents prop2 ;
         Format.fprintf out_fmter "@]"
     | Parsetree.Pr_not prop ->
         Format.fprintf out_fmter "@[<2>" ;
         Format.fprintf out_fmter "~" ;
         rec_generate_prop loc_idents prop ;
         Format.fprintf out_fmter "@]"
     | Parsetree.Pr_expr expr ->
         (* The wrapper surrounding the expression by Coq's *)
         (* "Is_true" if the expression's type is [bool].   *)
         let is_bool =
           (match expr.Parsetree.ast_type with
            | Parsetree.ANTI_type ty -> Types.is_bool_type ty
            | Parsetree.ANTI_none
            | Parsetree.ANTI_non_relevant
            | Parsetree.ANTI_scheme _ ->
                (* Note that expression never has a *)
                (* type scheme, but only a type.    *)
                assert false) in
         if is_bool then Format.fprintf out_fmter "@[<2>Is_true (" ;
         generate_expr ctx ~local_idents: loc_idents expr ;
         (* The end of the wrapper surrounding  *)
         (* the expression if it has type bool. *)
         if is_bool then Format.fprintf out_fmter ")@]"
     | Parsetree.Pr_paren prop ->
         Format.fprintf out_fmter "@[<1>(" ;
         rec_generate_prop loc_idents prop ;
         Format.fprintf out_fmter ")@]" in

  (* ************************************************ *)
  (* Now, let's really do the job of [generate_prop]. *)
  rec_generate_prop local_idents initial_proposition
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
      Next come the extra parameters coming from the methods we depend
      on.
    {b Rem} : Not exported outside this module.                           *)
(* ********************************************************************** *)
let generate_record_type_parameters ctx species_fields =
  let ppf = ctx.Species_gen_basics.scc_out_fmter in
  (* We first abstract the species parameters. *)
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
    ctx.Species_gen_basics.scc_collections_carrier_mapping ;
  (* Now, we will find the methods of the parameters we decl-depend  *)
  (* on in the Coq type expressions. Such dependencies can only      *)
  (* appear through properties and theorems bodies.                  *)
  let species_parameters_names =
    ctx.Species_gen_basics.scc_species_parameters_names in
  let (species_mod, species_vname) =
    ctx.Species_gen_basics.scc_current_species in
    let print_ctx = {
    Types.cpc_current_unit = ctx.Species_gen_basics.scc_current_unit ;
    Types.cpc_current_species =
      Some  (species_mod, (Parsetree_utils.name_of_vname species_vname)) ;
    Types.cpc_collections_carrier_mapping =
      (* Throw the [collection_carrier_mapping_info] *)
      (* in the printing context.                    *)
      List.map
        (fun (ctype, (mapped_name, _)) -> (ctype, mapped_name))
        ctx.Species_gen_basics.scc_collections_carrier_mapping } in
    (* We first build the lists of dependent methods for each *)
    (* property and theorem fields.                           *)
    let deps_lists_for_fields =
      List.map
        (function
          | Env.TypeInformation.SF_sig (_, _, _)
          | Env.TypeInformation.SF_let (_, _, _, _, _)
          | Env.TypeInformation.SF_let_rec _-> []
          | Env.TypeInformation.SF_theorem (_, _, _, prop, _)
          | Env.TypeInformation.SF_property (_, _, _, prop) ->
              (* Get the list of the methods from the species parameters the  *)
              (* current prop/theo depends on. Do not [fold_left] to keep the *)
              (* extra parameters in the same order than the species          *)
              (* parameters order. I.e. for a species [Foo (A ..., B) ...]    *)
              (* we want to have the extra parameters due to lambda-lifting   *)
              (* in the Coq record type ordered such as those coming from [A] *)
              (* are first, then come those from [B].                         *)
              let dependencies_from_params =
                List.fold_right
                  (fun species_param_name accu ->
                    let meths_from_param =
                      Param_dep_analysis.param_deps_prop
                        ~current_species:
                        ctx.Species_gen_basics.scc_current_species
                        species_param_name prop in
                    (* Return a couple binding the species parameter's name *)
                    (* with the methods of it we found as required for the  *)
                    (* current property/theorem.                            *)
                    (species_param_name, meths_from_param) :: accu)
                  species_parameters_names
                  [] in
              dependencies_from_params)
        species_fields in
    (* Now, we must flatten them and make sure that each record type's  *)
    (* parameter induced by a dependent method from a species parameter *)
    (* is uniquely printed to avoid having several times the same       *)
    (* parameter name in the record type in case where several fields   *)
    (* depend on the same method.                                       *)
    let flat_deps_for_fields = List.flatten deps_lists_for_fields in
    let seen = ref [] in
    List.iter
      (fun (species_param_name, meths) ->
        (* Each abstracted method will be named like "_p_", followed by *)
        (* the species parameter name, followed by "_", followed by the *)
        (* method's name.                                               *)
        let prefix =
          "_p_" ^ (Parsetree_utils.name_of_vname species_param_name) ^
          "_" in
        Parsetree_utils.DepNameSet.iter
          (fun (meth, ty) ->
            if not (List.mem (species_param_name, meth) !seen) then
              (begin
              seen := (species_param_name, meth) :: !seen ;
              let llift_name =
                prefix ^
                (Parsetree_utils.vname_as_string_with_operators_expanded
                   meth) in
              Format.fprintf ppf "(%s : %a)@ "
                llift_name (Types.pp_type_simple_to_coq print_ctx) ty
              end))
          meths)
      flat_deps_for_fields
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
  (* Generate the record parameters mapping the species  *)
  (* parameters and the methods from them we depend on ! *)
  generate_record_type_parameters
    ctx species_descr.Env.TypeInformation.spe_sig_methods ;
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
        (* No local idents in the context because we just enter the scope *)
        (* of a species fields and so we are not under a core expression. *)
        generate_prop ctx ~local_idents: [] prop ;
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
