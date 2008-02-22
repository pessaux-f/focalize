(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            Fran�ois Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: species_record_type_generation.ml,v 1.17 2008-02-22 18:06:29 pessaux Exp $ *)



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



let generate_expr_ident_for_E_var ctx ~local_idents ~self_as ident =
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
            (* Method call from the current species. *)
            match self_as with
             | Types.CSR_abst ->
                 Format.fprintf out_fmter "abst_%a"
                   Parsetree_utils.pp_vname_with_operators_expanded vname
             | Types.CSR_species ->
                 Format.fprintf out_fmter "%a_%a"
                   Parsetree_utils.pp_vname_with_operators_expanded
                   (snd ctx.Species_gen_basics.scc_current_species)
                   Parsetree_utils.pp_vname_with_operators_expanded vname
             | Types.CSR_self ->
                 Format.fprintf out_fmter "self_%a"
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




let generate_constructor_ident_for_method_generator ctx env cstr_expr =
  let mapping_info =
    Env.CoqGenEnv.find_constructor
      ~loc: cstr_expr.Parsetree.ast_loc
      ~current_unit: ctx.Species_gen_basics.scc_current_unit cstr_expr env in
(* [Unsure]
    let (_, coq_binding) =
      try
        List.find
          (function
            | (Parsetree.EL_Coq, _) -> true
            | (Parsetree.EL_Caml, _)
            | ((Parsetree.EL_external _), _) -> false)
          mapping_info
      with Not_found ->
        (* No Coq mapping found. *)
        raise
          (Externals_ml_generation.No_external_constructor_coq_def
             cstr_expr) in
    (* Now directly generate the name the constructor is mapped onto. *)
    Format.fprintf ctx.Species_gen_basics.scc_out_fmter "%s" ocaml_binding
  with
  | Env.Unbound_constructor (_, _) -> assert false
*)
  (match cstr_expr.Parsetree.ast_desc with
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
           Parsetree_utils.pp_vname_with_operators_expanded name) ;
  (* Returns the number of "_" that must be printed after the constructor. *)
  mapping_info.Env.CoqGenInformation.cmi_num_polymorphics_extra_args
;;



let generate_pattern ctx env pattern =
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
         let nb_poly_args =
           generate_constructor_ident_for_method_generator ctx env ident in
         (* Add the "_"'s due to polymorphism of the constructor. *)
         for i = 0 to nb_poly_args - 1 do
           Format.fprintf out_fmter "@ _"
           done ;
         rec_generate_pats_list pats
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



let rec let_binding_compile ctx ~local_idents ~self_as ~is_rec env bd =
  let out_fmter = ctx.Species_gen_basics.scc_out_fmter in
  (* Generate the bound name. *)
  Format.fprintf out_fmter "%a"
    Parsetree_utils.pp_vname_with_operators_expanded
    bd.Parsetree.ast_desc.Parsetree.b_name ;
  (* Generate the parameters if some, with their type constraints. *)
  let params_names = List.map fst bd.Parsetree.ast_desc.Parsetree.b_params in
  (* Recover the type scheme of the bound ident. *)
  let def_scheme =
    (match bd.Parsetree.ast_type with
     | Parsetree.ANTI_none | Parsetree.ANTI_non_relevant
     | Parsetree.ANTI_type _ -> assert false
     | Parsetree.ANTI_scheme s -> s) in
  let (params_with_type, result_ty, generalized_instanciated_vars) =
    Misc_ml_generation.bind_parameters_to_types_from_type_scheme
       (Some def_scheme) params_names in
  (* Build the print context. *)
  let print_ctx = {
    Types.cpc_current_unit = ctx.Species_gen_basics.scc_current_unit ;
    Types.cpc_current_species =
      Some
        (Parsetree_utils.type_coll_from_qualified_species
           ctx.Species_gen_basics.scc_current_species) ;
    Types.cpc_collections_carrier_mapping =
      (* Throw the [collection_carrier_mapping_info] *)
      (* in the printing context.                    *)
      List.map
        (fun (ctype, (mapped_name, _)) -> (ctype, mapped_name))
        ctx.Species_gen_basics.scc_collections_carrier_mapping } in
  (* We are printing each parameter's type. These types in fact belong *)
  (* to a same type scheme. Hence, they may share variables together.  *)
  (* For this reason, we first purge the printing variable mapping and *)
  (* after, activate its persistence between each parameter printing.  *)
  Types.purge_type_simple_to_coq_variable_mapping () ;
  (* If the original scheme is polymorphic, then we must ad extra Coq  *)
  (* parameters of type "Set" for each of the generalized variables.   *)
  (* Hence, printing the variables used to instanciate the polymorphic *)
  (* ones in front of the function, they will appear and moreover they *)
  (* will be "tagged" as "seen" in the variable mapping. Hence, when   *)
  (* we will print the arguments having these variables as type, the   *)
  (* same variable name will be used, hence establishing the correct   *)
  (* link between the type of the variable and the type variable of    *)
  (* the function argument's type.                                     *)
  List.iter
    (fun var ->
       Format.fprintf out_fmter "@ (%a : Set)"
        (Types.pp_type_simple_to_coq print_ctx ~reuse_mapping: true ~self_as)
        var)
    generalized_instanciated_vars ;
  (* Record NOW in the environment the number of extra arguments  *)
  (* due to polymorphism the current bound ident has in case of   *)
  (* recursive definition. Otherwise, it will only be done later. *)
  let nb_polymorphic_args = List.length generalized_instanciated_vars in
  let env' =
    if is_rec then
      Env.CoqGenEnv.add_value
        bd.Parsetree.ast_desc.Parsetree.b_name nb_polymorphic_args env
    else env in
  (* Now, generate each of the real function's parameter with its type. *)
  List.iter
    (fun (param_vname, pot_param_ty) ->
      match pot_param_ty with
       | Some param_ty ->
           Format.fprintf out_fmter "@ (%a : %a)"
             Parsetree_utils.pp_vname_with_operators_expanded param_vname
             (Types.pp_type_simple_to_coq
                print_ctx ~reuse_mapping: true ~self_as)
             param_ty
       | None ->
           (* Because we provided a type scheme to the function         *)
           (* [bind_parameters_to_types_from_type_scheme], MUST get one *)
           (* type for each parameter name !                            *)
           assert false)
    params_with_type ;
  (* If the definition is a recursive function, then one must exhibit  *)
  (* one decreasing argument. Because we don't know which one is, just *)
  (* take one at random... For instance, the first one...              *)
  (* If there is no parameter, then the binding is not a function and  *)
  (* we do not need to exhibit any decreasing argument.                *)
  if is_rec then
    (begin
    match params_with_type with
     | (param_vname, _) :: _ ->
         Format.fprintf out_fmter "@ {struct %a}"
           Parsetree_utils.pp_vname_with_operators_expanded param_vname
     | _ -> ()
    end) ;
  (* Now, print the result type of the "let". *)
  (match result_ty with
   | None ->
       (* Because we provided a type scheme to the function         *)
       (* [bind_parameters_to_types_from_type_scheme], MUST get one *)
       (* type for the result value of the "let".                   *)
       assert false
   | Some t ->
       Format.fprintf out_fmter "@ :@ %a"
         (Types.pp_type_simple_to_coq print_ctx ~reuse_mapping: true ~self_as)
         t) ;
  (* Now we don't need anymore the sharing. Hence, clean it. This should not *)
  (* be useful because the other guys usign printing should manage this      *)
  (* themselves (as we did just above by cleaning before activating the      *)
  (* sharing), but anyway, it is safer an not costly. So...                  *)
  Types.purge_type_simple_to_coq_variable_mapping () ;
  (* Output now the ":=" sign ending the Coq function's "header".     *)
  (* With a NON-breakable space before to prevent uggly hyphenation ! *)
  Format.fprintf out_fmter " :=@ " ;
  (* Here, each parameter name of the binding may mask a "in"-parameter. *)
  let local_idents' = params_names @ local_idents in
  (* Now, let's generate the bound body. *)
  generate_expr ctx ~local_idents: local_idents' ~self_as env'
    bd.Parsetree.ast_desc.Parsetree.b_body ;
  (* Finally, we record, even if it was already done in [env'] the number *)
  (* of extra arguments due to polymorphism the current bound identifier  *)
  (* has.                                                                 *)
  Env.CoqGenEnv.add_value
    bd.Parsetree.ast_desc.Parsetree.b_name nb_polymorphic_args env




and let_in_def_compile ctx ~local_idents ~self_as env let_def =
  let out_fmter = ctx.Species_gen_basics.scc_out_fmter in
  let is_rec =
    (match let_def.Parsetree.ast_desc.Parsetree.ld_rec with
     | Parsetree.RF_no_rec -> false
     | Parsetree.RF_rec -> true) in
  (* Generates the binder ("fix" or non-"fix"). *)
  Format.fprintf out_fmter "@[<2>let%s@ "
    (match is_rec with
     | false -> ""
     | true ->
         (* [Unsure] We don't known now how to compile several local mutually *)
         (* recursive functions. *)
         if (List.length let_def.Parsetree.ast_desc.Parsetree.ld_bindings) > 1
         then failwith "TODO: local mutual recursive functions." ;
         " fix"   (* NON-breakable space in front. *)) ;
  (* Now generate each bound definition. *)
  let env' =
    (match let_def.Parsetree.ast_desc.Parsetree.ld_bindings with
     | [] ->
         (* The "let" construct should always at least bind one identifier ! *)
         assert false
     | [one_bnd] ->
         let_binding_compile ctx ~local_idents ~self_as ~is_rec env one_bnd
     | first_bnd :: next_bnds ->
         let accu_env =
           ref
             (let_binding_compile
                ctx ~local_idents ~self_as ~is_rec env first_bnd) in
         List.iter
           (fun binding ->
             (* We transform "let and" non recursive functions *)
             (* into several "let in" definitions.             *)
             Format.fprintf out_fmter "@ in@]@\n@[<2>let " ;
             accu_env :=
               let_binding_compile
                 ctx ~local_idents ~self_as ~is_rec !accu_env binding)
           next_bnds ;
           !accu_env) in
  Format.fprintf out_fmter "@]" ;
  env'



and generate_expr ctx ~local_idents ~self_as initial_env
    initial_expression =
  let out_fmter = ctx.Species_gen_basics.scc_out_fmter in
  (* Create the coq type print context. *)
  let print_ctx = {
    Types.cpc_current_unit = ctx.Species_gen_basics.scc_current_unit ;
    Types.cpc_current_species =
      Some
        (Parsetree_utils.type_coll_from_qualified_species
           ctx.Species_gen_basics.scc_current_species) ;
    Types.cpc_collections_carrier_mapping =
      (* Throw the [collection_carrier_mapping_info] *)
      (* in the printing context.                    *)
      List.map
        (fun (ctype, (mapped_name, _)) -> (ctype, mapped_name))
        ctx.Species_gen_basics.scc_collections_carrier_mapping } in

  let rec rec_generate_expr loc_idents env expression =
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
                  (Types.pp_type_simple_to_coq
                     print_ctx ~reuse_mapping: false ~self_as)
                  arg_ty ;
                (* Return the remainder of the type to continue. *)
                res_ty)
              fun_ty
              vnames) ;
         Format.fprintf out_fmter "=>@ " ;
         rec_generate_expr loc_idents env body ;
         Format.fprintf out_fmter "@]" ;
     | Parsetree.E_var ident ->
         (begin
         generate_expr_ident_for_E_var
           ctx ~local_idents: loc_idents ~self_as ident ;
         (* Now, add the extra "_"'s if the identifier is polymorphic. *)
         try
           let nb_polymorphic_args =
             Env.CoqGenEnv.find_value
               ~loc: ident.Parsetree.ast_loc
               ~current_unit: ctx.Species_gen_basics.scc_current_unit
               ident env in
           for i = 0 to nb_polymorphic_args - 1 do
             Format.fprintf out_fmter "@ _"
           done
         with
           (* If the identifier was not found, then it was may be a local  *)
           (* identifier bound by a pattern. Then we can safely ignore it. *)
           Env.Unbound_identifier (_, _) -> ()
         end)
     | Parsetree.E_app (func_expr, args) ->
         Format.fprintf out_fmter "@[<2>(" ;
         rec_generate_expr loc_idents env func_expr ;
         Format.fprintf out_fmter "@ " ;
         rec_generate_exprs_list ~comma: false loc_idents env args ;
         Format.fprintf out_fmter ")@]"
     | Parsetree.E_constr (_cstr_ident, _args) ->
         (* [Unsure] *)
         Format.fprintf out_fmter "E_constr"
     | Parsetree.E_match (expr, pats_exprs) ->
         (begin
         Format.fprintf out_fmter "@[<1>match " ;
         rec_generate_expr loc_idents env expr ;
         Format.fprintf out_fmter " with" ;
         List.iter
           (fun (pattern, expr) ->
             (* My indentation style: indent of 4 between *)
             (* the pattern and its related processing.   *)
             Format.fprintf out_fmter "@\n@[<4>| " ;
             generate_pattern ctx env pattern ;
             Format.fprintf out_fmter " =>@\n" ;
             (* Here, each name of the pattern may mask a "in"-parameter. *)
             let loc_idents' =
               (Parsetree_utils.get_local_idents_from_pattern pattern) @
               loc_idents in
             rec_generate_expr loc_idents' env expr ;
             Format.fprintf out_fmter "@]")
           pats_exprs ;
         Format.fprintf out_fmter "@\nend@]"
         end)
     | Parsetree.E_if (expr1, expr2, expr3) ->
         Format.fprintf out_fmter "@[<2>if@ " ;
         rec_generate_expr loc_idents env expr1 ;
         Format.fprintf out_fmter "@ @[<2>then@ @]" ;
         rec_generate_expr loc_idents env expr2 ;
         Format.fprintf out_fmter "@ @[<2>else@ @]" ;
         rec_generate_expr loc_idents env expr3 ;
         Format.fprintf out_fmter "@]"
     | Parsetree.E_let (let_def, in_expr) ->
         let env' = let_in_def_compile ctx ~local_idents ~self_as env let_def in
         Format.fprintf out_fmter "@ in@\n" ;
         rec_generate_expr loc_idents env' in_expr
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
          | [one] -> rec_generate_expr loc_idents env one
          | _ ->
              Format.fprintf out_fmter "@[<1>(" ;
              rec_generate_exprs_list ~comma: true loc_idents env exprs ;
              Format.fprintf out_fmter ")@]"
         end)
     | Parsetree.E_external _ext_expr ->
         (* [Unsure] *)
         Format.fprintf out_fmter "E_external"
     | Parsetree.E_paren expr ->
         Format.fprintf out_fmter "@[<1>(" ;
         rec_generate_expr loc_idents env expr ;
         Format.fprintf out_fmter ")@]"



  and rec_generate_exprs_list ~comma loc_idents env = function
    | [] -> ()
    | [last] -> rec_generate_expr loc_idents env last
    | h :: q ->
        rec_generate_expr loc_idents env h ;
        if comma then Format.fprintf out_fmter ",@ "
        else Format.fprintf out_fmter "@ " ;
        rec_generate_exprs_list ~comma loc_idents env q in


  (* ************************************************ *)
  (* Now, let's really do the job of [generate_expr]. *)
  rec_generate_expr local_idents initial_env initial_expression
;;



let generate_prop ctx ~local_idents ~self_as initial_env
    initial_proposition =
  let out_fmter = ctx.Species_gen_basics.scc_out_fmter in
  (* Create the coq type print context. *)
  let print_ctx = {
    Types.cpc_current_unit = ctx.Species_gen_basics.scc_current_unit ;
    Types.cpc_current_species =
      Some
        (Parsetree_utils.type_coll_from_qualified_species
           ctx.Species_gen_basics.scc_current_species) ;
    Types.cpc_collections_carrier_mapping =
      (* Throw the [collection_carrier_mapping_info] *)
      (* in the printing context.                    *)
      List.map
        (fun (ctype, (mapped_name, _)) -> (ctype, mapped_name))
        ctx.Species_gen_basics.scc_collections_carrier_mapping } in
  let rec rec_generate_prop loc_idents env proposition =
    match proposition.Parsetree.ast_desc with
     | Parsetree.Pr_forall (vnames, ty_expr, prop)
     | Parsetree.Pr_exists (vnames, ty_expr, prop) ->
         (begin
         (* Recover the *scheme* annotating the type expression. *)
         let scheme =
           (match ty_expr.Parsetree.ast_type with
            | Parsetree.ANTI_none
            | Parsetree.ANTI_non_relevant
            | Parsetree.ANTI_type _ -> assert false
            | Parsetree.ANTI_scheme s -> s) in
         let (ty, generalized_instanciated_vars) =
           Types.specialize_n_show_instanciated_generalized_vars scheme in
         let binder =
           (match proposition.Parsetree.ast_desc with
            | Parsetree.Pr_forall (_, _, _) -> "forall"
            | Parsetree.Pr_exists (_, _, _) -> "exists"
            | _ -> assert false) in
         (* The header containing the binder... *)
         Format.fprintf out_fmter "@[<2>%s@ " binder ;
         (* IMHO : not really useful, but... doesn't hurt... *)
         Types.purge_type_simple_to_coq_variable_mapping () ;
         (* Now, print the polymorphic extra args. We use the same trick *)
         (* than in [let_binding_compile]. Consult comment over there... *)
         List.iter
           (fun var ->
              Format.fprintf out_fmter "@ (%a : Set)"
               (Types.pp_type_simple_to_coq
                  print_ctx ~reuse_mapping: true ~self_as)
               var)
           generalized_instanciated_vars ;
         (* Now, print the real bound variables. *)
         Format.fprintf out_fmter "%a :@ %a,@ "
           (Handy.pp_generic_separated_list
              " "
              (fun ppf vn ->
                Format.fprintf ppf "%s"
                  (Parsetree_utils.vname_as_string_with_operators_expanded vn)))
           vnames
           (Types.pp_type_simple_to_coq
              print_ctx ~reuse_mapping: true ~self_as) ty ;
         (* IMHO : not really useful, but... doesn't hurt... *)
         Types.purge_type_simple_to_coq_variable_mapping () ;
         (* Here, the bound variables name may mask a "in"-parameter. *)
         let loc_idents' = vnames @ loc_idents in
         (* Add the bound variable in the environment. *)
         let nb_polymorphic_args = List.length generalized_instanciated_vars in
         let env' =
           List.fold_left
             (fun accu_env vname ->
               Env.CoqGenEnv.add_value vname nb_polymorphic_args accu_env)
             env
             vnames in
         rec_generate_prop loc_idents' env' prop ;
         Format.fprintf out_fmter "@]"
         end)
     | Parsetree.Pr_imply (prop1, prop2) ->
         Format.fprintf out_fmter "@[<2>" ;
         rec_generate_prop loc_idents env prop1 ;
         Format.fprintf out_fmter " ->@ " ;
         rec_generate_prop loc_idents env prop2 ;
         Format.fprintf out_fmter "@]"
     | Parsetree.Pr_or  (prop1, prop2) ->
         Format.fprintf out_fmter "@[<2>" ;
         rec_generate_prop loc_idents env prop1 ;
         Format.fprintf out_fmter " \\/@ " ;
         rec_generate_prop loc_idents env prop2 ;
         Format.fprintf out_fmter "@]"
     | Parsetree.Pr_and  (prop1, prop2) ->
         Format.fprintf out_fmter "@[<2>" ;
         rec_generate_prop loc_idents env prop1 ;
         Format.fprintf out_fmter " /\\@ " ;
         rec_generate_prop loc_idents env prop2 ;
         Format.fprintf out_fmter "@]"
     | Parsetree.Pr_equiv (prop1, prop2) ->
         Format.fprintf out_fmter "@[<2>" ;
         rec_generate_prop loc_idents env prop1 ;
         Format.fprintf out_fmter " <->@ " ;
         rec_generate_prop loc_idents env prop2 ;
         Format.fprintf out_fmter "@]"
     | Parsetree.Pr_not prop ->
         Format.fprintf out_fmter "@[<2>" ;
         Format.fprintf out_fmter "~" ;
         rec_generate_prop loc_idents env prop ;
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
         generate_expr ctx ~local_idents: loc_idents ~self_as env expr ;
         (* The end of the wrapper surrounding  *)
         (* the expression if it has type bool. *)
         if is_bool then Format.fprintf out_fmter ")@]"
     | Parsetree.Pr_paren prop ->
         Format.fprintf out_fmter "@[<1>(" ;
         rec_generate_prop loc_idents env prop ;
         Format.fprintf out_fmter ")@]" in

  (* ************************************************ *)
  (* Now, let's really do the job of [generate_prop]. *)
  rec_generate_prop local_idents initial_env initial_proposition
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
(* [Unsure] Encore utile ? *)
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
(* Species_gen_basics.species_compil_context ->                           *)
(*   Env.TypeInformation.species_field list -> unit                       *)
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
       | Species_gen_basics.CCMI_is -> Format.fprintf ppf "Set"
       | Species_gen_basics.CCMI_in_or_not_param ->
           if param_ty_mod <> ctx.Species_gen_basics.scc_current_unit then
             Format.fprintf ppf "%s." param_ty_mod ;
           Format.fprintf ppf "%s_T" param_ty_coll) ;
      Format.fprintf ppf ")@ @]")
    ctx.Species_gen_basics.scc_collections_carrier_mapping ;
  (* Now, we will find the methods of the parameters we decl-depend  *)
  (* on in the Coq type expressions. Such dependencies can only      *)
  (* appear through properties and theorems bodies.                  *)
  let species_parameters_names =
    ctx.Species_gen_basics.scc_species_parameters_names in
  let print_ctx = {
    Types.cpc_current_unit = ctx.Species_gen_basics.scc_current_unit ;
    Types.cpc_current_species =
      Some
        (Parsetree_utils.type_coll_from_qualified_species
           ctx.Species_gen_basics.scc_current_species) ;
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
        | Env.TypeInformation.SF_let (_, _, _, _, _, _)
        | Env.TypeInformation.SF_let_rec _-> []
        | Env.TypeInformation.SF_theorem (_, _, _, prop, _, _)
        | Env.TypeInformation.SF_property (_, _, _, prop, _) ->
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
              llift_name
              (Types.pp_type_simple_to_coq
                 print_ctx ~reuse_mapping: false ~self_as: Types.CSR_species) ty
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
let generate_record_type ctx env species_descr =
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
  let print_ctx = {
    Types.cpc_current_unit = ctx.Species_gen_basics.scc_current_unit ;
    Types.cpc_current_species =
      Some
        (Parsetree_utils.type_coll_from_qualified_species
           ctx.Species_gen_basics.scc_current_species) ;
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
    | Env.TypeInformation.SF_let (from, n, _, sch, _, _) ->
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
            (Types.pp_type_simple_to_coq
               print_ctx ~reuse_mapping: false ~self_as: Types.CSR_species) ty ;
          if semi then Format.fprintf out_fmter " ;" ;
          Format.fprintf out_fmter "@]@\n"
          end)
        end)
    | Env.TypeInformation.SF_let_rec l ->
        List.iter
          (fun (from, n, _, sch, _, _) ->
            let ty = Types.specialize sch in
            Format.fprintf out_fmter "(* From species %a. *)@\n"
              Sourcify.pp_qualified_species from ;
            (* Field is prefixed by the species name for sake of unicity. *)
            Format.fprintf out_fmter "%s_%a : %a"
              my_species_name
              Parsetree_utils.pp_vname_with_operators_expanded n
              (Types.pp_type_simple_to_coq
                 print_ctx ~reuse_mapping: false ~self_as: Types.CSR_species)
              ty ;
            if semi then Format.fprintf out_fmter " ;" ;
            Format.fprintf out_fmter "@]@\n")
          l
    | Env.TypeInformation.SF_theorem  (from, n, _, prop, _, _)
    | Env.TypeInformation.SF_property (from, n, _, prop, _) ->
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
        generate_prop ctx
          ~local_idents: [] ~self_as: Types.CSR_species env prop ;
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
  Format.fprintf out_fmter "@]}.@\n@\n"
;;
