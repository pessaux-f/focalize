(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            Fran�ois Pessaux                                                *)
(*            Pierre Weis                                                     *)
(*            Damien Doligez                                                  *)
(*                                                                            *)
(*               LIP6  --  INRIA Rocquencourt -- ENSTA ParisTech              *)
(*                                                                            *)
(*  Copyright 2007 - ... LIP6 and INRIA                                       *)
(*            2012 - ... ENSTA ParisTech                                      *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)


(* ************************************************************************** *)
(** {b Descr} : This module performs the compilation from FoCaL to Ocaml
      of basic FoCaL expressions, i.e. of the core constructs that don't
      deal with species and collections. More accurately, this includes
      the let, let rec constructs and the simple expressions
      ([Parsetree.expr]) including their sub-parts (patterns, and constants). *)
(* ************************************************************************** *)



(* ************************************************************************* *)
(* Format.formatter -> Parsetree.constant -> unit                            *)
(** {b Descr} : Generate the OCaml code from a FoCaL constant expression.

    {b Args} :
      - [out_fmter] : The out channel where to generate the OCaml source
                    code.
      - [constant] : The constant expression to translate into OCaml source
          code.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let generate_constant out_fmter constant =
  let protect_negative i =
       if String.length i > 0 && i.[0] = '-' then
         Format.fprintf out_fmter "(%s)" i
       else
         Format.fprintf out_fmter "%s" i in
  match constant.Parsetree.ast_desc with
   | Parsetree.C_int i -> protect_negative i
   | Parsetree.C_float fl -> protect_negative fl
   | Parsetree.C_bool b -> Format.fprintf out_fmter "%s" b
   | Parsetree.C_string s ->
       Format.fprintf out_fmter "\"%s\"" (String.escaped s)
   | Parsetree.C_char c -> Format.fprintf out_fmter "'%c'" c
;;



(* *********************************************************************** *)
(* Context.reduced_compil_context ->                                       *)
(*   local_idents: Parsetree.vname list -> Parsetree.expr_ident -> unit    *)
(** {b Descr} : Generate the OCaml code from a FoCaL [ident] in the
    context of method generator generation.

    The most tricky stuff is dealing with [ident] denoting methods
    (i.e. "!-ed" identifiers).
    If the method [ident] corresponds to a parameter's method then we use
    the lambda-lifted principle. Otherwise, this ident is considered as
    from something not abstracted, coming from a toplevel collection. In
    this case, it is printed as a field coming from the collection's
    module, i.e. the direct name of the method with its scoping qualification
    if there is some.

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let generate_expr_ident_for_method_generator ctx ~local_idents ident =
  let out_fmter = ctx.Context.rcc_out_fmter in
  match ident.Parsetree.ast_desc with
   | Parsetree.EI_local vname ->
       (begin
       (* Thanks to the scoping pass, identifiers remaining "local" are either
          really let-bound in the context of the expression, hence have a
          direct mapping between FoCaL and OCaml code, or species
          "IN"-parameters and then must be mapped onto the lambda-lifted
          parameter introduced for it in the context of the current species.
          Be careful, because a recursive method called in its body is scoped
          AS LOCAL ! And because recursive methods do not have dependencies
          together, there is no way to recover the extra parameters to apply to
          them in this configuration. Hence, to avoid forgetting these extra
          arguments, we must use here the information recorded in the context,
          i.e. the extra arguments of the recursive functions.
          To check if a "smelling local" identifier is really local or a
          "IN"-parameter of the species, we use the same reasoning that in
          [param_dep_analysis.ml]. Check justification over there ! *)
       if (List.exists
             (fun species_param ->
               match species_param with
                | Env.TypeInformation.SPAR_in (vn, _, _) -> vn = vname
                | Env.TypeInformation.SPAR_is ((_, vn), _, _, _, _) ->
                    (Parsetree.Vuident vn) = vname)
             ctx.Context.rcc_species_parameters_names) &&
         (not (List.mem vname local_idents)) then
         (begin
         (* In fact, a species "in"-parameter. This parameter was of the form
            "foo in C". Then it's naming scheme will be "_p_" + the species
            parameter's name + the method's name that is trivially the
            parameter's name again (because this last one is computed as the
            "stuff" a dependency was found on, and in the case of a
            "IN"-parameter, the dependency can only be on the parameter's value
            itself, not on any method since there is none !). *)
         Format.fprintf out_fmter "_p_%a_%a"
           Parsetree_utils.pp_vname_with_operators_expanded vname
           Parsetree_utils.pp_vname_with_operators_expanded vname
         end)
       else
         (begin
         (* Really a local identifier or a call to a recursive method. *)
         Format.fprintf out_fmter "%a"
           Parsetree_utils.pp_vname_with_operators_expanded vname;
         (* Because this method can be recursive, we must apply it to  its
            extra parameters if it has some. *)
         try
           let extra_args =
             List.assoc vname ctx.Context.rcc_lambda_lift_params_mapping in
           List.iter
             (fun s ->
               (* Ignore "rep" if present because in OCaml code generation
                  model, the carrier is never lambda-lifted. *)
(* [Unsure] Je crois que maintenant le test n'est plus n�cessaire. *)
               if s <> "abst_rep" then Format.fprintf out_fmter "@ %s" s)
             extra_args
            with Not_found -> ()
         end)
       end)
   | Parsetree.EI_global (Parsetree.Vname _) ->
       (* In this case, may be there is some scoping process missing. *)
       assert false
   | Parsetree.EI_global (Parsetree.Qualified (mod_name, vname)) ->
       (* Call the OCaml corresponding identifier in the corresponding module
          (i.e. the capitalized [mod_name]). If the module is the currently
          compiled one, then do not qualify the identifier. *)
       if mod_name <> ctx.Context.rcc_current_unit then
         Format.fprintf out_fmter "%s.%a"
           (String.capitalize mod_name)
           Parsetree_utils.pp_vname_with_operators_expanded vname
       else
         Format.fprintf out_fmter "%a"
           Parsetree_utils.pp_vname_with_operators_expanded vname
   | Parsetree.EI_method (coll_specifier_opt, vname) ->
       (begin
       match coll_specifier_opt with
        | None
        | Some (Parsetree.Vname (Parsetree.Vuident "Self")) ->
            (begin
            (* Method call from the current species. This corresponds to a call
               to the corresponding lambda-lifted method that is represented as
               an extra parameter of the OCaml function. *)
            Format.fprintf out_fmter "abst_%a"
              Parsetree_utils.pp_vname_with_operators_expanded vname
            end)
        | Some coll_specifier ->
            (begin
            match coll_specifier with
             | Parsetree.Vname coll_name ->
                 (begin
                 (* Method call from a species that is not the current but is
                    implicitely in the current compilation unit. May be either
                    a paramater or a toplevel defined collection. *)
                 if List.exists
                     (fun species_param ->
                       match species_param with
                        | Env.TypeInformation.SPAR_in (vn, _, _) ->
                            vn = coll_name
                        | Env.TypeInformation.SPAR_is ((_, vn), _, _, _, _) ->
                            (Parsetree.Vuident vn) = coll_name)
                     ctx.Context.rcc_species_parameters_names then
                   (begin
                   (* It comes from a parameter. To retrieve the related
                      method name we build it the same way we built it while
                      generating the extra OCaml function's parameters due to
                      depdencencies coming from the species parameter.
                      I.e: "_p_", followed by the species parameter name,
                      followed by "_", followed by the method's name. *)
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
                     "%a.%a"
                     Parsetree_utils.pp_vname_with_operators_expanded coll_name
                     Parsetree_utils.pp_vname_with_operators_expanded vname
                   end)
                 end)
             | Parsetree.Qualified (module_name, coll_name) -> (
                 if module_name = ctx.Context.rcc_current_unit then (
                   (* Exactly like when it is method call from a species that
                      is not the current but is implicitely in the current
                      compilation unit : the call is performed to a method a
                      species that is EXPLICITELY in the current compilation
                      unit. *)
                   if List.exists
                       (fun species_param ->
                         match species_param with
                         | Env.TypeInformation.SPAR_in (vn, _, _) ->
                             vn = coll_name
                         | Env.TypeInformation.SPAR_is ((_, vn), _, _, _, _) ->
                             (Parsetree.Vuident vn) = coll_name)
                       ctx.Context.rcc_species_parameters_names then (
                     let prefix =
                       "_p_" ^ (Parsetree_utils.name_of_vname coll_name) ^
                       "_" in
                     Format.fprintf out_fmter "%s%a"
                       prefix Parsetree_utils.pp_vname_with_operators_expanded
                       vname
                    )
                   else (
                     Format.fprintf out_fmter
                       "%a.%a"
                       Parsetree_utils.pp_vname_with_operators_expanded
                       coll_name
                       Parsetree_utils.pp_vname_with_operators_expanded
                       vname
                    )
                  )
                 else (
                   (* The called method belongs to a collection that is not
                      ourselves and moreover belongs to another compilation
                      unit. May be a species from the toplevel of another
                      FoCaL source file. *)
                   let capitalized_modname = String.capitalize module_name in
                   Format.fprintf out_fmter
                     "%s.%a.%a"
                     capitalized_modname
                     Parsetree_utils.pp_vname_with_operators_expanded coll_name
                     Parsetree_utils.pp_vname_with_operators_expanded vname
                  )
                )
            end)
       end)
;;



(* ******************************************************************** *)
(* Context.reduced_compil_context ->                                    *)
(*   Env.MlGenEnv.t -> Parsetree.constructor_ident -> unit              *)
(** {b Descr} : Generate the OCaml code from a FoCaL [constructor_expr]
    in the context of method generator generation.
    The translation of the FoCaL sum constructor is obtained from the
    code generation environment. If this environment doesn't contain a
    binding for the constructor, this means that this constructor
    directly corresponds to its FoCaL name in the OCaml code.

    {b Rem} : Not exported outside this module.                         *)
(* ******************************************************************** *)
let generate_constructor_ident_for_method_generator ctx env cstr_expr =
  try
    let mapping_info =
      Env.MlGenEnv.find_constructor
        ~loc: cstr_expr.Parsetree.ast_loc
        ~current_unit: ctx.Context.rcc_current_unit cstr_expr env in
    let (_, ocaml_binding) =
      try
        List.find
          (function
            | (Parsetree.EL_Caml, _) -> true
            | (Parsetree.EL_Coq, _)
            | (Parsetree.EL_Dk, _)
            | ((Parsetree.EL_external _), _) -> false)
          mapping_info
      with Not_found ->
        (* No OCaml mapping found. *)
        raise
          (Externals_generation_errs.No_external_constructor_def
             ("OCaml", cstr_expr)) in
    (* Now directly generate the name the constructor is mapped onto. *)
    Format.fprintf ctx.Context.rcc_out_fmter "%s" ocaml_binding
  with
  | Env.Unbound_constructor (_, _) ->
      (begin
      let Parsetree.CI glob_ident = cstr_expr.Parsetree.ast_desc in
      match glob_ident.Parsetree.ast_desc with
       | Parsetree.I_local name | Parsetree.I_global (Parsetree.Vname name) ->
           Format.fprintf ctx.Context.rcc_out_fmter "%a"
             Parsetree_utils.pp_vname_with_operators_expanded name
       | Parsetree.I_global (Parsetree.Qualified (fname, name)) ->
           (* If the constructor belongs to the current compilation unit then
              one must not qualify it. *)
           if fname <> ctx.Context.rcc_current_unit then
             Format.fprintf ctx.Context.rcc_out_fmter "%s.%a"
               (String.capitalize fname)
               Parsetree_utils.pp_vname_with_operators_expanded name
           else
             Format.fprintf ctx.Context.rcc_out_fmter "%a"
               Parsetree_utils.pp_vname_with_operators_expanded name
      end)
;;




let pp_to_ocaml_label_ident ctx ppf lab_ident =
  (* Just mask the previous [lbl_ident] to simply remove the only possible 
     constructor [LI], and to get the interesting information. *)
  let Parsetree.LI lab_ident = lab_ident.Parsetree.ast_desc in
  match lab_ident.Parsetree.ast_desc with
   | Parsetree.I_local vname ->
       Format.fprintf ppf "%a"
         Parsetree_utils.pp_vname_with_operators_expanded vname
   | Parsetree.I_global qual_name ->
       let vname =
         (match qual_name with
          | Parsetree.Vname n -> n
          | Parsetree.Qualified (modname, n) ->
              (* If the constructor belongs to the current compilation unit
                 then one must not qualify it. *)
              if modname <> ctx.Context.rcc_current_unit then
                Format.fprintf ppf "%s." (String.capitalize modname) ;
              n) in
       Format.fprintf ppf "%a"
         Parsetree_utils.pp_vname_with_operators_expanded vname
;;



(* ************************************************************************** *)
(** {b Descr} : Function to the general expression generation that generates
    one record field name according to whether it has a special translation to
    OCaml because it is external (hence it is bound in the code generation
    environment) or it is straightly translated as a regular FoCaL record
    label.
    Making this function prevents from writing several times the code to
    access the environment and determine if we write the label with
    translation of directly depending of the access result.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let generate_record_field_name env ctx label =
  let out_fmter = ctx.Context.rcc_out_fmter in
  try
    let mapping_info =
      Env.MlGenEnv.find_label
        ~loc: label.Parsetree.ast_loc
        ~current_unit: ctx.Context.rcc_current_unit label env in
    let (_, ocaml_binding) =
      try
        List.find
          (function
            | (Parsetree.EL_Caml, _) -> true
            | (Parsetree.EL_Coq, _)
            | (Parsetree.EL_Dk, _)
            | ((Parsetree.EL_external _), _) -> false)
          mapping_info
      with Not_found ->
        (* No OCaml mapping found. *)
        raise
          (Externals_generation_errs.No_external_field_def
             ("OCaml", label)) in
    Format.fprintf out_fmter "%s" ocaml_binding
  with
  | Env.Unbound_label (_, _) ->
      (* If no binding for the field name in the environment, then it get's
         directly mapped onto its FoCaL name. *)
      Format.fprintf out_fmter "%a" (pp_to_ocaml_label_ident ctx) label
;;



let generate_pattern ctx env pattern =
  let out_fmter = ctx.Context.rcc_out_fmter in
  let rec rec_gen_pat pat =
    match pat.Parsetree.ast_desc with
     | Parsetree.P_const constant -> generate_constant out_fmter constant
     | Parsetree.P_var name ->
         Format.fprintf out_fmter "%a"
           Parsetree_utils.pp_vname_with_operators_expanded name
     | Parsetree.P_as (p, name) ->
         Format.fprintf out_fmter "(" ;
         rec_gen_pat p ;
         Format.fprintf out_fmter "as@ %a)"
           Parsetree_utils.pp_vname_with_operators_expanded name
     | Parsetree.P_wild -> Format.fprintf out_fmter "_"
     | Parsetree.P_constr (ident, pats) -> (
         generate_constructor_ident_for_method_generator ctx env ident ;
         (* Discriminate on the number of arguments to know if parens are
            needed. *)
         match pats with
          | [] -> ()
          | _ ->
              Format.fprintf out_fmter " (" ;
              rec_generate_pats_list pats ;
              Format.fprintf out_fmter ")"
        )
     | Parsetree.P_record labs_pats ->
         Format.fprintf out_fmter "@[<1>{" ;
         rec_generate_pats_record labs_pats ;
         Format.fprintf out_fmter "}@]"
     | Parsetree.P_tuple pats ->
         Format.fprintf out_fmter "@[<1>(" ;
         rec_generate_pats_list pats ;
         Format.fprintf out_fmter ")@]"
     | Parsetree.P_paren p ->
         Format.fprintf out_fmter "@[<1>(" ;
         rec_gen_pat p ;
         Format.fprintf out_fmter ")@]"

  and rec_generate_pats_record = function
    | [] -> ()
    | [(last_label, last_pattern)] ->
        generate_record_field_name env ctx last_label ;
        Format.fprintf out_fmter " =@ " ;
        rec_gen_pat last_pattern
    | (label, pattern) :: q ->
        generate_record_field_name env ctx label ;
        Format.fprintf out_fmter " =@ " ;
        rec_gen_pat pattern ;
        Format.fprintf out_fmter ";@ " ;
        rec_generate_pats_record q

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



let rec let_binding_compile ctx ~local_idents env bd opt_sch =
  let out_fmter = ctx.Context.rcc_out_fmter in
  (* Generate the bound name. *)
  Format.fprintf out_fmter "%a"
    Parsetree_utils.pp_vname_with_operators_expanded
    bd.Parsetree.ast_desc.Parsetree.b_name;
  (* Generate the parameters if some, with their type constraints. *)
  let params_names = List.map fst bd.Parsetree.ast_desc.Parsetree.b_params in
  (* We ignore the result type of the "let" if it's a function because we never
     print the type constraint on the result of the "let". We only print them
     in the arguments of the let-bound ident.
     We also ignore the variables used to instanciate the polymorphic ones of
     the scheme because in OCaml polymorphism is not explicit.
     Note by the way thet we do not have anymore information about "Self"'s
     structure... *)
  let (params_with_type, _, _) =
    MiscHelpers.bind_parameters_to_types_from_type_scheme
      ~self_manifest: None opt_sch params_names in
  List.iter
    (fun (param_vname, pot_param_ty) ->
      match pot_param_ty with
       | Some param_ty ->
           Format.fprintf out_fmter "@ (%a : %a)"
             Parsetree_utils.pp_vname_with_operators_expanded param_vname
             (Ml_pprint.pp_type_simple_to_ml
                ~current_unit: ctx.Context.rcc_current_unit
                ctx.Context.rcc_collections_carrier_mapping)
             param_ty
       | None ->
           Format.fprintf out_fmter "@ %a"
             Parsetree_utils.pp_vname_with_operators_expanded param_vname)
    params_with_type ;
  (* Output now the "=" sign ending the OCaml function's "header".
     With a NON-breakable space before to prevent uggly hyphenation ! *)
  Format.fprintf out_fmter " =@ ";
  (* Here, each parameter name of the binding may mask a "in"-parameter. *)
  let local_idents' = params_names @ local_idents in
  (* Now, let's generate the bound body. *)
  match bd.Parsetree.ast_desc.Parsetree.b_body with
   | Parsetree.BB_computational e ->
       generate_expr ctx ~local_idents: local_idents' env e
   | Parsetree.BB_logical _ -> assert false



(* ********************************************************************* *)
(* Context.reduced_compil_context -> Env.MlGenEnv.t ->                   *)
(*   local_idents: Parsetree.vname list -> Parsetree.let_def ->          *)
(*     Types.type_scheme list -> unit                                    *)
(** {b Desrc} : Generates the OCaml code for a FoCaL "let"-definition.

    {b Args} :
      - [out_fmter] : The out channel where to generate the OCaml source
              code.

      - [local_idents] : The list of known local identifiers in the
          scope of this "let" EXPRESSION.

      - [env] : The current compilation environment allowing to know on
          what to map sum constructors and record field names.

      - [let_def] : The [Parsetree.let_def] structure representing the
          "let-definition" for which th generate the OCaml source code.

      - [bound_schemes] : The list of types schemes of the identifiers
          bound to the "let-definition" (i.e. several if the definition
          is a "rec", hence binds several identifiers).
          In effect, because we do not have directly inside the
          [Parsetree.let_def] these schemes, in order to be able to
          generate the type constraints of each components of the
          "let-definition", we must take these schemes aside.
          It is sometimes impossible yet to have this information.
          In this case, no type constraint will be added to the
          parameter of the bound identifiers.

    {b Rem} : Not exported outside this module.                          *)
(* ********************************************************************* *)
and let_def_compile ctx ~local_idents env let_def bound_schemes =
  (* For OCaml, logical lets are not generated. *)
  if let_def.Parsetree.ast_desc.Parsetree.ld_logical =
     Parsetree.LF_no_logical then
    (begin
    let out_fmter = ctx.Context.rcc_out_fmter in
    (* Generates the binder ("rec" or non-"rec"). *)
    Format.fprintf out_fmter "@[<2>let%s@ "
      (match let_def.Parsetree.ast_desc.Parsetree.ld_rec with
       | Parsetree.RF_no_rec -> ""
       | Parsetree.RF_rec ->
           " rec"   (* NON-breakable space in front. *)) ;
    (* Now generate each bound definition. *)
    (match (let_def.Parsetree.ast_desc.Parsetree.ld_bindings, bound_schemes)
    with
     | ([], []) ->
         (* The "let" construct should always at least bind one identifier ! *)
         assert false
     | ([one_bnd], [one_scheme]) ->
         let_binding_compile ctx ~local_idents env one_bnd one_scheme
     | ((first_bnd :: next_bnds), (first_scheme :: next_schemes)) ->
         let_binding_compile ctx ~local_idents env first_bnd first_scheme;
         List.iter2
           (fun binding scheme ->
             Format.fprintf out_fmter "@]@\n@[<2>and ";
             let_binding_compile ctx ~local_idents env binding scheme)
           next_bnds
           next_schemes
     | (_, _) ->
         (* Because the FoCaL has been parsed and typechecked, we must never
            have a different number of bound identifiers and bound-identifiers'
            type schemes. If this arise, then we have a serious bug
            somewhere. *)
         assert false);
    Format.fprintf out_fmter "@]"
    end)



(* ******************************************************************** *)
(* Context.reduced_compil_context ->                                    *)
(*   local_idents: Parsetree.vname list ->  Env.MlGenEnv.t ->           *)
(*     Parsetree.expr -> unit                                           *)
(** {b Descr} : Generate the OCaml code from a FoCaL expression.

    {b Rem} : Not exported outside this module.                         *)
(* ******************************************************************** *)
and generate_expr ctx ~local_idents env initial_expression =
  let out_fmter = ctx.Context.rcc_out_fmter in
  let rec rec_generate loc_idents expr =
    (* Generate the source code for the expression. *)
    match expr.Parsetree.ast_desc with
     | Parsetree.E_self ->
         (* [Unsure] D'ailleurs, est-ce possible en fait ? *)
         failwith "generate_expr E_self TODO"
     | Parsetree.E_const cst -> generate_constant out_fmter cst
     | Parsetree.E_fun (args_names, body) ->
         List.iter
           (fun n ->
             Format.fprintf out_fmter "@[<2>(fun@ %a@ ->@ "
               Parsetree_utils.pp_vname_with_operators_expanded n)
           args_names;
         (* Here, the function parameter name may mask a "in"-parameter. *)
         let loc_idents' = args_names @ loc_idents in
         rec_generate loc_idents' body;
         Format.fprintf out_fmter ")@]"
     | Parsetree.E_var ident ->
         generate_expr_ident_for_method_generator
           ctx ~local_idents: loc_idents ident
     | Parsetree.E_app (expr, exprs) ->
         Format.fprintf out_fmter "@[<2>(";
         rec_generate loc_idents expr;
         Format.fprintf out_fmter "@ ";
         rec_generate_exprs_list "" loc_idents exprs;
         Format.fprintf out_fmter ")@]"
     | Parsetree.E_constr (cstr_expr, exprs) ->
         (begin
         (* If the constructor has parameters, we force the whole value to be
            surrounded by parentheses to prevent associativity problems. *)
         if exprs <> [] then Format.fprintf out_fmter "(";
         generate_constructor_ident_for_method_generator ctx env cstr_expr;
         match exprs with
          | [] -> ()
          | _ ->
              (* If argument(s), enclose by parens to possibly make a tuple. *)
              Format.fprintf out_fmter "@ @[<1>(";
              rec_generate_exprs_list "," loc_idents exprs;
              (* Don't forget to close the parenthesis opened to surround the
                 whole value. *)
              Format.fprintf out_fmter ")@])"
         end)
     | Parsetree.E_match (matched_expr, pats_exprs) ->
         (begin
         Format.fprintf out_fmter "@[<1>match ";
         rec_generate loc_idents matched_expr;
         Format.fprintf out_fmter " with";
         List.iter
           (fun (pattern, expr) ->
             (* My indentation style: indent of 4 between the pattern and its
                related processing. *)
             Format.fprintf out_fmter "@\n@[<4>| ";
             generate_pattern ctx env pattern;
             (* Enclose each match-case by begin/end to ensure no confusion. *)
             Format.fprintf out_fmter " ->@\n(begin@\n";
             (* Here, each name of the pattern may mask a "in"-parameter. *)
             let loc_idents' =
               (Parsetree_utils.get_local_idents_from_pattern pattern) @
               loc_idents in
             rec_generate loc_idents' expr;
             Format.fprintf out_fmter "@\nend)@]")
           pats_exprs;
         Format.fprintf out_fmter "@]"
         end)
     | Parsetree.E_if (expr1, expr2, expr3) ->
         Format.fprintf out_fmter "@[<2>if@ ";
         rec_generate loc_idents expr1;
         Format.fprintf out_fmter "@ @[<2>then@ @]";
         rec_generate loc_idents expr2;
         Format.fprintf out_fmter "@ @[<2>else@ @]";
         rec_generate loc_idents expr3;
         Format.fprintf out_fmter "@]"
     | Parsetree.E_let (let_def, in_expr) ->
         (* Here we do not have type contraints under the hand. So give-up
            generating such constraints. Just generate the raw code for the
            "let-definition". *)
         let bound_schemes =
           List.map
             (fun _ -> None)
             let_def.Parsetree.ast_desc.Parsetree.ld_bindings in
         let_def_compile
           ctx ~local_idents: loc_idents env let_def bound_schemes ;
         Format.fprintf out_fmter "@ in@\n" ;
         rec_generate loc_idents in_expr
     | Parsetree.E_record labs_exprs ->
         Format.fprintf out_fmter "@[<1>{@ " ;
         rec_generate_record_field_exprs_list loc_idents labs_exprs ;
         Format.fprintf out_fmter "@ }@]"
     | Parsetree.E_record_access (expr, label_name) ->
         Format.fprintf out_fmter "@[<2>" ;
         rec_generate loc_idents expr ;
         Format.fprintf out_fmter ".@," ;
         generate_record_field_name env ctx label_name ;
         Format.fprintf out_fmter "@]"
     | Parsetree.E_record_with (expr, labs_exprs) ->
         (* Because in OCaml the with construct only starts by an ident, we
            create a temporary ident to bind the expression to an ident. *)
         Format.fprintf out_fmter "@[<2>let __foc_tmp_with_ =@ ";
         rec_generate loc_idents expr;
         Format.fprintf out_fmter "@ in@] ";
         (* Now really generate the "with"-construct. *)
         Format.fprintf out_fmter "@[<2>{ __foc_tmp_with_ with@\n";
         List.iter
           (fun (label_name, field_expr) ->
             Format.fprintf out_fmter "%a =@ "
               (pp_to_ocaml_label_ident ctx) label_name ;
             rec_generate loc_idents field_expr ;
             Format.fprintf out_fmter ";")
           labs_exprs ;
         Format.fprintf out_fmter "@ }@]"
     | Parsetree.E_tuple exprs -> (
         match exprs with
          | [] -> assert false
          | [one] -> rec_generate loc_idents one
          | _ ->
              Format.fprintf out_fmter "@[<1>(";
              rec_generate_exprs_list "," loc_idents exprs;
              Format.fprintf out_fmter ")@]"
        )
     | Parsetree.E_sequence exprs -> (
         match exprs with
         | [] -> Format.fprintf out_fmter "()"
         | _ ->
             Format.fprintf out_fmter "@[<2>begin@ ";
             rec_generate_exprs_list ";" loc_idents exprs ;
             Format.fprintf out_fmter "@ end@]"
        )
     | Parsetree.E_external external_expr ->
         (begin
          let e_translation =
            external_expr.Parsetree.ast_desc.Parsetree.ee_external in
          try
            (* Simply a somewhat verbatim output of the Caml translation. *)
            let (_, caml_code) =
              List.find
                (function
                 | (Parsetree.EL_Caml, _) -> true
                 | (Parsetree.EL_Coq, _)
                 | (Parsetree.EL_Dk, _)
                 | ((Parsetree.EL_external _), _) -> false)
                e_translation.Parsetree.ast_desc in
            Format.fprintf out_fmter "%s" caml_code
          with Not_found ->
            (* No OCaml mapping found. *)
            raise
              (Externals_generation_errs.No_external_value_def
                 ("OCaml", (Parsetree.Vlident "<expr>"),
                   expr.Parsetree.ast_loc))
          end)
     | Parsetree.E_paren e -> rec_generate loc_idents e



  and rec_generate_exprs_list comma loc_idents = function
    | [] -> ()
    | [last] -> rec_generate loc_idents last
    | h :: q ->
        rec_generate loc_idents h ;
        Format.fprintf out_fmter "%s@ " comma ;
        rec_generate_exprs_list comma loc_idents q



  and rec_generate_record_field_exprs_list loc_idents = function
    | [] -> ()
    | [(label, last)] ->
        generate_record_field_name env ctx label ;
        Format.fprintf out_fmter " =@ " ;
        rec_generate loc_idents last
    | (h_label, h_expr) :: q ->
        generate_record_field_name env ctx h_label;
        Format.fprintf out_fmter " =@ " ;
        rec_generate loc_idents h_expr;
        Format.fprintf out_fmter ";@ " ;
        rec_generate_record_field_exprs_list loc_idents q in


  (* ************************************************ *)
  (* Now, let's really do the job of [generate_expr]. *)
  rec_generate local_idents initial_expression
;;
