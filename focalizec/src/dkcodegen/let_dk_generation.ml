(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            Raphaël Cauderlier                                              *)
(*                                                                            *)
(*               LIP6  --  INRIA Rocquencourt -- ENSTA ParisTech              *)
(*                                                                            *)
(*  Copyright 2007 - ... LIP6 and INRIA                                       *)
(*            2012 - ... ENSTA ParisTech                                      *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)

(* ************************************************************************** *)
(** {b Descr}: Exception raised when a toplevel let-definition is tagged
    "logical".

    {b Rem}: Exported outside this module.                                    *)
(* ************************************************************************** *)
exception Logical_methods_only_inside_species of Location.t ;;



(* ************************************************************************** *)
(** {b Descr}: Code generation for *one* let binding, recursive of not.
    If the binding is recursive, then whatever the choosen Dk primitive ("fix"
    or "Fixpoint"),
    This function is called by [Main_dk_generation.toplevel_let_def_compile]
    to generate code for toplevel definitions and by [let_in_def_compile] to
    generate code for local definitions.

    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
let non_rec_let_binding_compile ctx
    ~in_recursive_let_section_of ~local_idents ~self_methods_status
    ~recursive_methods_status ~toplevel env bd
    pre_computed_bd_info =
  (* Create once for all the flag used to insert the let-bound idents in the
     environment. *)
  let toplevel_loc = if toplevel then Some bd.Parsetree.ast_loc else None in
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Generate the binder and the bound name. *)
  let fun_name = bd.Parsetree.ast_desc.Parsetree.b_name in
  Format.fprintf out_fmter "def %a"
    Parsetree_utils.pp_vname_with_operators_expanded
    fun_name;
  (* Build the print context. *)
  let print_ctx = {
    Dk_pprint.dpc_current_unit = ctx.Context.scc_current_unit ;
    Dk_pprint.dpc_current_species =
      Some
        (Parsetree_utils.type_coll_from_qualified_species
           ctx.Context.scc_current_species) ;
    Dk_pprint.dpc_collections_carrier_mapping =
      ctx.Context.scc_collections_carrier_mapping } in
  let generalized_vars = pre_computed_bd_info.Expr_dk_generation.lbpc_generalized_vars in
  (* If the original scheme is polymorphic, then we must add extra Dk
     parameters of type "Set" for each of the generalized variables. Hence,
     printing the variables used to instanciate the polymorphic ones in front
     of the function, they will appear and moreover they will be "tagged" as
     "seen" in the variable mapping. Hence, when we will print the arguments
     having these variables as type, the same variable name will be used,
     hence establishing the correct link between the type of the variable and
     the type variable of the function argument's type. *)
  List.iter
    (fun var ->
      Format.fprintf out_fmter "@ (%a : cc.uT)"
        Dk_pprint.pp_type_variable_to_dk var)
    generalized_vars ;
  let params_with_type = pre_computed_bd_info.Expr_dk_generation.lbpc_params_with_type in
  (* Now, generate each of the real function's parameter with its type. *)
  List.iter
    (fun (param_vname, pot_param_ty) ->
      match pot_param_ty with
       | Some param_ty ->
           Format.fprintf out_fmter "@ (%a : cc.eT %a)"
             Parsetree_utils.pp_vname_with_operators_expanded param_vname
             (Dk_pprint.pp_type_simple_to_dk print_ctx) param_ty
       | None ->
           (* Because we provided a type scheme to the function
              [bind_parameters_to_types_from_type_scheme], MUST get one type
              for each parameter name ! *)
           assert false)
    params_with_type ;
  (* Now, print the result type of the "definition". *)
  (match pre_computed_bd_info.Expr_dk_generation.lbpc_result_ty with
   | None ->
       (* Because we provided a type scheme to the function
          [bind_parameters_to_types_from_type_scheme], MUST get one type for
          the result value of the "let". *)
       assert false
   | Some t ->
       Format.fprintf out_fmter "@ :@ cc.eT %a"
         (Dk_pprint.pp_type_simple_to_dk print_ctx) t
  ) ;
  (* Output now the ":=" sign ending the Dk function's "header".
     With a NON-breakable space before to prevent uggly hyphenation ! *)
  Format.fprintf out_fmter " :=@\n" ;
  (* Here, each parameter name of the binding may mask a "in"-parameter. *)
  let local_idents' =
    (List.map fst pre_computed_bd_info.Expr_dk_generation.lbpc_params_with_type) @ local_idents in
  (* Now, let's generate the bound body. *)
  (match bd.Parsetree.ast_desc.Parsetree.b_body with
  | Parsetree.BB_computational e ->
      Expr_dk_generation.generate_expr
        ctx ~in_recursive_let_section_of ~local_idents: local_idents'
        ~self_methods_status ~recursive_methods_status env e
  | Parsetree.BB_logical _ -> assert false) ;
  (* Finally, we record, (except if it was already done in [env'] in case of
     recursive binding) the number of extra arguments due to polymorphism the
     current bound identifier has. *)
  Env.DkGenEnv.add_value
    ~toplevel: toplevel_loc bd.Parsetree.ast_desc.Parsetree.b_name
    pre_computed_bd_info.Expr_dk_generation.lbpc_value_body env
;;

let rec_let_binding_compile ctx
    ~in_recursive_let_section_of ~local_idents ~self_methods_status
    ~recursive_methods_status env bd
    pre_computed_bd_info =
  let out_fmter = ctx.Context.scc_out_fmter in
  let fun_name = bd.Parsetree.ast_desc.Parsetree.b_name in
  (* Generate the binder and the bound name for the function. *)
  Format.fprintf out_fmter "def %a :@ "
    Parsetree_utils.pp_vname_with_operators_expanded
    fun_name ;
  (* Build the print context. *)
  let print_ctx = {
    Dk_pprint.dpc_current_unit = ctx.Context.scc_current_unit ;
    Dk_pprint.dpc_current_species =
      Some
        (Parsetree_utils.type_coll_from_qualified_species
           ctx.Context.scc_current_species) ;
    Dk_pprint.dpc_collections_carrier_mapping =
      ctx.Context.scc_collections_carrier_mapping } in
  let generalized_vars = pre_computed_bd_info.Expr_dk_generation.lbpc_generalized_vars in
  List.iter
    (fun var ->
      Format.fprintf out_fmter "%a : cc.uT ->@ "
        Dk_pprint.pp_type_variable_to_dk var)
    generalized_vars ;
  let params_with_type = pre_computed_bd_info.Expr_dk_generation.lbpc_params_with_type in
  (* Now, generate each of the real function's parameter with its type. *)
  List.iter
    (fun (param_vname, pot_param_ty) ->
      match pot_param_ty with
       | Some param_ty ->
           Format.fprintf out_fmter "%a : cc.eT (%a) ->@ "
             Parsetree_utils.pp_vname_with_operators_expanded param_vname
             (Dk_pprint.pp_type_simple_to_dk print_ctx) param_ty
       | None ->
           (* Because we provided a type scheme to the function
              [bind_parameters_to_types_from_type_scheme], MUST get one type
              for each parameter name ! *)
           assert false)
    params_with_type ;
  let return_ty =
  (match pre_computed_bd_info.Expr_dk_generation.lbpc_result_ty with
   | None ->
       (* Because we provided a type scheme to the function
          [bind_parameters_to_types_from_type_scheme], MUST get one type for
          the result value of the "let". *)
       assert false
   | Some t -> t) in
  Format.fprintf out_fmter "@ cc.eT (%a)"
                 (Dk_pprint.pp_type_simple_to_dk print_ctx) return_ty;
  (* Now, print the result type of the "definition". *)
  Format.fprintf out_fmter ".@\n";
  let rec print_cbv_types_as_arrows out = function
    | [] -> Dk_pprint.pp_type_simple_to_dk print_ctx out return_ty
    | ty :: l ->
       Format.fprintf out "(@[cc.Arrow %a %a@])"
                      (Dk_pprint.pp_type_simple_to_dk print_ctx) ty
                      print_cbv_types_as_arrows l
  in
  let rec print_cbv accu out = function
    | [] -> Parsetree_utils.pp_vname_with_operators_expanded out fun_name
    | (a, Some ty) :: l when Dk_pprint.has_cbv ty ->
       Format.fprintf out "@[%a@ %a@ (%a)@ %a@]"
                      (Dk_pprint.pp_for_cbv_type_simple_to_dk print_ctx) ty
                      print_cbv_types_as_arrows accu
                      (print_cbv (ty :: accu)) l
                      Parsetree_utils.pp_vname_with_operators_expanded a
    | (a, Some ty) :: l ->
       Format.fprintf out "@[%a@ %a@]"
                      (print_cbv (ty :: accu)) l
                      Parsetree_utils.pp_vname_with_operators_expanded a
    | (_, None) :: _ -> assert false
  in
  Format.fprintf out_fmter "@[[";
  List.iter
    (fun var ->
      Format.fprintf out_fmter "%a,@ "
        Dk_pprint.pp_type_variable_to_dk var)
    generalized_vars ;
  List.iteri
    (fun i (param_vname, _) ->
         Format.fprintf out_fmter (if i = 0 then "%a" else ",@ %a")
             Parsetree_utils.pp_vname_with_operators_expanded param_vname)
    params_with_type ;
  Format.fprintf out_fmter "] %a"
                 Parsetree_utils.pp_vname_with_operators_expanded
                 fun_name;
  List.iter
    (fun var ->
      Format.fprintf out_fmter "@ %a"
        Dk_pprint.pp_type_variable_to_dk var)
    generalized_vars ;
  List.iter
    (fun (param_vname, _) ->
           Format.fprintf out_fmter "@ %a"
             Parsetree_utils.pp_vname_with_operators_expanded param_vname)
    params_with_type ;
  Format.fprintf out_fmter " -->@ (%a :=@ "
                 Parsetree_utils.pp_vname_with_operators_expanded
                 fun_name;
  List.iter
    (fun var ->
      Format.fprintf out_fmter "@ (%a : cc.uT =>@ "
        Dk_pprint.pp_type_variable_to_dk var)
    generalized_vars ;
  List.iter
    (function
      | (param_vname, Some ty) ->
         Format.fprintf out_fmter "@ (%a : cc.eT (%a) =>@ "
           Parsetree_utils.pp_vname_with_operators_expanded param_vname
           (Dk_pprint.pp_type_simple_to_dk print_ctx) ty
      | _, None -> assert false)
    params_with_type ;
  print_cbv [] out_fmter (List.rev params_with_type);
  List.iter (fun _ -> Format.fprintf out_fmter ")") generalized_vars;
  List.iter (fun _ -> Format.fprintf out_fmter ")") params_with_type;
  Format.fprintf out_fmter "@ =>@ ";
  (* Here, each parameter name of the binding may mask a "in"-parameter. *)
  let local_idents' =
    (List.map fst pre_computed_bd_info.Expr_dk_generation.lbpc_params_with_type) @ local_idents in
  (* Now, let's generate the bound body. *)
  (match bd.Parsetree.ast_desc.Parsetree.b_body with
  | Parsetree.BB_computational e ->
      Expr_dk_generation.generate_expr
        ctx ~in_recursive_let_section_of ~local_idents: local_idents'
        ~self_methods_status ~recursive_methods_status env e
  | Parsetree.BB_logical _ -> assert false) ;
  Format.fprintf out_fmter ")@]";
  (* The value has already been added to the environment. *)
  env

let let_binding_compile ctx
    ~in_recursive_let_section_of ~local_idents ~self_methods_status
    ~recursive_methods_status ~rec_status ~toplevel env bd
    pre_computed_bd_info =
  if rec_status = Env.RC_rec Env.RPK_other then
    (rec_let_binding_compile ctx
    ~in_recursive_let_section_of ~local_idents ~self_methods_status
    ~recursive_methods_status env bd
    pre_computed_bd_info)
  else (non_rec_let_binding_compile ctx
    ~in_recursive_let_section_of ~local_idents ~self_methods_status
    ~recursive_methods_status ~toplevel env bd
    pre_computed_bd_info)
;;




(* ************************************************************************** *)
(** {b Descr}: Generates code for a toplevel function.                        *)
(* ************************************************************************** *)
let toplevel_let_def_compile ctx env let_def =
  if let_def.Parsetree.ast_desc.Parsetree.ld_logical = Parsetree.LF_logical then
    raise (Logical_methods_only_inside_species let_def.Parsetree.ast_loc) ;
  let out_fmter = ctx.Context.scc_out_fmter in
  let rec_status =
    (match let_def.Parsetree.ast_desc.Parsetree.ld_rec with
     | Parsetree.RF_no_rec -> Env.RC_non_rec
     | Parsetree.RF_rec -> Env.RC_rec Env.RPK_other
    ) in
  let in_recursive_let_section_of =
    if rec_status <> Env.RC_non_rec then  (* Is rec. *)
      List.map
        (fun b -> b.Parsetree.ast_desc.Parsetree.b_name)
        let_def.Parsetree.ast_desc.Parsetree.ld_bindings
    else [] in
  Format.fprintf out_fmter "@[<2>" ;
  (* Recover pre-compilation info and extended environment in case of
     recursivity for all the bindings. *)
  let (env, pre_comp_infos) =
    Expr_dk_generation.pre_compute_let_bindings_infos_for_rec
      ~rec_status ~toplevel: true env
      let_def.Parsetree.ast_desc.Parsetree.ld_bindings in
  (* Now generate each bound definition. Remark that there is no local idents
     in the scope because we are at toplevel. In the same way, because we are
     not under the scope of a species, the way "Self" must be printed is
     non-relevant. We use [SMS_from_species] by default. *)
  let env' =
    (match (let_def.Parsetree.ast_desc.Parsetree.ld_bindings, pre_comp_infos)
    with
     | ([], _) ->
         (* The "let" construct should always at least bind one identifier ! *)
         assert false
     | ([one_bnd], [one_pre_comp_info]) ->
         let_binding_compile
           ctx ~local_idents: []
           ~in_recursive_let_section_of
           (* Or whatever since "Self" does not exist anymore. *)
           ~self_methods_status: Expr_dk_generation.SMS_from_record
           ~recursive_methods_status: Expr_dk_generation.RMS_regular
           ~toplevel: true ~rec_status env one_bnd one_pre_comp_info
     | ((first_bnd :: next_bnds),
        (first_pre_comp_info :: next_pre_comp_infos)) ->
         let accu_env =
           ref
             (let_binding_compile
                ctx ~local_idents: []
                ~in_recursive_let_section_of
                (* Or whatever since "Self" does not exist anymore. *)
                ~self_methods_status:
                  Expr_dk_generation.SMS_from_record
                ~recursive_methods_status:
                  Expr_dk_generation.RMS_regular
                ~toplevel: true ~rec_status env first_bnd
                first_pre_comp_info) in
         List.iter2
           (fun binding pre_comp_info ->
             Format.fprintf out_fmter "@]@\n@[<2>" ;
             accu_env :=
               let_binding_compile
                 ctx ~local_idents: []
                 ~in_recursive_let_section_of
                 (* Or whatever since "Self" does not exist anymore. *)
                 ~self_methods_status:
                   Expr_dk_generation.SMS_from_record
                 ~recursive_methods_status:
                   Expr_dk_generation.RMS_regular
                 ~toplevel: true ~rec_status !accu_env binding pre_comp_info)
           next_bnds next_pre_comp_infos ;
         !accu_env
     | (_, _) ->
         (* Case where we would not have the same number og pre-compiled infos
            and of bindings. Should never happen. *)
         assert false) in
  Format.fprintf out_fmter "@]" ;
  env'
;;

