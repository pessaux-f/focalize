(***********************************************************************)
(*                                                                     *)
(*                        FoCaLiZe compiler                            *)
(*                                                                     *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                                                                     *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: rec_let_gen.ml,v 1.22 2012-02-10 11:00:14 pessaux Exp $ *)

let is_recursive_call ctx ~local_idents recursive_name expr_ident =
  match expr_ident.Parsetree.ast_desc with
   | Parsetree.EI_local vname ->
       (begin
       if (List.exists
             (fun species_param ->
               match species_param with
                | Env.TypeInformation.SPAR_in (vn, _, _) -> vn = vname
                | Env.TypeInformation.SPAR_is ((_, vn), _, _, _, _) ->
                    (Parsetree.Vuident vn) = vname)
             ctx.Context.scc_species_parameters_names) &&
          (not (List.mem vname local_idents)) then
         false             (* In fact, a species "IN"-parameter. *)
       else
         (* Really a local identifier or a call to a recursive method. *)
         vname = recursive_name
       end)
   | Parsetree.EI_global _ | Parsetree.EI_method (_, _) -> false
;;



let transform_recursive_calls_args_into_tuple ctx ~local_idents recursive_name
    initial_expr =
  let rec rec_transform_expr expr =
    let (new_desc, recursive_call_found) =
      match expr.Parsetree.ast_desc with
       | Parsetree.E_self | Parsetree.E_const _ | Parsetree.E_external _ ->
           (expr.Parsetree.ast_desc, false)
       | Parsetree.E_var expr_ident ->
           let rec_found =
             is_recursive_call ctx ~local_idents recursive_name expr_ident in
           (expr.Parsetree.ast_desc, rec_found)
       | Parsetree.E_fun (args, body) ->
           let (body', rec_found) = rec_transform_expr body in
           if rec_found then failwith "Tupling too complex" ;
           ((Parsetree.E_fun (args, body')), false)
       | Parsetree.E_app (e, args) ->
           (begin
           let args' =
             List.map
               (fun ex ->
                 let (ex', rec_found) = rec_transform_expr ex in
                 if rec_found then failwith "Tupling too complex" ;
                 ex')
               args in
           let (e', rec_found) = rec_transform_expr e in
           let args'' =
             if rec_found then
               (begin
               let tupled_args_desc = Parsetree.E_tuple args' in
               [ { Parsetree.ast_loc = Location.none ;
                   Parsetree.ast_desc = tupled_args_desc ;
                   Parsetree.ast_annot = [] ;
                   Parsetree.ast_type = Parsetree.ANTI_irrelevant } ]
               end)
             else args' in
           ((Parsetree.E_app (e', args'')), false)
           end)
       | Parsetree.E_constr (cident, args) ->
           let args' =
             List.map
               (fun ex ->
                 let (ex', rec_found) = rec_transform_expr ex in
                 if rec_found then failwith "Tupling too complex" ;
                 ex')
               args in
           ((Parsetree.E_constr (cident, args')), false)
       | Parsetree.E_match (e, pats_exprs) ->
           let pats_exprs' =
             List.map
               (fun (p, e) ->
                 let (e', rec_found) = rec_transform_expr e in
                 if rec_found then failwith "Tupling too complex" ;
                 (p, e'))
               pats_exprs in
           let (e', rec_found) = rec_transform_expr e in
           if rec_found then failwith "Tupling too complex" ;
           ((Parsetree.E_match (e', pats_exprs')), false)
       | Parsetree.E_if (e1, e2, e3) ->
           let (e1', rec_found1) = rec_transform_expr e1 in
           if rec_found1 then failwith "Tupling too complex" ;
           let (e2', rec_found2) = rec_transform_expr e2 in
           if rec_found2 then failwith "Tupling too complex" ;
           let (e3', rec_found3) = rec_transform_expr e3 in
           if rec_found3 then failwith "Tupling too complex" ;
           ((Parsetree.E_if (e1', e2', e3')), false)
       | Parsetree.E_let (let_def, e) ->
           let let_def' = transform_let_def let_def in
           let (e', rec_found) = rec_transform_expr e in
           ((Parsetree.E_let (let_def', e')), rec_found)
       | Parsetree.E_record labs_exprs ->
           let labs_exprs' =
             List.map
               (fun (lbl, e) ->
                 let (e', rec_found) = rec_transform_expr e in
                 if rec_found then failwith "Tupling too complex" ;
                 (lbl, e'))
               labs_exprs in
           ((Parsetree.E_record labs_exprs'), false)
       | Parsetree.E_record_access (e, lbl) ->
           let (e', rec_found) = rec_transform_expr e in
           (* A record expression can never be a function ! *)
           assert (rec_found = false) ;
           ((Parsetree.E_record_access (e', lbl)), false)
       | Parsetree.E_record_with (e, labs_exprs) ->
           let (e', rec_found) = rec_transform_expr e in
           (* A record expression can never be a function ! *)
           assert (rec_found = false) ;
           let labs_exprs' =
             List.map
               (fun (lbl, ex) ->
                 let (ex', rec_f) = rec_transform_expr ex in
                 if rec_f then failwith "Tupling too complex" ;
                 (lbl, ex'))
               labs_exprs in
           ((Parsetree.E_record_with (e', labs_exprs')), false)
       | Parsetree.E_tuple exprs ->
           let exprs' =
             List.map
               (fun e ->
                 let (e', rec_found) = rec_transform_expr e in
                 if rec_found then failwith "Tupling too complex" ;
                 e')
               exprs in
           ((Parsetree.E_tuple exprs'), false)
       | Parsetree.E_sequence exprs ->
           let exprs' =
             List.map
               (fun e ->
                 let (e', rec_found) = rec_transform_expr e in
                 if rec_found then failwith "Sequence too complex" ;
                 e')
               exprs in
           ((Parsetree.E_sequence exprs'), false)
       | Parsetree.E_paren e ->
           let (e', rec_found) = rec_transform_expr e in
           ((Parsetree.E_paren e'), rec_found) in
    ({ initial_expr with Parsetree.ast_desc = new_desc }, recursive_call_found)



  and rec_transform_logical_expr logical_expr =
    let new_desc =
      (match logical_expr.Parsetree.ast_desc with
       | Parsetree.Pr_forall (vnames, type_expr, lexpr) ->
           Parsetree.Pr_forall
             (vnames, type_expr, (rec_transform_logical_expr lexpr))
       | Parsetree.Pr_exists (vnames, type_expr, lexpr) ->
           Parsetree.Pr_exists
             (vnames, type_expr, (rec_transform_logical_expr lexpr))
       | Parsetree.Pr_imply (lexpr1, lexpr2) ->
           let lexpr1' = rec_transform_logical_expr lexpr1 in
           let lexpr2' = rec_transform_logical_expr lexpr2 in
           Parsetree.Pr_imply (lexpr1', lexpr2')
       | Parsetree.Pr_or (lexpr1, lexpr2) ->
           let lexpr1' = rec_transform_logical_expr lexpr1 in
           let lexpr2' = rec_transform_logical_expr lexpr2 in
           Parsetree.Pr_or (lexpr1', lexpr2')
       | Parsetree.Pr_and (lexpr1, lexpr2) ->
           let lexpr1' = rec_transform_logical_expr lexpr1 in
           let lexpr2' = rec_transform_logical_expr lexpr2 in
           Parsetree.Pr_and (lexpr1', lexpr2')
       | Parsetree.Pr_equiv (lexpr1, lexpr2) ->
           let lexpr1' = rec_transform_logical_expr lexpr1 in
           let lexpr2' = rec_transform_logical_expr lexpr2 in
           Parsetree.Pr_equiv (lexpr1', lexpr2')
       | Parsetree.Pr_not lexpr ->
           Parsetree.Pr_not (rec_transform_logical_expr lexpr)
       | Parsetree.Pr_expr expr ->
           let (expr', _) = rec_transform_expr expr in
           Parsetree.Pr_expr expr'
       | Parsetree.Pr_paren lexpr ->
           Parsetree.Pr_paren (rec_transform_logical_expr lexpr)) in
    { logical_expr with Parsetree.ast_desc = new_desc }
           


  and transform_binding_body body =
    match body with
     | Parsetree.BB_logical lexpr ->
         Parsetree.BB_logical (rec_transform_logical_expr lexpr)
     | Parsetree.BB_computational expr ->
         let (expr', _) = rec_transform_expr expr in
         Parsetree.BB_computational expr'



  and transform_binding bnd =
    let new_body =
      transform_binding_body bnd.Parsetree.ast_desc.Parsetree.b_body in
    let new_desc = {
      bnd.Parsetree.ast_desc with Parsetree.b_body = new_body } in
    { bnd with Parsetree.ast_desc = new_desc }



  and transform_let_def let_def =
    let new_bindings =
      List.map
        transform_binding let_def.Parsetree.ast_desc.Parsetree.ld_bindings in
    let new_desc = {
      let_def.Parsetree.ast_desc with Parsetree.ld_bindings = new_bindings } in
    { let_def with Parsetree.ast_desc = new_desc } in

  (* ************** *)
  (* Finally, the body of [transform_recursive_calls_args_into_tuple], simply
     calling [rec_transform_expr] on the [initial_expr]. *)
  fst (rec_transform_expr initial_expr)
;;



let generate_binding_match ctx print_ctx env expr pattern =
  let out_fmter = ctx.Context.scc_out_fmter in
  let local_idents = Parsetree_utils.get_local_idents_from_pattern pattern in
  (* Now, generate "pattern = expr". But attention !!! We print something like
     a pattern, but not a pattern ! Since we are not in the case of a pattern
     in a match, we must apply the possible polymorphic arguments of the sum
     value constructors! So, force the extra "_"s to be printed. *)
  Species_record_type_generation.generate_pattern
    ~force_polymorphic_explicit_args: true ctx print_ctx env pattern ;
  Format.fprintf out_fmter " =@ " ;
  Species_record_type_generation.generate_expr
    ctx ~in_recursive_let_section_of: [] ~local_idents
    ~self_methods_status: Species_record_type_generation.SMS_abstracted
    ~recursive_methods_status: Species_record_type_generation.RMS_regular
    ~gen_vars_in_scope: [] env expr
;;



let generate_binding_let ctx print_ctx env binding =
  let out_fmter = ctx.Context.scc_out_fmter in
  let binding_desc = binding.Parsetree.ast_desc in
  (* Quantification of the variable was done previously by the function
     [generate_variables_quantifications]. *)
  Format.fprintf out_fmter "%a =@ "
    Parsetree_utils.pp_vname_with_operators_expanded
    binding_desc.Parsetree.b_name ;
  (* If the binding has arguments, then it's a function. So for a binding
     looking like "let f (x, y) = ..." we generate
     "x = (fun x => fun y => ...)". *)
  let local_idents =
    if binding_desc.Parsetree.b_params <> [] then
      (begin
      (* It is pretty like [let_binding_compile] in the file
         [species_record_type_generation.ml]. *)
      let params_names = List.map fst binding_desc.Parsetree.b_params in
      (* Recover the type scheme of the bound ident. *)
      let def_scheme =
        (match binding.Parsetree.ast_type with
         | Parsetree.ANTI_none | Parsetree.ANTI_irrelevant
         | Parsetree.ANTI_type _ -> assert false
         | Parsetree.ANTI_scheme s -> s) in
      (* We do not have anymore information about "Self"'s structure... *)
      let (params_with_type, _, generalized_instanciated_vars) =
        MiscHelpers.bind_parameters_to_types_from_type_scheme
          ~self_manifest: None ~gen_vars_in_scope: [] (Some def_scheme)
          params_names in
      Format.fprintf out_fmter "(@[<1>" ;
      (* If the original scheme is polymorphic, then we must ad extra Coq
         parameters of type "Set" for each of the generalized variables. *)
      List.iter
        (fun (_, var) ->
           Format.fprintf out_fmter "fun (%a : Set) =>@ "
            (Types.pp_type_simple_to_coq print_ctx ~reuse_mapping: true)
            var)
        generalized_instanciated_vars ;
      (* Now, generate each of the real function's parameter with its type. *)
      List.iter
        (fun (param_vname, pot_param_ty) ->
          match pot_param_ty with
           | Some param_ty ->
               Format.fprintf out_fmter "fun (%a : %a) =>@ "
                 Parsetree_utils.pp_vname_with_operators_expanded param_vname
                 (Types.pp_type_simple_to_coq print_ctx ~reuse_mapping: true)
                 param_ty
           | None ->
               (* Because we provided a type scheme to the function
                  [bind_parameters_to_types_from_type_scheme], MUST get one type
                  for each parameter name ! *)
               assert false)
        params_with_type ;
        params_names
      end)
    else
      [ (* No local identifiers since no parameters to the let binding. *) ] in
  (* Now, in any case, we print the body of the let binding. *)
  (match binding_desc.Parsetree.b_body with
   | Parsetree.BB_computational e ->
       Species_record_type_generation.generate_expr
         ctx ~in_recursive_let_section_of: [] ~local_idents
         ~self_methods_status: Species_record_type_generation.SMS_abstracted
         ~recursive_methods_status: Species_record_type_generation.RMS_regular
         ~gen_vars_in_scope: [] env e
   | Parsetree.BB_logical p ->
       Species_record_type_generation.generate_logical_expr
         ctx ~in_recursive_let_section_of: [] ~local_idents
         ~self_methods_status: Species_record_type_generation.SMS_abstracted
         ~recursive_methods_status: Species_record_type_generation.RMS_regular
         ~gen_vars_in_scope: [] env p) ;
  (* If there were parameters, we must close a parenthesis. *)
  if binding_desc.Parsetree.b_params <> [] then
    Format.fprintf out_fmter "@]"
;;



let generate_variables_as_tuple out_fmter vars =
  let rec rec_gen = function
    | [] -> assert false
    | [(last, _)] ->
        Format.fprintf out_fmter "%a"
          Parsetree_utils.pp_vname_with_operators_expanded last
    | (h, _) :: q -> 
        Format.fprintf out_fmter "%a,@ "
          Parsetree_utils.pp_vname_with_operators_expanded h ;
        rec_gen q in
  (* *********************** *)
  (* Now, really do the job. *)
  match vars with
   | [] -> assert false
   | [(one, _)] ->
       Format.fprintf out_fmter "%a"
         Parsetree_utils.pp_vname_with_operators_expanded one
   | _ ->
       Format.fprintf out_fmter "(" ;
       rec_gen vars ;
       Format.fprintf out_fmter ")"
;;



let generate_variables_quantifications out_fmter print_ctx vars bindings =
  List.iter
    (fun (v, ty) ->
      Format.fprintf out_fmter "forall %a : %a,@ "
        Parsetree_utils.pp_vname_with_operators_expanded v
        (Types.pp_type_simple_to_coq print_ctx ~reuse_mapping: true) ty)
    vars ;
  (* Now, quantify the variables bound in the bindings. *)
  List.iter
    (function
      | Recursion.B_let binding ->
          (begin
          (* Generate a forall to bind the identifier. *)
          let scheme =
            (match binding.Parsetree.ast_type with
             | Parsetree.ANTI_none | Parsetree.ANTI_irrelevant
             | Parsetree.ANTI_type _ -> assert false
             | Parsetree.ANTI_scheme s -> s) in
          let ty = Types.specialize scheme in
          Format.fprintf out_fmter "forall %a :@ %a,@ "
            Parsetree_utils.pp_vname_with_operators_expanded
            binding.Parsetree.ast_desc.Parsetree.b_name
            (Types.pp_type_simple_to_coq print_ctx ~reuse_mapping: true) ty
          end)
      | Recursion.B_match (_, pattern) ->
          (begin
          let bound_vars =
            Parsetree_utils.get_local_idents_and_types_from_pattern pattern in
          (* Generate a forall for each bound variable. *)
          List.iter
            (fun (v, ty_info) ->
              let t =
                (match ty_info with
                 | Parsetree.ANTI_type t -> t
                 | _ -> assert false) in
              Format.fprintf out_fmter "forall %a :@ %a,@ "
                Parsetree_utils.pp_vname_with_operators_expanded v
                (Types.pp_type_simple_to_coq print_ctx ~reuse_mapping: true) t)
            bound_vars
          end)
      | Recursion.B_condition (_, _) ->
          (* No possible variable bound, so nothing to do. *)
          ())
    bindings
;;



let generate_exprs_as_tuple ctx env exprs =
  match exprs with
   | [] -> assert false
   | [one] ->
       Species_record_type_generation.generate_expr
         ctx ~in_recursive_let_section_of: [] ~local_idents: []
         ~self_methods_status: Species_record_type_generation.SMS_abstracted
         ~recursive_methods_status: Species_record_type_generation.RMS_regular
         ~gen_vars_in_scope: [] env one
   | _ ->
       let fake_tuple_desc = Parsetree.E_tuple exprs in
       let fake_tuple = {
         Parsetree.ast_loc = Location.none ;
         Parsetree.ast_desc = fake_tuple_desc ;
         Parsetree.ast_annot = [] ;
         Parsetree.ast_type = Parsetree.ANTI_irrelevant } in
       Species_record_type_generation.generate_expr
         ctx ~in_recursive_let_section_of: [] ~local_idents: []
         ~self_methods_status: Species_record_type_generation.SMS_abstracted
         ~recursive_methods_status: Species_record_type_generation.RMS_regular
         ~gen_vars_in_scope: [] env fake_tuple
;;



(** [explicit_order] : Serves to use this function for both the code generation
    in the Section of the recursive function and outside.
    In the first case, the order is represented by a Variable ("__term_order")
    with no need to apply any argument since abstractions are done by Variables
    in the Section.
    In the second case, the order is the really defined one and is called by
    function's name + "_wforder". In this second case, since we manage
    ourselves explicitely the abstractions and we are not in a Section, we must
    explicitely apply the order to the stuff it needs due to extra arguments
    induced by the various dependencies. *)
let generate_termination_lemmas ctx print_ctx env ~explicit_order
    recursive_calls =
  let out_fmter = ctx.Context.scc_out_fmter in
  List.iter
    (fun (n_exprs, bindings) ->
      (* The list of hypotheses induced by bindings is in *reverse order*.
         Let's reverse it. *)
      let bindings = List.rev bindings in
      Format.fprintf out_fmter "(" ;
      (* [n_exprs]: (initial variable of the function * expression provided
         in the recursive call). The expression must hence be < to the initial
         variable for the function to terminate. In fact that's the tuple of
         initial variables that must be < to the tuple of expressions provided
         in the recursive call. *)
      let (initial_vars, rec_args) = List.split n_exprs in
      (* For each variable, bind it by a forall. *)
      generate_variables_quantifications
        out_fmter print_ctx initial_vars bindings ;
      (* We must generate the hypotheses and separate them by ->. *)
      List.iter
        (function
          | Recursion.B_let let_binding ->
              (* A "let x = e" induces forall x: ..., x "=" e.
                 [Unsure] Alpha-conv !!!!!!! *)
              Format.fprintf out_fmter "(" ;
              generate_binding_let ctx print_ctx env let_binding ;
              Format.fprintf out_fmter ") ->@ " ;
          | Recursion.B_match (expr, pattern) ->
              (* Induces "forall variables of the pattern, pattern = expr".
                 [Unsure] Alpha-conv des vars du pattern !!!!!!! *)
              Format.fprintf out_fmter "(" ;
              generate_binding_match ctx print_ctx env expr pattern ;
              Format.fprintf out_fmter ") ->@ " ;
          | Recursion.B_condition (expr, bool_val) ->
              (* Induces "Is_true (expr)" if [bool_val] is true, else
                 "~ Is_true (expr)" if [bool_val] is false. *)
              if not bool_val then Format.fprintf out_fmter "~@ " ;
              Format.fprintf out_fmter "Is_true (@[<1>" ;
              Species_record_type_generation.generate_expr
                ctx ~in_recursive_let_section_of: [] ~local_idents: []
                ~self_methods_status:
                  Species_record_type_generation.SMS_abstracted
                ~recursive_methods_status:
                  Species_record_type_generation.RMS_regular
                ~gen_vars_in_scope: [] env expr ;
              Format.fprintf out_fmter "@]) ->@ ")
        bindings ;
      (* Now, generate the expression that telling the decreasing applying
         the "__term_order" Variable or the really defined order if we were
         provided one by the argument [~explicit_order] with its arguments to
         apply due to lambda-liftings. *)
      (match explicit_order with
       | None ->
           Format.fprintf out_fmter "__term_order@ "
       | Some (fun_name, ai, sorted_deps_from_params, abstracted_methods) ->
           Format.fprintf out_fmter "(%a_wforder"
             Parsetree_utils.pp_vname_with_operators_expanded fun_name ;
           Species_record_type_generation.generate_method_lambda_lifted_arguments
             ~only_for_Self_meths: false out_fmter ai sorted_deps_from_params
             abstracted_methods ;
           Format.fprintf out_fmter ")@ ") ;
      (* Now, generate the arguments to provide to the order. *)
      generate_exprs_as_tuple ctx env rec_args ;
      Format.fprintf out_fmter "@ " ;
      (* Generate a tuple of all the variables. *)
      generate_variables_as_tuple out_fmter initial_vars ;
      Format.fprintf out_fmter ")@\n/\\@\n")
    (* Connected by ^'s. *)
    recursive_calls
;;
