(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            François Pessaux                                                *)
(*            Pierre Weis                                                     *)
(*            Damien Doligez                                                  *)
(*                                                                            *)
(*                               LIP6  --  INRIA Rocquencourt                 *)
(*                                                                            *)
(*  Copyright 2007 - 2012 LIP6 and INRIA                                      *)
(*            2012 ENSTA ParisTech                                            *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)


(* ************************************************************************** *)
(** {b Descr} This mmodule contains utilities for recursive functions code
    generation in Coq. It does *not* contain the core of generation code.     *)
(* ************************************************************************** *)



let is_recursive_call ctx ~current_unit ~local_idents recursive_name
    expr_ident =
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
   | Parsetree.EI_global qvn -> (
       match qvn with
       | Parsetree.Vname vname -> vname = recursive_name
       | Parsetree.Qualified (unit_name, vname) ->
           (* In case of toplevel recursive function, the name of the function
              is a global ident. Check that we are really talking of the ident
              of the same compilation unit than the one we are currently
              processing. *)
           vname = recursive_name && unit_name = current_unit
       )
   | Parsetree.EI_method (_, _) -> false
;;



let transform_recursive_calls_args_into_tuple ctx ~current_unit ~local_idents
    recursive_name
    initial_expr =
  let rec rec_transform_expr expr =
    let (new_desc, recursive_call_found) =
      match expr.Parsetree.ast_desc with
       | Parsetree.E_self | Parsetree.E_const _ | Parsetree.E_external _ ->
           (expr.Parsetree.ast_desc, false)
       | Parsetree.E_var expr_ident ->
           let rec_found =
             is_recursive_call
               ctx ~current_unit ~local_idents recursive_name expr_ident in
           (expr.Parsetree.ast_desc, rec_found)
       | Parsetree.E_fun (args, body) ->
           let (body', rec_found) = rec_transform_expr body in
           if rec_found then failwith "Tupling too complex" ;
           ((Parsetree.E_fun (args, body')), false)
       | Parsetree.E_app (e, args) -> (
           let args' =
             List.map
               (fun ex ->
                 let (ex', rec_found) = rec_transform_expr ex in
                 if rec_found then failwith "Tupling too complex" ;
                 ex')
               args in
           let (e', rec_found) = rec_transform_expr e in
           let args'' =
             if rec_found then (
               let tupled_args_desc = Parsetree.E_tuple args' in
               [ { Parsetree.ast_loc = Location.none ;
                   Parsetree.ast_desc = tupled_args_desc ;
                   Parsetree.ast_annot = [] ;
                   Parsetree.ast_type = Parsetree.ANTI_irrelevant } ]
              )
             else args' in
           ((Parsetree.E_app (e', args'')), false)
          )
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
  (* Now, generate "Is_true (basics._equal_ _ expr pattern" to keep consistent
     with the way we generate equalities,. But attention !!! We print
     something like a pattern, but not a pattern ! Since we are not in the
     case of a pattern in a match, we must apply the possible polymorphic
     arguments of the sum value constructors! So, force the extra "_"s to be
     printed. *)
  Format.fprintf out_fmter "Is_true ((basics._equal_ _) (" ;
  Species_record_type_coq_generation.generate_expr
    ctx ~in_recursive_let_section_of: [] ~local_idents
    ~self_methods_status: Species_record_type_coq_generation.SMS_abstracted
    ~recursive_methods_status: Species_record_type_coq_generation.RMS_regular
    env expr ;
  Format.fprintf out_fmter ")@ (" ;
  Species_record_type_coq_generation.generate_pattern
    ~force_polymorphic_explicit_args: true ctx print_ctx env pattern ;
  Format.fprintf out_fmter "))"
;;



let generate_binding_let ctx print_ctx env binding =
  let out_fmter = ctx.Context.scc_out_fmter in
  let binding_desc = binding.Parsetree.ast_desc in
  (* Quantification of the variable was done previously by the function
     [generate_variables_quantifications].
     To keep consistent with the way we generate equalities, do not directly
     use a =. Instead, use the regular scheme with Is_true and
     basics._equal_. *)
  Format.fprintf out_fmter "Is_true ((basics._equal_ _) " ;
  (* If the binding has arguments, then it's a function. So for a binding
     looking like "let f (x, y) = ..." we generate
     "x = (fun x => fun y => ...)". *)
  let local_idents =
    if binding_desc.Parsetree.b_params <> [] then
      (begin
      (* It is pretty like [let_binding_compile] in the file
         [species_record_type_coq_generation.ml]. *)
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
          ~self_manifest: None (Some def_scheme) params_names in
      Format.fprintf out_fmter "(@[<1>" ;
      (* If the original scheme is polymorphic, then we must ad extra Coq
         parameters of type "Set" for each of the generalized variables. *)
      List.iter
        (fun var ->
           Format.fprintf out_fmter "fun (%a : Set) =>@ "
            Coq_pprint.pp_type_variable_to_coq var)
        generalized_instanciated_vars ;
      (* Now, generate each of the real function's parameter with its type. *)
      List.iter
        (fun (param_vname, pot_param_ty) ->
          match pot_param_ty with
           | Some param_ty ->
               Format.fprintf out_fmter "fun (%a : %a) =>@ "
                 Parsetree_utils.pp_vname_with_operators_expanded param_vname
                 (Coq_pprint.pp_type_simple_to_coq print_ctx)
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
       Species_record_type_coq_generation.generate_expr
         ctx ~in_recursive_let_section_of: [] ~local_idents
         ~self_methods_status: Species_record_type_coq_generation.SMS_abstracted
         ~recursive_methods_status:
           Species_record_type_coq_generation.RMS_regular
         env e
   | Parsetree.BB_logical p ->
       Species_record_type_coq_generation.generate_logical_expr
         ctx ~in_recursive_let_section_of: [] ~local_idents
         ~self_methods_status: Species_record_type_coq_generation.SMS_abstracted
         ~recursive_methods_status:
           Species_record_type_coq_generation.RMS_regular
         env p) ;
  (* If there were parameters, we must close a parenthesis. *)
  if binding_desc.Parsetree.b_params <> [] then
    Format.fprintf out_fmter "@])" ;
  (* The bound variable (after, like Coq does). *)
  Format.fprintf out_fmter "@ %a)"
    Parsetree_utils.pp_vname_with_operators_expanded
    binding_desc.Parsetree.b_name
;;



let generate_variables_as_tuple out_fmter vars =
  let rec rec_coq_gen = function
    | [] -> assert false
    | [(last, _)] ->
        Format.fprintf out_fmter "%a"
          Parsetree_utils.pp_vname_with_operators_expanded last
    | (h, _) :: q ->
        Format.fprintf out_fmter "%a,@ "
          Parsetree_utils.pp_vname_with_operators_expanded h ;
        rec_coq_gen q in
  (* *********************** *)
  (* Now, really do the job. *)
  match vars with
   | [] -> assert false
   | [(one, _)] ->
       Format.fprintf out_fmter "%a"
         Parsetree_utils.pp_vname_with_operators_expanded one
   | _ ->
       Format.fprintf out_fmter "(" ;
       rec_coq_gen vars ;
       Format.fprintf out_fmter ")"
;;



let generate_variables_quantifications out_fmter print_ctx vars bindings =
  List.iter
    (fun (v, ty) ->
      Format.fprintf out_fmter "forall %a : %a,@ "
        Parsetree_utils.pp_vname_with_operators_expanded v
        (Coq_pprint.pp_type_simple_to_coq print_ctx) ty)
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
            (Coq_pprint.pp_type_simple_to_coq print_ctx) ty
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
                (Coq_pprint.pp_type_simple_to_coq print_ctx) t)
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
       Species_record_type_coq_generation.generate_expr
         ctx ~in_recursive_let_section_of: [] ~local_idents: []
         ~self_methods_status: Species_record_type_coq_generation.SMS_abstracted
         ~recursive_methods_status:
           Species_record_type_coq_generation.RMS_regular
         env one
   | _ ->
       let fake_tuple_desc = Parsetree.E_tuple exprs in
       let fake_tuple = {
         Parsetree.ast_loc = Location.none ;
         Parsetree.ast_desc = fake_tuple_desc ;
         Parsetree.ast_annot = [] ;
         Parsetree.ast_type = Parsetree.ANTI_irrelevant } in
       Species_record_type_coq_generation.generate_expr
         ctx ~in_recursive_let_section_of: [] ~local_idents: []
         ~self_methods_status: Species_record_type_coq_generation.SMS_abstracted
         ~recursive_methods_status:
           Species_record_type_coq_generation.RMS_regular
         env fake_tuple
;;



type termination_expr_kind =
  | TEK_order of Parsetree.expr    (** Expression denotes an order. *)
  | TEK_measure of Parsetree.expr  (** Expression denotes a measure. *)
;;



(** {b Descr}: Represent the 2 kinds of order to generate to argue the
    decreasing of a recursive function. Depending on the point of view,
    different code has to be emitted. In the first case we are dealing with
    the "user order" or the "user measure". In the second we are dealing
    with the real "Function" order, i.e. the one that Function needs. *)
type order_kind =
  | OK_expr of (termination_expr_kind * int) (** Case of the
         "user-order". It only deals with the function's argument involved in
         the decreasing of the recursing. The user specifies his order on only
         the unique argument he is interested in. So the order to generate is
         directly coming from the expression given as "order" by the user. The
         int is the indiex of the function's parameter used by this
         "user-order". *)
  | OK_wfounded of   (** Case of the "Function-order", i.e. the one that the
         Coq construct Function expects to prove correct recursive definition.
         This order uses all the arguments of the function and is made of
         the "xxx_wforder" generated applied to its dependencies. Hence in
         this case we need dependency information. *)
      (Parsetree.vname *
       (Parsetree.vname list) *
       ((Env.TypeInformation.species_param *
         Env.ordered_methods_from_params) list) *
       Parsetree.vname list)
;;



(** [explicit_order] : Serves to use this function for code generation
    of the user-side and the Function-side termination theorems.
    In the first case, the order is the one provided by the user in its
    termination proof, using order or measure. It doesn't deal with all the
    function's arguments but only the ones involved in the recursion decreasing.
    This order expression is then eta-expanded and wrapped into a Is_true.
    In the second case, the order is a generated one and is called by
    function's name + "_wforder". It deals with all the function's arguments! *)
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
              (* A "let x = e" induces forall x: ...,
                 Is_true (basics._equal_ _ e x).
                 [Unsure] Alpha-conv !!!!!!! *)
              Format.fprintf out_fmter "(" ;
              generate_binding_let ctx print_ctx env let_binding ;
              Format.fprintf out_fmter ") ->@ "
          | Recursion.B_match (expr, pattern) ->
              (* Induces "forall variables of the pattern,
                 Is_true (basics._equal_ _ expr pattern)".
                 [Unsure] Alpha-conv des vars du pattern !!!!!!! *)
              Format.fprintf out_fmter "(" ;
              generate_binding_match ctx print_ctx env expr pattern ;
              Format.fprintf out_fmter ") ->@ "
          | Recursion.B_condition (expr, bool_val) ->
              (* Induces "Is_true (expr)" if [bool_val] is true, else
                 "~ Is_true (expr)" if [bool_val] is false. *)
              if not bool_val then Format.fprintf out_fmter "~@ " ;
              Format.fprintf out_fmter "Is_true (@[<1>" ;
              Species_record_type_coq_generation.generate_expr
                ctx ~in_recursive_let_section_of: [] ~local_idents: []
                ~self_methods_status:
                  Species_record_type_coq_generation.SMS_abstracted
                ~recursive_methods_status:
                  Species_record_type_coq_generation.RMS_regular
                env expr ;
              Format.fprintf out_fmter "@]) ->@ ")
        bindings ;
      (* Now, generate the expression that states the decreasing applying
         the "user-" or "Function-" order depending the argument
         [~explicit_order]. In case of "Function-" order, we have to apply it
         to its arguments coming from to lambda-liftings. *)
      (match explicit_order with
       | OK_expr ((TEK_order expr_order), rec_fun_used_arg_index) ->
           (* Case of a proof by an order.
              Surround by a Is_true since the user order can only be a
              function returning a bool, hence must be plunged into Prop. *)
           Format.fprintf out_fmter "Is_true ((@[<1>" ;
           Species_record_type_coq_generation.generate_expr
             ctx ~in_recursive_let_section_of: [] ~local_idents: []
             ~self_methods_status:
               Species_record_type_coq_generation.SMS_abstracted
             ~recursive_methods_status:
               Species_record_type_coq_generation.RMS_regular env expr_order ;
           Format.fprintf out_fmter "@])@ " ;
           (* Now, generate the only argument used in the order given by the
              user to provide to this order. *)
           let rec_arg = List.nth rec_args rec_fun_used_arg_index in
           let initial_var = List.nth initial_vars rec_fun_used_arg_index in
           (* Generate the corresponding recursive call argument. *)
           Species_record_type_coq_generation.generate_expr
             ctx ~in_recursive_let_section_of: [] ~local_idents: []
             ~self_methods_status:
               Species_record_type_coq_generation.SMS_abstracted
             ~recursive_methods_status:
               Species_record_type_coq_generation.RMS_regular env rec_arg ;
           (* Generate the initial argument of the function. *)
           Format.fprintf out_fmter "@ %a"
             Parsetree_utils.pp_vname_with_operators_expanded
             (fst initial_var) ;
           (* Close the surrounding Is_true. *)
           Format.fprintf out_fmter "@])"
       | OK_expr ((TEK_measure expr_mea), rec_fun_used_arg_index) ->
           (* Case of a proof by a measure.
              Surround by a Is_true since < returns a bool, hence must be
              plunged into Prop. *)
           Format.fprintf out_fmter "Is_true (@[<1>basics._lt_@ (" ;
           (* Call the measure on the first argument. *)
           Species_record_type_coq_generation.generate_expr
             ctx ~in_recursive_let_section_of: [] ~local_idents: []
             ~self_methods_status:
               Species_record_type_coq_generation.SMS_abstracted
             ~recursive_methods_status:
               Species_record_type_coq_generation.RMS_regular env expr_mea ;
           let rec_arg = List.nth rec_args rec_fun_used_arg_index in
           let initial_var = List.nth initial_vars rec_fun_used_arg_index in
           (* Put the first argument (recursive call argument). *)
           Format.fprintf out_fmter "@ " ;
           Species_record_type_coq_generation.generate_expr
             ctx ~in_recursive_let_section_of: [] ~local_idents: []
             ~self_methods_status:
               Species_record_type_coq_generation.SMS_abstracted
             ~recursive_methods_status:
               Species_record_type_coq_generation.RMS_regular env rec_arg ;
           (* Close paren of first application of the measure and open one for
              the second . *)
           Format.fprintf out_fmter ")@ (" ;
           (* Call a seconde time the measure. *)
           Species_record_type_coq_generation.generate_expr
             ctx ~in_recursive_let_section_of: [] ~local_idents: []
             ~self_methods_status:
               Species_record_type_coq_generation.SMS_abstracted
             ~recursive_methods_status:
               Species_record_type_coq_generation.RMS_regular env expr_mea ;
           (* Put the second argument (initial one of the function). *)
           Format.fprintf out_fmter "@ %a"
             Parsetree_utils.pp_vname_with_operators_expanded
             (fst initial_var) ;
           (* Close second app of the measure and the surrounding Is_true. *)
           Format.fprintf out_fmter ")@])"
       | OK_wfounded (fname, ai, sorted_deps_from_params, abstracted_methods) ->
           Format.fprintf out_fmter "(%a_wforder"
             Parsetree_utils.pp_vname_with_operators_expanded fname ;
           Species_record_type_coq_generation.generate_method_lambda_lifted_arguments
             ~only_for_Self_meths: false out_fmter ai sorted_deps_from_params
             abstracted_methods ;
           Format.fprintf out_fmter ")@ " ;
           (* Now, generate the tuples of all the arguments to provide to the
              order as Function expects it. *)
           generate_exprs_as_tuple ctx env rec_args ;
           Format.fprintf out_fmter "@ " ;
           (* Generate a tuple of all the variables. *)
           generate_variables_as_tuple out_fmter initial_vars) ;
      (* Connected by ^'s. *)
      Format.fprintf out_fmter ")@\n/\\@\n")
    recursive_calls
;;



let print_user_variables_quantifications vars bindings =
  List.iter
    (fun (v, ty) ->
      Format.printf "all %a : %a,@ "
        Sourcify.pp_vname v Types.pp_type_simple ty)
    vars ;
  (* Now, quantify the variables bound in the bindings. *)
  List.iter
    (function
      | Recursion.B_let binding -> (
          (* Generate a forall to bind the identifier. *)
          let scheme =
            (match binding.Parsetree.ast_type with
            | Parsetree.ANTI_none | Parsetree.ANTI_irrelevant
            | Parsetree.ANTI_type _ -> assert false
            | Parsetree.ANTI_scheme s -> s) in
          let ty = Types.specialize scheme in
          Format.printf  "all %a :@ %a,@ "
            Sourcify.pp_vname binding.Parsetree.ast_desc.Parsetree.b_name
            Types.pp_type_simple ty
         )
      | Recursion.B_match (_, pattern) -> (
          let bound_vars =
            Parsetree_utils.get_local_idents_and_types_from_pattern pattern in
          (* Generate a forall for each bound variable. *)
          List.iter
            (fun (v, ty_info) ->
              let t =
                (match ty_info with
                | Parsetree.ANTI_type t -> t
                | _ -> assert false) in
              Format.printf "all %a :@ %a,@ "
                Sourcify.pp_vname v Types.pp_type_simple t)
            bound_vars
         )
      | Recursion.B_condition (_, _) ->
          (* No possible variable bound, so nothing to do. *)
          ())
    bindings
;;



let print_user_binding_let binding =
  let binding_desc = binding.Parsetree.ast_desc in
  (* Quantification of the variable was done previously by the function
     [generate_variables_quantifications]. *)
  (* If the binding has arguments, then it's a function. So for a binding
     looking like "let f (x, y) = ... in" we generate the same form of
     obligation since we have no other way to represent the binding. In effect,
     we don't have anomymous lambdas ike those we use to generate the Coq
     version of the obligations. *)
   if binding_desc.Parsetree.b_params <> [] then (
     Format.printf "let@ %a@ in@ " Sourcify.pp_binding binding
   )
   else (
     (* Otherwise, we print the body of the let binding and the bound name will
        arrive after. *)
     Format.printf "(%a"
       Sourcify.pp_binding_body binding_desc.Parsetree.b_body ;
     (* The bound variable (after, like Coq does). *)
     Format.printf "=@ %a)@ ->@ "
       Parsetree_utils.pp_vname_with_operators_expanded
       binding_desc.Parsetree.b_name
    )
;;



(* ************************************************************************ *)
(** {b Descr}: Prints on stdout the obligation proofs the user will have to
    do on its recursive function to prove that its order makes arguments
    decreasing and that the order is well-founded.
    This output is lighter than its Coq version because it outputs
    some Focal source code.

    {b Args}:
    - [fun_params_n_tys]: *All* the parameters of the function with their
      types.

    {b Exported}: Yes.                                                      *)
(* ************************************************************************ *)
let print_user_termination_obls_for_order fun_name fun_params_n_tys
    recursive_calls user_order rec_fun_used_arg_index =
  (* Record the variables of the function appearing in the order
     expression. *)
  let vars_of_order_expr =
    Parsetree_utils.get_free_local_vnames_from_expr_desc
      user_order.Parsetree.ast_desc in
  let params_in_order_expr =
    List.filter
      (fun (n, _) -> List.mem n vars_of_order_expr)
      fun_params_n_tys in
  Format.printf
    "@\n\
    ---------------------------------------------------------------@\n\
    %tTermination proof obligations for the recursive function%t '%t%a%t':@\n"
   Handy.pp_set_bold Handy.pp_reset_effects
   Handy.pp_set_underlined Sourcify.pp_vname fun_name Handy.pp_reset_effects ;
  let counter = ref 1 in
  List.iter
    (fun (n_exprs, bindings) ->
      (* The list of hypotheses induced by bindings is in *reverse order*.
         Let's reverse it. *)
      let bindings = List.rev bindings in
      Format.printf "@[<2><1>%d prove@ " !counter ;
      incr counter ;
      (* [n_exprs]: (initial variable of the function * expression provided
         in the recursive call). The expression must hence be < to the initial
         variable for the function to terminate. In fact that's the tuple of
         initial variables that must be < to the tuple of expressions provided
         in the recursive call. *)
      let (initial_vars, rec_args) = List.split n_exprs in
      (* For each variable, bind it by a forall. *)
      print_user_variables_quantifications initial_vars bindings ;
      (* We must generate the hypotheses and separate them by ->. *)
      List.iter
        (function
          | Recursion.B_let let_binding -> print_user_binding_let let_binding
          | Recursion.B_match (expr, pattern) ->
              Format.printf "(%a@ = %a)@ ->@ "
                Sourcify.pp_pattern pattern Sourcify.pp_expr expr
          | Recursion.B_condition (expr, bool_val) ->
              Format.printf "(%a" Sourcify.pp_expr expr ;
              if not bool_val then Format.printf "@ =@ false" ;
              Format.printf ")@ ->@ ")
        bindings ;
      (* Now, generate the goals that states the decreasing applying
         the "user-"order. *)
      Format.printf "%a@ (" Sourcify.pp_expr user_order ;
      (* Now, generate the only argument used in the order given by the user
         to provide to this order. *)
      let rec_arg = List.nth rec_args rec_fun_used_arg_index in
      let initial_var = List.nth initial_vars rec_fun_used_arg_index in
      (* Generate the corresponding recursive call argument. *)
      Format.printf "%a" Sourcify.pp_expr rec_arg ;
      (* Generate the initial argument of the function. *)
      Format.printf ",@ %a)" Sourcify.pp_vname (fst initial_var) ;
      Format.printf "@]@\n")
    recursive_calls ;
  (* Print the obligation stating the well-foundedness of the order. *)
  Format.printf "@[<2><1>%d prove " !counter ;
  (* Bind the function parameters appearing in the measure expression. *)
  List.iter
    (fun (n, t) ->
      Format.printf "all@ %a@ :@ %a,@ "
        Sourcify.pp_vname n Types.pp_type_simple t)
    params_in_order_expr ;
  Format.printf "is_well_founded@ (%a)@]@\n" Sourcify.pp_expr user_order ;
  (* Print the conclusion step since it is always the same. *)
  Format.printf
    "@[<2><1>e qed coq proof {*wf_qed*}@]@\n\
     ---------------------------------------------------------------@\n"
;;



(* ************************************************************************ *)
(** {b Descr}: Prints on stdout the obligation proofs the user will have to
    do on its recursive function to prove that its order makes arguments
    decreasing and that the measure is always positive or null.
    This output is lighter than its Coq version because it outputs
    some Focal source code.

    {b Args}:
    - [fun_params_n_tys]: *All* the parameters of the function with their
      types.

    {b Exported}: Yes.                                                      *)
(* ************************************************************************ *)
let print_user_termination_obls_for_measure fun_name fun_params_n_tys
    recursive_calls user_meas rec_fun_used_arg_index rec_fun_used_param
    rec_fun_used_param_ty =
  (* Record the variables of the function appearing in the order
     expression. *)
  let vars_of_meas_expr =
    Parsetree_utils.get_free_local_vnames_from_expr_desc
      user_meas.Parsetree.ast_desc in
  let params_in_meas_expr =
    List.filter
      (fun (n, _) -> List.mem n vars_of_meas_expr)
      fun_params_n_tys in
  Format.printf
    "@\n\
    ---------------------------------------------------------------@\n\
    %tTermination proof obligations for the recursive function%t '%t%a%t':@\n"
   Handy.pp_set_bold Handy.pp_reset_effects
   Handy.pp_set_underlined Sourcify.pp_vname fun_name Handy.pp_reset_effects ;
  let counter = ref 1 in
  List.iter
    (fun (n_exprs, bindings) ->
      (* The list of hypotheses induced by bindings is in *reverse order*.
         Let's reverse it. *)
      let bindings = List.rev bindings in
      Format.printf "@[<2><1>%d prove@ " !counter ;
      incr counter ;
      (* [n_exprs]: (initial variable of the function * expression provided
         in the recursive call). The expression must hence be < to the initial
         variable for the function to terminate. In fact that's the tuple of
         initial variables that must be < to the tuple of expressions provided
         in the recursive call. *)
      let (initial_vars, rec_args) = List.split n_exprs in
      (* For each variable, bind it by a forall. *)
      print_user_variables_quantifications initial_vars bindings ;
      (* We must generate the hypotheses and separate them by ->. *)
      List.iter
        (function
          | Recursion.B_let let_binding -> print_user_binding_let let_binding
          | Recursion.B_match (expr, pattern) ->
              Format.printf "(%a@ = %a)@ ->@ "
                Sourcify.pp_pattern pattern Sourcify.pp_expr expr
          | Recursion.B_condition (expr, bool_val) ->
              Format.printf "(%a" Sourcify.pp_expr expr ;
              if not bool_val then Format.printf "@ =@ false" ;
              Format.printf ")@ ->@ ")
        bindings ;
      (* Now, generate the goals that states the decreasing applying
         the "user-"measure. *)
      Format.printf "%a@ (" Sourcify.pp_expr user_meas ;
      (* Now, generate the only argument used in the order given by the user
         to provide to this order. *)
      let rec_arg = List.nth rec_args rec_fun_used_arg_index in
      let initial_var = List.nth initial_vars rec_fun_used_arg_index in
      (* Generate the corresponding recursive call argument. *)
      Format.printf "%a" Sourcify.pp_expr rec_arg ;
      (* Generate the initial argument of the function. *)
      Format.printf ")@ <@ %a@ (%a)"
        Sourcify.pp_expr user_meas Sourcify.pp_vname (fst initial_var) ;
      Format.printf "@]@\n")
    recursive_calls ;
  (* Print the obligation stating the measure is always positive ot null. *)
  Format.printf "@[<2><1>%d prove@ " !counter   ;
  (* Bind the function parameters appearing in the measure expression. *)
  List.iter
    (fun (n, t) ->
      Format.printf "all@ %a@ :@ %a,@ "
        Sourcify.pp_vname n Types.pp_type_simple t)
    params_in_meas_expr ;
  Format.printf "all %a :@ %a,@ "
    Sourcify.pp_vname rec_fun_used_param
    Types.pp_type_simple rec_fun_used_param_ty ;
  Format.printf "0 <= %a (%a)@]@\n"
    Sourcify.pp_expr user_meas Sourcify.pp_vname rec_fun_used_param ;
  (* Print the conclusion step since it is always the same. *)
  Format.printf
    "@[<2><1>e qed coq proof {*mf_qed*}@]@\n\
     ---------------------------------------------------------------@\n"
;;



(** **************************************************************************
    {b Descr} Create an application expr_desc.

    {b Args}:
    - [tuple_name] : The name of the tuple argument of the recursive function.
      This tuple is the aggregation of the initial parameters of the recursive
      function. This argument name is the one applied to the projection.
    - [proj_name] : Name of the projection function to use.

    {b Rem}: The compilation unit where to find the definition corresponding
    to the projection function name is always assumed to be "basics".

    {b Exported} : No.
    ************************************************************************* *)
let make_proj_app_desc ~tuple_name ~proj_name =
  (* First, the argument of the projector. *)
  let proj_arg_ident =
    Parsetree_utils.make_ast
      (Parsetree.EI_local (Parsetree.Vlident tuple_name)) in
  let proj_arg = Parsetree_utils.make_ast (Parsetree.E_var proj_arg_ident) in
  (* Then, the projector name. Assumed this will be always used with functions
     coming from "basics". *)
  let proj_name =
    Parsetree_utils.make_ast
      (Parsetree.E_var
         (Parsetree_utils.make_ast
            (Parsetree.EI_global
               (Parsetree.Qualified ("basics",
                                     (Parsetree.Vlident proj_name)))))) in
  (* Finally, the application. *)
  Parsetree.E_app (proj_name, [proj_arg])
;;



(** **************************************************************************
    {b Descr} Substitute in an expression, all the occurrences of a function
    parameters names by a projection on the variable [tuple_name] assumed to
    be the tuple of all the parameters of the function.
    This is used for the compilation of termination proofs (by measure or
    order) of recursive functions.

    {b Args}:
    - [tuple_name] : The name of the tuple argument of the recursive function.
      This tuple is the aggregation of the initial parameters of the recursive
      function. This argument name is the one applied to the projection.
    - [initial_expr] : The expression in which subsitutions are performed.
    - [fun_arity] : The number of parameters of the recursive function
      processed (the name of this function never appears here).

    {b Exported} : No.
    ************************************************************************* *)
let subst_params_by_tuple_projections tuple_name initial_expr fun_arity
    fun_params_n_tys =
  (* Compute once for all the list containing only the parameters names. *)
  let fun_params = List.map fst fun_params_n_tys in

  (* Substitution in computational expressions. *)
  let rec rec_subst_expr local_vars expr =
    { expr with Parsetree.ast_desc =
        rec_subst_expr_desc local_vars expr.Parsetree.ast_desc }
  and rec_subst_expr_desc local_vars expr_desc =
    match expr_desc with
    | Parsetree.E_self | Parsetree.E_const _ | Parsetree.E_external _ ->
        expr_desc
    | Parsetree.E_fun (vnames, body) ->
        Parsetree.E_fun (vnames, (rec_subst_expr (vnames @ local_vars) body))
    | Parsetree.E_var id -> (
        (* Function parameters are forcibly local identifiers. Hence ignore
           the substitution for all the other kinds of identifiers. *)
        match id.Parsetree.ast_desc with
        | Parsetree.EI_local vname -> (
            try
              let p_index = Handy.list_index_of vname fun_params in
              if p_index = 0 then (
                make_proj_app_desc
                  ~tuple_name
                  ~proj_name: ("__tpl_firstpr" ^ (string_of_int fun_arity))
               )
              else (
                make_proj_app_desc
                  ~tuple_name
                  ~proj_name: ("__tpl_lastprj" ^
                               (string_of_int (fun_arity - p_index)))
               )
            with Not_found -> expr_desc
          )
        | Parsetree.EI_global _ | Parsetree.EI_method (_, _) -> expr_desc
       )
    | Parsetree.E_app (e, es) ->
        let e' = rec_subst_expr local_vars e in
        let es' = List.map (rec_subst_expr local_vars) es in
        Parsetree.E_app (e', es')
    | Parsetree.E_constr (constr_ident, es) ->
        let es' = List.map (rec_subst_expr local_vars) es in
        Parsetree.E_constr (constr_ident, es')
    | Parsetree.E_match (e, pat_exprs) ->
        let e' = rec_subst_expr local_vars e in
        let pat_exprs' =
          List.map
            (fun (pat, ex) ->
              (* Extract the variables of the pattern and add them to the
                 known local variables. *)
              let local_vars' =
                (Parsetree_utils.get_local_idents_from_pattern pat)
                @ local_vars in
              (pat, (rec_subst_expr local_vars' ex)))
            pat_exprs in
        Parsetree.E_match (e', pat_exprs')
    | Parsetree.E_if (e1, e2, e3) ->
        let e1' = rec_subst_expr local_vars e1 in
        let e2' = rec_subst_expr local_vars e2 in
        let e3' = rec_subst_expr local_vars e3 in
        Parsetree.E_if (e1', e2', e3')
    | Parsetree.E_let (let_def, e) ->
        let let_def' = rec_subst_let_def local_vars let_def in
        let e' = rec_subst_expr local_vars e in
        Parsetree.E_let (let_def', e')
    | Parsetree.E_record label_exprs ->
        let label_exprs' =
          List.map
            (fun (li, e) -> (li, rec_subst_expr local_vars e)) label_exprs in
        Parsetree.E_record (label_exprs')
    | Parsetree.E_record_access (e, label_name) ->
        Parsetree.E_record_access ((rec_subst_expr local_vars e), label_name)
    | Parsetree.E_record_with (e, label_exprs) ->
        let e' = rec_subst_expr local_vars e in
        let label_exprs' =
          List.map
            (fun (li, e) -> (li, (rec_subst_expr local_vars e))) label_exprs in
        Parsetree.E_record_with (e', label_exprs')
    | Parsetree.E_tuple es ->
        Parsetree.E_tuple (List.map (rec_subst_expr local_vars) es)
    | Parsetree.E_sequence es ->
        Parsetree.E_sequence (List.map (rec_subst_expr local_vars) es)
    | Parsetree.E_paren e -> Parsetree.E_paren (rec_subst_expr [] e)

  (* Substitution in logical expressions. *)
  and rec_subst_logical_expr local_vars log_expr =
    { log_expr with Parsetree.ast_desc =
        rec_subst_logical_expr_desc local_vars log_expr.Parsetree.ast_desc }
  and rec_subst_logical_expr_desc local_vars log_expr_desc =
    match log_expr_desc with
    | Parsetree.Pr_forall (vars, ty_expr, lexpr) ->
        let lexpr' = rec_subst_logical_expr (vars @ local_vars) lexpr in
        Parsetree.Pr_forall (vars, ty_expr, lexpr')
    | Parsetree.Pr_exists (vars, ty_expr, lexpr) ->
        let lexpr' = rec_subst_logical_expr (vars @ local_vars) lexpr in
        Parsetree.Pr_exists (vars, ty_expr, lexpr')
    | Parsetree.Pr_imply (lexp1, lexp2) ->
        Parsetree.Pr_imply
          ((rec_subst_logical_expr local_vars lexp1),
           (rec_subst_logical_expr local_vars lexp2))
    | Parsetree.Pr_or (lexp1, lexp2) ->
        Parsetree.Pr_or
          ((rec_subst_logical_expr local_vars lexp1),
           (rec_subst_logical_expr local_vars lexp2))
    | Parsetree.Pr_and (lexp1, lexp2) ->
        Parsetree.Pr_and
          ((rec_subst_logical_expr local_vars lexp1),
           (rec_subst_logical_expr local_vars lexp2))
    | Parsetree.Pr_equiv (lexp1, lexp2) ->
        Parsetree.Pr_equiv
          ((rec_subst_logical_expr local_vars lexp1),
           (rec_subst_logical_expr local_vars lexp2))
    | Parsetree.Pr_not lexp ->
        Parsetree.Pr_not (rec_subst_logical_expr local_vars lexp)
    | Parsetree.Pr_expr exp ->
        Parsetree.Pr_expr (rec_subst_expr local_vars exp)
    | Parsetree.Pr_paren lexp ->
        Parsetree.Pr_paren (rec_subst_logical_expr local_vars lexp)

  (* Substitution in let definitions.
     Note that we do not nedd to substitute in the termination proof since
     it never appears as a computational part (hence as a measure or order
     expression). *)
  and rec_subst_let_def local_vars ldef =
    { ldef with Parsetree.ast_desc =
        rec_subst_let_def_desc local_vars ldef.Parsetree.ast_desc }
  and rec_subst_let_def_desc local_vars ldef_desc =
    (* Depending on whether the bindings are recursive, we add or not
       the bound names in the local variables. *)
    let local_vars_for_rec =
      (match ldef_desc.Parsetree.ld_rec with
      | Parsetree.RF_rec ->
          (List.map
             (fun b -> b.Parsetree.ast_desc.Parsetree.b_name)
             ldef_desc.Parsetree.ld_bindings)
          @ local_vars
      | Parsetree.RF_no_rec -> local_vars) in
    let bindings' =
      List.map
        (fun bnd ->
          let body' =
            match bnd.Parsetree.ast_desc.Parsetree.b_body with
            | Parsetree.BB_logical log_e ->
                Parsetree.BB_logical
                  (rec_subst_logical_expr local_vars_for_rec log_e)
            | Parsetree.BB_computational e ->
                Parsetree.BB_computational
                  (rec_subst_expr local_vars_for_rec e) in
          (* Rebuild the new binding desc. *)
          let descr' =
            { bnd.Parsetree.ast_desc with Parsetree.b_body = body' } in
          (* Rebuild the new binding. *)
          { bnd with Parsetree.ast_desc = descr' })
        ldef_desc.Parsetree.ld_bindings in
    (* Rebuild the new let definition. *)
    { ldef_desc with Parsetree.ld_bindings = bindings' } in

  (* Now, really do the job. *)
  rec_subst_expr [] initial_expr
;;
