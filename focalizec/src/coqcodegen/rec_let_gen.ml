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

(* $Id: rec_let_gen.ml,v 1.7 2008-10-16 20:55:03 weis Exp $ *)



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
                   Parsetree.ast_doc = [] ;
                   Parsetree.ast_type = Parsetree.ANTI_non_relevant } ]
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
       | Parsetree.E_equality (e1, e2) ->
           let (e1', rec_found1) = rec_transform_expr e1 in
           if rec_found1 then failwith "Tupling too complex" ;
           let (e2', rec_found2) = rec_transform_expr e2 in
           if rec_found2 then failwith "Tupling too complex" ;
           ((Parsetree.E_equality (e1', e2')), false)
       | Parsetree.E_paren e ->
           let (e', rec_found) = rec_transform_expr e in
           ((Parsetree.E_paren e'), rec_found)
       | Parsetree.E_equality (e1, e2) ->
           let (e1', rec_found1) = rec_transform_expr e1 in
           if rec_found1 then failwith "Tupling too complex" ;
           let (e2', rec_found2) = rec_transform_expr e2 in
           if rec_found2 then failwith "Tupling too complex" ;
           ((Parsetree.E_equality (e1', e2')), false) in
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



let generate_termination_lemmas _ctx _print_ctx _env recursive_calls =
  List.iter
    (fun (n_exprs, bindings) ->
      Format.eprintf "n/expr@." ;
      List.iter
        (fun (n, expr) ->
          Format.eprintf "\tn: %a, expr: %a@."
            Sourcify.pp_vname n Sourcify.pp_expr expr)
        n_exprs ;
      Format.eprintf "bindings@." ;
      List.iter
        (function
          | Recursion.B_let let_binding ->
              Format.eprintf "\tB_let, binding: %a@."
                Sourcify.pp_binding let_binding
          | Recursion.B_match (expr, pattern) ->
              Format.eprintf "\tB_match,  expr: %a, pattern: %a@."
                Sourcify.pp_expr expr Sourcify.pp_pattern pattern
          | Recursion.B_condition (expr, bool_val) ->
              Format.eprintf "\tB_condition, expr: %a, bool: %b@."
                Sourcify.pp_expr expr bool_val)
        bindings)
    recursive_calls
;;
