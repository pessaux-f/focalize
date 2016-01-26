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

let generate_recursive_definition ctx print_ctx env name params scheme body_expr
                                  (print_field_definition_prelude : ?sep:string -> bool -> unit) =
  let species_name = snd (ctx.Context.scc_current_species) in
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Extend the context with the mapping between these recursive
          functions and their extra arguments. Since we are in Dk, we
          need to take care of the logical definitions and of the
          explicite types abstraction management. *)
  (* We get the function's parameters and their types. This will serve
          at various stage, each time we will need to speak about a
          parameter. *)
  (* For [bind_parameters_to_types_from_type_scheme], not that we do not
          have anymore information about "Self"'s structure... *)
  let (params_with_type, return_ty_opt, _) =
    MiscHelpers.bind_parameters_to_types_from_type_scheme
      ~self_manifest: None (Some scheme) params in
  (* Just remove the option that must always be Some since we provided
          a scheme. *)
  let params_with_type =
    List.map
      (fun (n, opt_ty) ->
       match opt_ty with None -> assert false | Some t -> (n, t))
      params_with_type in
  let return_ty =
    match return_ty_opt with None -> assert false | Some t -> t in
  (*
          A recursive method m is defined in Dedukti by two symbols m and rec_m:
          let rec m (arg1, arg2) = F(m, arg1, arg2)
          becomes
          def rec_m : T1 -> T2 -> T.
          def m : T1 -> T2 -> T.
          [arg1 : T1, arg2 : T2] m arg1 arg2 --> F(m, arg1, arg2).
          [arg1 : T1, arg2 : T2] rec_m arg1 arg2 --> call_by_value_T2 T (call_by_value_T1 (T2 -> T) m arg1) arg2.
          call_by_value_Ti has been defined with Ti such that if v is a value of type Ti and f a function of type
          Ti -> T then
          (call_by_value_Ti T f v) rewrites to (f v).
   *)
  (* Define the type of both symbols *)
  Format.fprintf out_fmter
                 "@[<2>def %a__%a_type@ : Type := ("
                 Parsetree_utils.pp_vname_with_operators_expanded species_name
                 Parsetree_utils.pp_vname_with_operators_expanded name;
  print_field_definition_prelude ~sep:"->" false;
  List.iter
    (fun (_, ty) ->
     Format.fprintf out_fmter "cc.eT (%a) ->@ "
                    (Dk_pprint.pp_type_simple_to_dk print_ctx) ty)
    params_with_type;
  Format.fprintf out_fmter "cc.eT (%a)).@]@\n"
                 (Dk_pprint.pp_type_simple_to_dk print_ctx) return_ty ;
  (* Declare both symbols *)
  Format.fprintf out_fmter
                 "@[<2>def %a__rec_%a@ : %a__%a_type.@]@\n"
                 Parsetree_utils.pp_vname_with_operators_expanded species_name
                 Parsetree_utils.pp_vname_with_operators_expanded name
                 Parsetree_utils.pp_vname_with_operators_expanded species_name
                 Parsetree_utils.pp_vname_with_operators_expanded name;
  Format.fprintf out_fmter
                 "@[<2>def %a__%a@ : %a__%a_type.@]@\n"
                 Parsetree_utils.pp_vname_with_operators_expanded species_name
                 Parsetree_utils.pp_vname_with_operators_expanded name
                 Parsetree_utils.pp_vname_with_operators_expanded species_name
                 Parsetree_utils.pp_vname_with_operators_expanded name;
  let rec print_list_param sep out = function
    | [] -> ()
    | [(a, _)] ->
       Format.fprintf out "%a"
                      Parsetree_utils.pp_vname_with_operators_expanded a
    | (a, _) :: l ->
       Format.fprintf out "%a%s@ %a"
                      Parsetree_utils.pp_vname_with_operators_expanded a
                      sep
                      (print_list_param sep) l
  in
  (* Generate the recursive function. *)
  Format.fprintf out_fmter "@[<2>[";
  print_field_definition_prelude  ~sep:"," true;
  print_list_param "," out_fmter params_with_type;
  Format.fprintf out_fmter "] %a__%a"
                 Parsetree_utils.pp_vname_with_operators_expanded species_name
                 Parsetree_utils.pp_vname_with_operators_expanded name;
  print_field_definition_prelude true;
  List.iter
    (fun (a, _) ->
     Format.fprintf out_fmter "@ (%a)"
                    Parsetree_utils.pp_vname_with_operators_expanded a)
    params_with_type;
  Format.fprintf out_fmter "@ -->@\n";
  (* Let-bind the unqualified version of the recursive method because it is how
          it appears in the recursive call. *)
  Format.fprintf out_fmter "(%a :=@ %a__rec_%a =>@ "
                 Parsetree_utils.pp_vname_with_operators_expanded name
                 Parsetree_utils.pp_vname_with_operators_expanded species_name
                 Parsetree_utils.pp_vname_with_operators_expanded name;
  (* In Dedukti, we do want the current recursive method to be applied to
          parameters coming from lambda-lifting. Hence ~in_recursive_let_section_of: []
          instead of ~in_recursive_let_section_of: [name] (as found in Coq translation) *)
  Expr_dk_generation.generate_expr
    ctx ~local_idents: [] ~in_recursive_let_section_of: []
    ~self_methods_status: Expr_dk_generation.SMS_abstracted
    ~recursive_methods_status: Expr_dk_generation.RMS_regular
    env body_expr ;
  Format.fprintf out_fmter ").@]@\n";
  (* Generate the CBV version. *)
  Format.fprintf out_fmter "@[<2>[] %a__rec_%a -->@ ("
                 Parsetree_utils.pp_vname_with_operators_expanded species_name
                 Parsetree_utils.pp_vname_with_operators_expanded name;
  print_field_definition_prelude ~sep:"=>" false;
  List.iter
    (fun (a, ty) ->
     Format.fprintf out_fmter "%a : cc.eT (%a) =>@ "
                    Parsetree_utils.pp_vname_with_operators_expanded a
                    (Dk_pprint.pp_type_simple_to_dk print_ctx) ty)
    params_with_type;
  let rec print_cbv_types_as_arrows out = function
    | [] -> Dk_pprint.pp_type_simple_to_dk print_ctx out return_ty
    | ty :: l ->
       Format.fprintf out "(@[cc.Arrow %a %a@])"
                      (Dk_pprint.pp_type_simple_to_dk print_ctx) ty
                      print_cbv_types_as_arrows l
  in
  let rec print_cbv accu out = function
    | [] ->
       Format.fprintf out "@[%a__%a"
                      Parsetree_utils.pp_vname_with_operators_expanded species_name
                      Parsetree_utils.pp_vname_with_operators_expanded name;
       print_field_definition_prelude true;
       Format.fprintf out "@]"
    | (a, ty) :: l when Dk_pprint.has_cbv ty ->
       Format.fprintf out "@[%a@ %a@ (%a)@ %a@]"
                      (Dk_pprint.pp_for_cbv_type_simple_to_dk print_ctx) ty
                      print_cbv_types_as_arrows accu
                      (print_cbv (ty :: accu)) l
                      Parsetree_utils.pp_vname_with_operators_expanded a
    | (a, ty) :: l ->
       Format.fprintf out "@[%a@ %a@]"
                      (print_cbv (ty :: accu)) l
                      Parsetree_utils.pp_vname_with_operators_expanded a
  in
  print_cbv [] out_fmter (List.rev params_with_type);
  (* Close the pretty print box. *)
  Format.fprintf out_fmter ").@]@\n" ;
;;
