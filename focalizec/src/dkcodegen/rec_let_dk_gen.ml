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

type pfdp_type = ?sep:string -> bool -> Format.formatter -> unit;;

(* Prints a list of params separated by sep.
   Not exported.
 *)
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
;;

(* Prints a list of types as a chain of arrows, useful for the second
   argument to the call-by-value operator.
   Not exported. *)
let rec print_cbv_types_as_arrows print_ctx return_ty out = function
  | [] -> Dk_pprint.pp_type_simple_to_dk print_ctx out return_ty
  | ty :: l ->
     Format.fprintf out "(@[cc.Arrow %a %a@])"
                    (Dk_pprint.pp_type_simple_to_dk print_ctx) ty
                    (print_cbv_types_as_arrows print_ctx return_ty) l
;;

(* Prints a function application guarded by the call-by-value
   operator.
   Not exported. *)
let rec print_cbv print_ctx print_name return_ty accu out = function
  | [] -> print_name out;
  | (a, ty) :: l ->
     Format.fprintf out "@[%a@ %a@ (%a)@ %a@]"
                    (Dk_pprint.pp_for_cbv_type_simple_to_dk print_ctx) ty
                    (print_cbv_types_as_arrows print_ctx return_ty) accu
                    (print_cbv print_ctx print_name return_ty (ty :: accu)) l
                    Parsetree_utils.pp_vname_with_operators_expanded a
;;

(* Recursive functions are declared and then defined by a rewrite rule
   forbidding partial application. *)
let declare_recursive_function out_fmter print_ctx prefix name ~close_parens
           params_with_type return_ty
           (print_field_definition_prelude : pfdp_type) =
  Format.fprintf out_fmter
    "@[<2>def %s%a@ : %t%tcc.eT (%a)%t.@]@\n"
    prefix
    Parsetree_utils.pp_vname_with_operators_expanded name
    (print_field_definition_prelude ~sep:"->" false)
    (fun out_fmter ->
     List.iter
       (fun (_, ty) ->
        Format.fprintf out_fmter "cc.eT (%a) ->@ "
          (Dk_pprint.pp_type_simple_to_dk print_ctx) ty)
       params_with_type)
    (Dk_pprint.pp_type_simple_to_dk print_ctx) return_ty
    (fun out -> for _ = 1 to close_parens do Format.fprintf out ")" done);;

let generate_recursive_definition ctx print_ctx env name params scheme body_expr ~abstract ~close_parens ~toplevel
                                  (print_field_definition_prelude : pfdp_type) =
  let species_name = snd (ctx.Context.scc_current_species) in
  let prefix =
    if abstract then
      "abst_"
    else
      if toplevel then
        ""
      else
        Parsetree_utils.vname_as_string_with_operators_expanded
          species_name ^ "__"
  in
  let out_fmter = ctx.Context.scc_out_fmter in
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
          A recursive method m is defined in Dedukti by a rewrite-rule:
          let rec m (arg1, arg2) = F(m, arg1, arg2)
          becomes
          <<start>>
          def m : T1 -> T2 -> T.
          [arg1 : T1, arg2 : T2] m arg1 arg2 -->
                 (m := call_by_value_T2 T (call_by_value_T1 (T2 -> T) m arg1) arg2
                  => F(rec_m, arg1, arg2)).
          <<end>>
          call_by_value_Ti has been defined with Ti such that if v is a value of type Ti and f a function of type
          Ti -> T then
          (call_by_value_Ti T f v) rewrites to (f v).
   *)
  (* Declare the symbol *)
  declare_recursive_function ~close_parens out_fmter print_ctx prefix name params_with_type
                             return_ty print_field_definition_prelude;
  (* Generate the recursive function. *)
  Format.fprintf out_fmter "@[<2>[";
  print_field_definition_prelude  ~sep:"," true out_fmter;
  print_list_param "," out_fmter params_with_type;
  Format.fprintf out_fmter "] %s%a"
                 prefix
                 Parsetree_utils.pp_vname_with_operators_expanded name;
  print_field_definition_prelude true out_fmter;
  List.iter
    (fun (a, _) ->
     Format.fprintf out_fmter "@ (%a)"
                    Parsetree_utils.pp_vname_with_operators_expanded a)
    params_with_type;
  Format.fprintf out_fmter "@ -->@\n";
  (* Let-bind the unqualified (because it is how it appears in the
    recursive call) version of the recursive method to the version
    protected by call-by-value operator *)
  Format.fprintf out_fmter "(%a :=@ (%t%t%a%t) =>@ ("
                 Parsetree_utils.pp_vname_with_operators_expanded name
                 (print_field_definition_prelude ~sep:"=>" false)
                 (fun out_fmter ->
                  List.iter
                    (fun (a, ty) ->
                     Format.fprintf out_fmter "%a : cc.eT (%a) =>@ ("
                                    Parsetree_utils.pp_vname_with_operators_expanded a
                                    (Dk_pprint.pp_type_simple_to_dk print_ctx) ty)
                    params_with_type)
                 (print_cbv print_ctx
                            (fun out ->
                             Format.fprintf out "@[%s%a%t@]"
                               prefix
                               Parsetree_utils.pp_vname_with_operators_expanded name
                               (print_field_definition_prelude true))
                            return_ty [])
                 (List.rev params_with_type)
                 (fun out ->
                  for _ = 1 to close_parens + List.length params_with_type do
                    Format.fprintf out ")"
                  done);
  (* In Dedukti, we do want the current recursive method to be applied to
     parameters coming from lambda-lifting. Hence ~in_recursive_let_section_of: []
     instead of ~in_recursive_let_section_of: [name] (as found in Coq translation) *)
  Expr_dk_generation.generate_expr
    ctx ~local_idents: [] ~in_recursive_let_section_of: []
    ~self_methods_status: Expr_dk_generation.SMS_abstracted
    ~recursive_methods_status: Expr_dk_generation.RMS_regular
    env body_expr ;
  Format.fprintf out_fmter "))@]@\n";
;;

