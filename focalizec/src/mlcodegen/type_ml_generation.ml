(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            François Pessaux                                                *)
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


(* ************************************************************************ *)
(* Misc_ml_generation.reduced_compil_context -> Types.type_simple list ->   *)
(*   unit                                                                   *)
(** {b Descr} : Just an helper to print a list of types separated by commas
       and sharing a same variables mapping and an empty collection carrier
       mapping. If the list has only 1 element then it is NOT enclosed
       between parens.
       If it a several elements, then it IS enclosed between parens.
       If is has no element (degenerated case) then nothing gets printed.
       This is espercially used to print the parameters of a type
       definition in [type_def_compile].

    {b Rem} : Not exported outside this module.                             *)
(* ************************************************************************ *)
let print_types_comma_with_same_vmapping_and_empty_carrier_mapping ctx tys =
  let current_unit = ctx.Context.rcc_current_unit in
  let out_fmter = ctx.Context.rcc_out_fmter in
  match tys with
   | [] -> ()
   | [one] ->
       Format.fprintf out_fmter " %a"
         (Ml_pprint.pp_type_simple_to_ml ~current_unit []) one
   | several ->
       (begin
       (* Enclose by parentheses and separate by commas. *)
       let rec rec_print_params = function
         | [] -> ()
         | [last] ->
             Format.fprintf out_fmter "%a"
               (Ml_pprint.pp_type_simple_to_ml ~current_unit []) last
         | first :: rem ->
             Format.fprintf out_fmter "%a,@ "
               (Ml_pprint.pp_type_simple_to_ml ~current_unit []) first ;
             rec_print_params rem in
       Format.fprintf out_fmter " (@[<1>" ;
       rec_print_params several ;
       Format.fprintf out_fmter "@])"
       end)
;;




let extend_ml_gen_env_with_type_external_mapping env external_mapping =
  let rec rec_extend rec_env = function
    | [] -> rec_env
    | e_binding :: e_mapping ->
        let (bound_name, external_translation) = e_binding.Parsetree.ast_desc in
        let rec_env' =
          (match bound_name with
           | Parsetree.Vlident _ ->
               (* Starting by a lowercase letter means record field name. *)
               Env.MlGenEnv.add_label
                 bound_name external_translation.Parsetree.ast_desc rec_env
           | Parsetree.Vuident _ ->
               (* Starting by an uppercase letter means sum constructor. *)
               Env.MlGenEnv.add_constructor
                 bound_name external_translation.Parsetree.ast_desc rec_env
           | Parsetree.Viident _ ->
               (* An infix ident means sum constructor. *)
               Env.MlGenEnv.add_constructor
                 bound_name external_translation.Parsetree.ast_desc rec_env
           | Parsetree.Vpident _ ->
               (* A prefix ident means sum constructor. *)
               Env.MlGenEnv.add_constructor
                 bound_name external_translation.Parsetree.ast_desc rec_env
           | _ ->
               (* This syntactically should never arise. *)
               assert false) in
        rec_extend rec_env' e_mapping in
  (* ********************** *)
  (* Now, let's do the job. *)
  rec_extend env external_mapping.Parsetree.ast_desc
;;



(* ****************************************************************** *)
(* Misc_ml_generation.reduced_compil_context -> Env.MlGenEnv.t ->     *)
(*   Parsetree.vname -> Env.TypeInformation.type_description ->       *)
(*     Env.MlGenEnv.t                                                 *)
(** {b Descr} : Generates the OCaml code for a FoCaLiZe type definition.
      The process is split in 2 pretty different generation models
      in order to handled both:
       1) the regular (i.e. non-external) type definitions.
       2) the external type definitions.
    In case 1), the generated type body is based on its
    [type_descr.Env.TypeInformation.type_identity] and its
    [type_descr.Env.TypeInformation.type_kind].
    In case 2), the type will be for sure a TK_abstract (because it can't be a
    sum or a record. Then if it is fully abstract, we map it directly to an
    OCaml type wearing the same name than the defined type itself. This means
    that this type must exists in the OCaml environment of the generated file.
    Returns the ML code generation environment extended by the mappings of
    record field names or sum constructor possibly induced by the type
    definition if it's an external one.

    {b Rem} : Exported outside this module.                           *)
(* ****************************************************************** *)
let type_def_compile ctx env ~is_first ~is_last type_def_name type_descr =
  let out_fmter = ctx.Context.rcc_out_fmter in
  (* Type definition header. *)
  if is_first then Format.fprintf out_fmter "@[<2>type"
  else Format.fprintf out_fmter "@[<2>and" ;
  (* We do not operate on a fresh instance of the type's identity scheme. We
     directly work on the type scheme, taking care to perform *no* unifications
     to prevent poluting it ! *)
  let type_def_params = type_descr.Env.TypeInformation.type_params in
  let (_, tydef_body) =
    Types.scheme_split type_descr.Env.TypeInformation.type_identity in
  Ml_pprint.purge_type_simple_to_ml_variable_mapping () ;
  (* Now, generates the type definition's body. *)
  match type_descr.Env.TypeInformation.type_kind with
  | Env.TypeInformation.TK_abstract ->
      (* Print the parameter(s) stuff if any. *)
      print_types_comma_with_same_vmapping_and_empty_carrier_mapping
        ctx type_def_params ;
      (* Now print the type constructor's name. *)
      Format.fprintf out_fmter " _focty_%a =@ "
        Parsetree_utils.pp_vname_with_operators_expanded type_def_name ;
      (* Type abbreviation: the body is the abbreviated type. *)
      Format.fprintf out_fmter "%a@]"
        (Ml_pprint.pp_type_simple_to_ml
           ~current_unit: ctx.Context.rcc_current_unit [])
        tydef_body ;
      (* End of the definition. *)
      if is_last then Format.fprintf out_fmter " ;;" ;
      Format.fprintf out_fmter "@\n" ;
      (* Not an external type definition, so nothing new in the environment. *)
      env
  | Env.TypeInformation.TK_external (external_trans, external_mapping) ->
      (begin
      (* Print the parameter(s) stuff if any. *)
      print_types_comma_with_same_vmapping_and_empty_carrier_mapping
        ctx type_def_params ;
      (* Now, the type name, renamed as "_focty_" followed by the original
         name. *)
      Format.fprintf out_fmter " _focty_%a =@ "
        Parsetree_utils.pp_vname_with_operators_expanded type_def_name ;
      (* And now, bind the FoCaLiZe identifier to the OCaml one. *)
      (try
        let (_, ocaml_code) =
          List.find
            (function
             | (Parsetree.EL_Caml, _) -> true
             | (Parsetree.EL_Coq, _)
             | (Parsetree.EL_Dk, _)
             | ((Parsetree.EL_external _), _) -> false)
            external_trans.Parsetree.ast_desc in
        Format.fprintf out_fmter "%s@]" ocaml_code ;
        (* End of the definition. *)
        if is_last then Format.fprintf out_fmter " ;;" ;
        Format.fprintf out_fmter "@\n" ;
       with Not_found ->
         (* We didn't find any correspondance for OCaml. *)
         raise
           (Externals_generation_errs.No_external_type_def
              ("OCaml", type_def_name, external_trans.Parsetree.ast_loc))) ;
      (* Finally, we return the extended code generation environment in
         which sum constructors or labels are recorded in order to be able to
         remind on what to map them when we will see them. *)
      extend_ml_gen_env_with_type_external_mapping env external_mapping
      end)
  | Env.TypeInformation.TK_variant cstrs ->
      (begin
      (* To ensure variables names sharing, rely on the type definition being
         consistent, sharing variables between the type header and its
         constructors. Remind a constructor has a functional type whose
         arguments are the sum constructor's arguments and result is the same
         type that the hosting type itself. The only exception is for 0-ary
         constructors that are not functions: they are constants of the hosting
         type.
         Hence, we harvest the constructors names and *argument* types. For
         n-ary constructors we remind the type of the arguments. For 0-ary we do
         not record any type since there is no argument. *)
      let sum_constructors_to_print =
        List.map
          (fun (sum_cstr_name, sum_cstr_arity, sum_cstr_scheme) ->
Format.eprintf "generating cstr: %a@." Sourcify.pp_vname sum_cstr_name ;
            if sum_cstr_arity = Env.TypeInformation.CA_some then (
              let (_, sum_cstr_ty) = Types.scheme_split sum_cstr_scheme in
              let sum_cstr_args =
                (* We don't have anymore info about "Self"'s structure... *)
                Types.extract_fun_ty_arg ~self_manifest: None sum_cstr_ty in
              (sum_cstr_name, (Some sum_cstr_args))
             )
            else (sum_cstr_name, None))
          cstrs in
      (* Print the parameter(s) stuff if any. Do it only now the unifications
         have been done with the sum constructors to be sure that thanks to
         unifications, "sames" variables will have the "same" name everywhere
         (i.e. in the the parameters enumeration of the type and in the sum
         constructors definitions). *)
      print_types_comma_with_same_vmapping_and_empty_carrier_mapping
        ctx type_def_params ;
      (* Now print the type constructor's name. *)
      Format.fprintf out_fmter " _focty_%a =@ "
        Parsetree_utils.pp_vname_with_operators_expanded type_def_name ;
      (* And finally really print the constructors definitions. *)
      List.iter
        (fun (sum_cstr_name, opt_args) ->
          (* The sum constructor name. *)
          Format.fprintf out_fmter "@\n| %a"
            Parsetree_utils.pp_vname_with_operators_expanded sum_cstr_name ;
          match opt_args with
           | None -> ()
           | Some sum_cstr_args ->
               (* The argument(s) of the constructor. *)
               Format.fprintf out_fmter " of@ (@[<1>%a@])"
                 (Ml_pprint.pp_type_simple_to_ml
                    ~current_unit: ctx.Context.rcc_current_unit [])
                 sum_cstr_args)
        sum_constructors_to_print ;
      Format.fprintf out_fmter "@]" ;
      (* End of the definition. *)
      if is_last then Format.fprintf out_fmter " ;;" ;
      Format.fprintf out_fmter "@\n" ;
      (* Not an external type definition, so nothing new in the environment. *)
      env
      end)
  | Env.TypeInformation.TK_record fields ->
      (begin
       (* Like for the sum types, we directly dig in the fields schemes,
          assuming the type definition is consistent, hance sharing variables
          between header of the definition and its fields. *)
      let record_fields_to_print =
        List.map
          (fun (field_name, field_mut, field_scheme) ->
            try
              let (_, field_ty) = Types.scheme_split field_scheme in
              (* We do not have anymore information about "Self"'s
                 structure... *)
              let field_args =
                Types.extract_fun_ty_arg ~self_manifest: None field_ty in
              (field_name, field_mut, field_args)
            with _ ->
              (* Because program is already well-typed, this should always
                 succeed. *)
              assert false)
          fields in
      (* Print the parameter(s) stuff if any. *)
      print_types_comma_with_same_vmapping_and_empty_carrier_mapping
        ctx type_def_params ;
      (* Now print the type constructor's name. *)
      Format.fprintf out_fmter " _focty_%a = {@ "
        Parsetree_utils.pp_vname_with_operators_expanded type_def_name ;
      (* And finally really print the fields definitions. *)
      List.iter
        (fun (field_name, field_mut, field_ty) ->
          Format.fprintf out_fmter "@\n " ;
          (* Generate the mutability flag. *)
          if field_mut = Env.TypeInformation.FM_mutable then
            Format.fprintf out_fmter "mutable " ;
          Format.fprintf out_fmter "%a :@ %a ;"
            Parsetree_utils.pp_vname_with_operators_expanded field_name
            (Ml_pprint.pp_type_simple_to_ml
               ~current_unit: ctx.Context.rcc_current_unit [])
            field_ty)
        record_fields_to_print ;
      Format.fprintf out_fmter "@]@\n}" ;
      (* End of the definition. *)
      if is_last then Format.fprintf out_fmter " ;;" ;
      Format.fprintf out_fmter "@\n" ;
      (* Not an external type definition, so nothing new in the environment. *)
      env
      end)
 ;;


let type_defs_compile env out_fmter ~current_unit ty_descrs =
  let rec fold_compile ~is_first accu_env = function
    | [] -> accu_env
    | (type_def_name, type_descr) :: q ->
        Ml_pprint.purge_type_simple_to_ml_variable_mapping () ;
        (* Create the initial context for compiling the type definition. *)
        let ctx = {
          Context.rcc_current_unit = current_unit ;
          (* Not under a species, hence no species parameter. *)
          Context.rcc_species_parameters_names = [] ;
          (* Not under a species, hence empty carriers mapping. *)
          Context.rcc_collections_carrier_mapping = [] ;
          (* Not in the context of generating a method's body code, then
             empty. *)
          Context.rcc_lambda_lift_params_mapping = [] ;
          Context.rcc_out_fmter = out_fmter } in
        let accu_env' =
          type_def_compile
            ctx accu_env ~is_first ~is_last: (q = []) type_def_name
            type_descr in
        (* Generate the remaining of the type definitions in the extended
           environment. The next call is no more the first one. *)
        fold_compile ~is_first: false accu_env' q in
  (* Initial call: [is_first] is true. *)
  fold_compile ~is_first: true env ty_descrs
;;
