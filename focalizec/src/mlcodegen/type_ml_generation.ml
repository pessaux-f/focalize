(***********************************************************************)
(*                                                                     *)
(*                        FoCaLiZe compiler                            *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: type_ml_generation.ml,v 1.14 2011-05-27 14:38:09 weis Exp $ *)


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
         (Types.pp_type_simple_to_ml ~current_unit ~reuse_mapping: true []) one
   | several ->
       (begin
       (* Enclose by parentheses and separate by commas. *)
       let rec rec_print_params = function
         | [] -> ()
         | [last] ->
             Format.fprintf out_fmter "%a"
               (Types.pp_type_simple_to_ml
                  ~current_unit ~reuse_mapping: true [])
               last
         | first :: rem ->
             Format.fprintf out_fmter "%a,@ "
               (Types.pp_type_simple_to_ml
                  ~current_unit ~reuse_mapping: true [])
               first ;
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
    In case 2), the type will be for sure a TK_abstract (because it
    can't be a sum or a record. Then if it is fully abstract, we map
    it directly to an OCaml type wearing the same name than the
    defined type itself. This means that this type must exists in
    the OCaml environment of the generated file.
    Returns the ML code generation environment extended by the
    mappings of record field names or sum constructor possibly
    induced by the type definition if it's an external one.

    {b Rem} : Exported outside this module.                           *)
(* ****************************************************************** *)
let type_def_compile ctx env type_def_name type_descr =
  let out_fmter = ctx.Context.rcc_out_fmter in
  (* Type definition header. *)
  Format.fprintf out_fmter "@[<2>type" ;
  (* Get a fresh instance of the type's identity scheme directly instanciated
     with the variables that will serve as parameters of the definition. We
     keep the list of these variables to be able to print them in front of the
     type constructor in the OCaml definition. *)
  let type_def_params =
    List.map
      (fun _ -> Types.type_variable ())
      type_descr.Env.TypeInformation.type_params in
  let instanciated_body =
    Types.specialize_with_args
      type_descr.Env.TypeInformation.type_identity type_def_params in
  Types.purge_type_simple_to_ml_variable_mapping () ;
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
      Format.fprintf out_fmter "%a@] ;;@\n "
        (Types.pp_type_simple_to_ml
           ~current_unit: ctx.Context.rcc_current_unit
           ~reuse_mapping: true [])
        instanciated_body ;
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
        let (_, ocaml_binding) =
          List.find
            (function
             | (Parsetree.EL_Caml, _) -> true
             | (Parsetree.EL_Coq, _)
             | ((Parsetree.EL_external _), _) -> false)
            external_trans.Parsetree.ast_desc in
        Format.fprintf out_fmter "%s@]@ ;;@\n" ocaml_binding
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
      (* To ensure variables names sharing, we will unify an instance of each
         constructor result type (remind they have a functional type whose
         arguments are the sum constructor's arguments and result is the same
         type that the hosting type itself) with the instance of the defined
         type identity. *)
      let sum_constructors_to_print =
        List.map
          (fun (sum_cstr_name, sum_cstr_arity, sum_cstr_scheme) ->
            if sum_cstr_arity = Env.TypeInformation.CA_some then
              (begin
              try
                let sum_cstr_ty = Types.specialize sum_cstr_scheme in
                let unified_sum_cstr_ty =
                  Types.unify
                    ~loc: Location.none ~self_manifest: None
                    (Types.type_arrow
                       (Types.type_variable ()) instanciated_body)
                    sum_cstr_ty in
                let sum_cstr_args =
                  (* We do not have anymore information about "Self"'s
                     structure... *)
                  Types.extract_fun_ty_arg
                    ~self_manifest: None unified_sum_cstr_ty in
                (sum_cstr_name, (Some sum_cstr_args))
              with _ ->
                (* Because program is already well-typed, this should always
                   succeed. *)
                assert false
              end)
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
                 (Types.pp_type_simple_to_ml
                    ~current_unit: ctx.Context.rcc_current_unit
                    ~reuse_mapping: true [])
                 sum_cstr_args)
        sum_constructors_to_print ;
      Format.fprintf out_fmter "@]@\n ;;@\n" ;
      (* Not an external type definition, so nothing new in the environment. *)
      env
      end)
  | Env.TypeInformation.TK_record fields ->
      (begin
      (* Like for the sum types, we make use of unification to ensure the
         sharing of variables names. We proceed exactly the same way,
         delaying the whole print until we unified into each record-field
         type. *)
      let record_fields_to_print =
        List.map
          (fun (field_name, field_mut, field_scheme) ->
            try
              let field_ty = Types.specialize field_scheme in
              let unified_field_ty =
                Types.unify
                  ~loc: Location.none ~self_manifest: None
                  (Types.type_arrow (Types.type_variable ()) instanciated_body)
                  field_ty in
              (* We do not have anymore information about "Self"'s
                 structure... *)
              let field_args =
                Types.extract_fun_ty_arg
                  ~self_manifest: None unified_field_ty in
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
            (Types.pp_type_simple_to_ml
               ~current_unit: ctx.Context.rcc_current_unit
               ~reuse_mapping: true [])
            field_ty)
        record_fields_to_print ;
      Format.fprintf out_fmter " }@] ;;@\n " ;
      (* Not an external type definition, so nothing new in the environment. *)
      env
      end)
 ;;
