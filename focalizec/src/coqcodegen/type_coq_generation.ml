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

(* $Id: type_coq_generation.ml,v 1.21 2012-10-30 09:54:04 pessaux Exp $ *)


(* ********************************************************************** *)
(* {b Descr} : Exception raise when a record type definition containing a
      mutable field tries to be translated into Coq. Currently, we don't
      know how to map mutable records into Coq.

    {b Exported} : Yes.                                                   *)
(* ********************************************************************** *)
exception Mutable_record_fields_not_in_coq of
  (Location.t *      (** Location of the type definition hosting the field. *)
   Parsetree.vname)  (** The mutable field's name. *)
;;



(* ************************************************************************* *)
(* Types.coq_print_context -> Format.formatter -> Types.type_simple list ->  *)
(*   unit                                                                    *)
(** {b  Descr} : Generate the parameters of a type definition. Each variable
    is generated as being of type "Set". We do not need any carrier mapping
    since type definitions are always outside a species, hence can never
    refer to a species parameter's carrier.
    We must share the variable-mapping for each printed type since the
    definition variables were inserted inside before we go on printing here.

    {b Exported} : No.                                                       *)
(* ************************************************************************* *)
let print_types_parameters_sharing_vmapping_and_empty_carrier_mapping
    print_ctx out_fmter tys =
  List.iter
    (fun ty ->
      Format.fprintf out_fmter "(%a : Set)@ "
        (Types.pp_type_simple_to_coq print_ctx) ty)
    tys
;;



(** [nb_extra_args] : the number of extra argument of type "Set" used to
     represent the polymorphism in case where the constructor belongs to a
     polymorphic type. *)
let extend_coq_gen_env_with_type_external_mapping env nb_extra_args
    external_mapping =
  let rec rec_extend rec_env = function
    | [] -> rec_env
    | binding :: rem_bindings ->
        let (bound_name, external_translation) = binding.Parsetree.ast_desc in
        let rec_env' =
          (match bound_name with
           | Parsetree.Vlident _ ->
               (* Starting by a lowercase letter means record field name. *)
               Env.CoqGenEnv.add_label
                 bound_name external_translation.Parsetree.ast_desc rec_env
           | Parsetree.Vuident _ | Parsetree.Viident _ | Parsetree.Vpident _ ->
               (* Starting by an uppercase letter means sum constructor. *)
               let cstr_mapping_info = {
                 Env.CoqGenInformation.cmi_num_polymorphics_extra_args =
                   nb_extra_args;
                 Env.CoqGenInformation.cmi_external_translation =
                   Some external_translation.Parsetree.ast_desc } in
               Env.CoqGenEnv.add_constructor
                 bound_name cstr_mapping_info rec_env
           | _ ->
               (* This syntactically should never arise. *)
               assert false) in
        rec_extend rec_env' rem_bindings in
  (* ********************** *)
  (* Now, let's do the job. *)
  rec_extend env external_mapping.Parsetree.ast_desc
;;




(* ************************************************************************* *)
(* record_in_env: bool -> Context.reduced_compil_context ->                  *)
(*   Env.CoqGenEnv.t -> Parsetree.vname ->                                   *)
(*     Env.TypeInformation.type_description -> Env.CoqGenEnv.t               *)
(** {b Descr} : Emits the Coq code for a type definition. Depending on the
    [~record_in_env] flag, it enriches the environment with elements induced
    by the type definition. Returns the environment either enriched or
    non-modified.

    {b Params} :
      - [~record_in_env] : Since this function is used to generate the Coq
    code for a type definition (and in this case, we want to enrich the
    environment) and the tip for Zenon in a proof "by type ..." (and in this
    case, since the type is already defined, we don't want to enrich the
    environment otherwise we would have an error telling that the type name
    already exist in the environment), this flag tells if the environment
    got as argument must be enriched and returned as function result. Note
    that if we don't want to enrich the environment, then when we invoke the
    current function, the user should make an "ignore" since it should not
    be interested in the function return value.

    {b Exported} : Yes.                                                      *)
(* ************************************************************************* *)
let type_def_compile ~record_in_env ctx env type_def_name type_descr =
  let out_fmter = ctx.Context.rcc_out_fmter in
  (* Build the print context for the methods once for all. *)
  let print_ctx = {
    Types.cpc_current_unit = ctx.Context.rcc_current_unit;
    Types.cpc_current_species = None;
    Types.cpc_collections_carrier_mapping =
      ctx.Context.rcc_collections_carrier_mapping } in
  (* We do not operate on a fresh instance of the type's identity scheme. We
     directly work on the type scheme, taking care to perform *no* unifications
     to prevent poluting it ! *)
  let type_def_params = type_descr.Env.TypeInformation.type_params in
  let (_, tydef_body) =
    Types.scheme_split type_descr.Env.TypeInformation.type_identity in
  (* Compute the number of extra polymorphic-induced arguments to the
     constructor. *)
  let nb_extra_args = List.length type_def_params in
  (* Now, generates the type definition's body. *)
  match type_descr.Env.TypeInformation.type_kind with
   | Env.TypeInformation.TK_abstract ->
       Format.fprintf out_fmter "@[<2>Definition %a__t@ "
         Parsetree_utils.pp_vname_with_operators_expanded type_def_name;
       (* Print the parameter(s) stuff if any. *)
       print_types_parameters_sharing_vmapping_and_empty_carrier_mapping
         print_ctx out_fmter type_def_params;
       (* Since types are toplevel, the way "Self" is printed is non relevant.
          Indeed, "Self" can only appear inside the scope of a species, hence
          never at toplevel, hence we don't need to add any bindind in the
          [collection_carrier_mapping]. *)
       Format.fprintf out_fmter ":=@ %a.@]@\n"
         (Types.pp_type_simple_to_coq print_ctx) tydef_body ;
       if record_in_env then
         (* Not an external type definition, so just add the type definition in
            the environment. *)
         Env.CoqGenEnv.add_type
           ~loc: type_descr.Env.TypeInformation.type_loc type_def_name
           type_descr env
       else env
   | Env.TypeInformation.TK_external (external_expr, external_mapping) ->
       (begin
       Format.fprintf out_fmter "@[<2>Definition %a__t@ "
         Parsetree_utils.pp_vname_with_operators_expanded type_def_name;
       (* Print the parameter(s) stuff if any. *)
       print_types_parameters_sharing_vmapping_and_empty_carrier_mapping
         print_ctx out_fmter type_def_params;
       Format.fprintf out_fmter ":=@ ";
       (* And now, bind the FoCaL identifier to the Coq one. *)
       (try
         let (_, coq_binding) =
           List.find
             (function
               | (Parsetree.EL_Coq, _) -> true
               | (Parsetree.EL_Caml, _)
               | ((Parsetree.EL_external _), _) -> false)
             external_expr.Parsetree.ast_desc in
         Format.fprintf out_fmter "%s.@]@.@\n" coq_binding
       with Not_found ->
         (* We didn't find any correspondance for Coq. *)
         raise
           (Externals_generation_errs.No_external_type_def
              ("Coq", type_def_name, external_expr.Parsetree.ast_loc)));
       (* We build the extended code generation environment in which sum
          constructors or labels are recorded in order to be able to remind on
          what to map them when we will see them. *)
       let env_with_external_mapping =
         extend_coq_gen_env_with_type_external_mapping
           env nb_extra_args external_mapping in
       if record_in_env then
         (* Finally add the type definition in the returned environment. *)
         Env.CoqGenEnv.add_type
           ~loc: type_descr.Env.TypeInformation.type_loc type_def_name
           type_descr env_with_external_mapping
       else env
       end)
   | Env.TypeInformation.TK_variant cstrs ->
       (begin
       (* To ensure variables names sharing, rely on the type definition being
          consistent, sharing variables between the type header and its
          constructors. Remind a constructor has a functional type whose
          arguments are the sum constructor's arguments and result is the same
          type that the hosting type itself. The only difference with OCaml is
          that we keep the complete functionnal type since in Coq constructors
          are really "functions" : one must also write the return type of the
          constructor (that is always the type of the definition hosting the
          constructor. *)
       let sum_constructors_to_print =
         List.map
           (fun (sum_cstr_name, _, sum_cstr_scheme) ->
             (* Recover the body of the scheme of the constructor. *)
             let (_, sum_cstr_ty) = Types.scheme_split sum_cstr_scheme in
               (sum_cstr_name, sum_cstr_ty))
           cstrs in
       Format.fprintf out_fmter "@[<2>Inductive %a__t@ "
         Parsetree_utils.pp_vname_with_operators_expanded type_def_name;
       (* Print the parameter(s) stuff if any. Do it only now the unifications
          have been done with the sum constructors to be sure that thanks to
          unifications, "sames" variables will have the "same" name everywhere
          (i.e. in the the parameters enumeration of the type and in the sum
          constructors definitions). *)
       print_types_parameters_sharing_vmapping_and_empty_carrier_mapping
         print_ctx out_fmter type_def_params;
       Format.fprintf out_fmter ":@ Set :=@ ";
       (* And finally really print the constructors definitions. *)
       List.iter
         (fun (sum_cstr_name, cstr_ty) ->
           (* The sum constructor name. *)
           Format.fprintf out_fmter "@\n| %a"
             Parsetree_utils.pp_vname_with_operators_expanded sum_cstr_name;
           (* The type of the constructor. *)
           Format.fprintf out_fmter " :@ (@[<1>%a@])"
             (Types.pp_type_simple_to_coq print_ctx) cstr_ty)
         sum_constructors_to_print;
       Format.fprintf out_fmter ".@]@\n@\n";
       if record_in_env then
         (begin
         (* Since any variant type constructors must be inserted in the
            environment in order to know the number of extra leading "_" due to
            polymorphism, we return the extended environment. *)
         let env_with_value_constructors =
           List.fold_left
             (fun accu_env (sum_cstr_name, _) ->
               Env.CoqGenEnv.add_constructor
                 sum_cstr_name
                 { Env.CoqGenInformation.cmi_num_polymorphics_extra_args =
                   nb_extra_args;
                   Env.CoqGenInformation.cmi_external_translation = None}
                 accu_env)
             env
             sum_constructors_to_print in
         (* Finally add the type definition in the returned environment. *)
         Env.CoqGenEnv.add_type
           ~loc: type_descr.Env.TypeInformation.type_loc type_def_name
           type_descr env_with_value_constructors
         end)
       else env
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
       Format.fprintf out_fmter "@[<2>Record@ %a__t@ "
         Parsetree_utils.pp_vname_with_operators_expanded type_def_name;
       (* Print the parameter(s) stuff if any. *)
       print_types_parameters_sharing_vmapping_and_empty_carrier_mapping
         print_ctx out_fmter type_def_params;
       Format.fprintf out_fmter ":@ Type :=@\nmk_%a__t {@\n"
         Parsetree_utils.pp_vname_with_operators_expanded type_def_name;
       (* And finally really print the fields definitions. We just create a
          local handy function to print the trailing semi only if the
          processed field is not the last of the list (Coq syntax need). *)
       let rec local_print_fields = function
         | [] -> ()
         | (field_name, field_mut, field_ty) :: q ->
             (* The mutable fields are not yet supported for Coq code. *)
             if field_mut = Env.TypeInformation.FM_mutable then
               raise
                 (Mutable_record_fields_not_in_coq
                    (type_descr.Env.TypeInformation.type_loc, field_name)) ;
             Format.fprintf out_fmter "%a :@ %a"
               Parsetree_utils.pp_vname_with_operators_expanded field_name
               (Types.pp_type_simple_to_coq print_ctx) field_ty ;
             if q <> [] then Format.fprintf out_fmter ";" ;
             Format.fprintf out_fmter "@\n" ;
             local_print_fields q in
       (* Do the printing job... *)
       local_print_fields record_fields_to_print ;
       Format.fprintf out_fmter " }.@]@\n " ;
       (* Not an external type definition, so just add the type definition in
          the environment. *)
       if record_in_env then
         Env.CoqGenEnv.add_type
           ~loc: type_descr.Env.TypeInformation.type_loc type_def_name
           type_descr env
       else env
       end)
;;
