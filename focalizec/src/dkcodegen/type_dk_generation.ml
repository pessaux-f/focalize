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


(* ********************************************************************** *)
(* {b Descr} : Exception raise when a record type definition containing a
      mutable field tries to be translated into Dk. Currently, we don't
      know how to map mutable records into Dk.

    {b Exported} : Yes.                                                   *)
(* ********************************************************************** *)
exception Mutable_record_fields_not_in_dk of
  (Location.t *      (** Location of the type definition hosting the field. *)
   Parsetree.vname)  (** The mutable field's name. *)
;;



(* ************************************************************************* *)
(* Types.dk_print_context -> Format.formatter -> Types.type_simple list ->  *)
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
      Format.fprintf out_fmter "(%a : cc.uT)@ "
        (Dk_pprint.pp_type_simple_to_dk print_ctx) ty)
    tys
;;

(* Same but arrows between parameters instead of spaces.
   Useful for Dedukti declarations for which we cannot pass parameters
   but only a (Pi-) type.
*)
let print_types_parameters_sharing_vmapping_and_empty_carrier_mapping_with_arrows
    print_ctx out_fmter tys =
  List.iter
    (fun ty ->
      Format.fprintf out_fmter "%a : cc.uT ->@ "
        (Dk_pprint.pp_type_simple_to_dk print_ctx) ty)
    tys
;;

(* Same but commas between parameters instead of spaces.
   Useful for Dedukti rewrite contexts.
*)
let print_types_parameters_sharing_vmapping_and_empty_carrier_mapping_with_commas
    print_ctx out_fmter tys =
  List.iter
    (fun ty ->
      Format.fprintf out_fmter "%a,@ "
        (Dk_pprint.pp_type_simple_to_dk print_ctx) ty)
    tys
;;

let print_types_parameters_sharing_vmapping_and_empty_carrier_mapping_with_spaces
    print_ctx out_fmter tys =
  List.iter
    (fun ty ->
      Format.fprintf out_fmter "%a@ "
        (Dk_pprint.pp_type_simple_to_dk print_ctx) ty)
    tys
;;

(* Same but at the other side of the ":". *)
let print_types_parameters_with_arrows print_ctx out_fmter tys =
  List.iter
    (fun ty ->
      Format.fprintf out_fmter "cc.eT %a ->@ "
        (Dk_pprint.pp_type_simple_to_dk print_ctx) ty)
    tys
;;

(* Same but with spaces. *)
let print_types_parameters_with_spaces print_ctx out_fmter tys =
  List.iter
    (fun ty ->
      Format.fprintf out_fmter "@ %a"
        (Dk_pprint.pp_type_simple_to_dk print_ctx) ty)
    tys
;;

let extend_dk_gen_env_with_type_external_mapping env
    external_mapping =
  let rec rec_extend rec_env = function
    | [] -> rec_env
    | binding :: rem_bindings ->
        let (bound_name, external_translation) = binding.Parsetree.ast_desc in
        let rec_env' =
          (match bound_name with
           | Parsetree.Vlident _ ->
               (* Starting by a lowercase letter means record field name. *)
               let label_mapping_info = {
                 Env.DkGenInformation.lmi_external_translation =
                   Some external_translation.Parsetree.ast_desc } in
               Env.DkGenEnv.add_label bound_name label_mapping_info rec_env
           | Parsetree.Vuident _ | Parsetree.Viident _ | Parsetree.Vpident _ ->
               (* Starting by an uppercase letter means sum constructor. *)
               let cstr_mapping_info = {
                 Env.DkGenInformation.cmi_external_translation =
                   Some external_translation.Parsetree.ast_desc } in
               Env.DkGenEnv.add_constructor
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
(* as_zenon_fact: bool -> Context.reduced_compil_context ->                  *)
(*   Env.DkGenEnv.t -> Parsetree.vname ->                                   *)
(*     Env.TypeInformation.type_description -> Env.DkGenEnv.t               *)
(** {b Descr} : Emits the Dk code for a type definition. Depending on the
    [~as_zenon_fact] flag, it enriches the environment with elements induced
    by the type definition. Returns the environment either enriched or
    non-modified.

    {b Params} :
      - [~as_zenon_fact] : Since this function is used to generate the Dk
    code for a type definition (and in this case, we want to enrich the
    environment) and the tip for Zenon in a proof "by type ..." (and in this
    case, since the type is already defined, we don't want to enrich the
    environment otherwise we would have an error telling that the type name
    already exist in the environment), this flag tells if the environment
    got as argument must be enriched and returned as function result. Note
    that if we don't want to enrich the environment, then when we invoke the
    current function, the user should make an "ignore" since it should not
    be interested in the function return value. Hence if this flag is true
    then the environment must not be enriched, and moreover, the emitted
    definition for Zenon must qualify the constructors if the type is not
    hosted in the current compilation unit. In effect, in functions using
    this type and on which we depend to make proofs, constructors WILL be
    qualified. Not qualifying constructors in the "fake" definition for Zenon
    would prevent ot from finding proofs.
    ATTENTION: constructors must be qualified without the "filesystem-path"
    of their hosting file (was bug #19).

    {b Exported} : Yes.                                                      *)
(* ************************************************************************* *)
let type_def_compile ~as_zenon_fact ctx env type_def_name type_descr =
  let out_fmter = ctx.Context.rcc_out_fmter in
  (* Build the print context for the methods once for all. *)
  let print_ctx = {
    Dk_pprint.dpc_current_unit = ctx.Context.rcc_current_unit ;
    Dk_pprint.dpc_current_species = None;
    Dk_pprint.dpc_collections_carrier_mapping =
      ctx.Context.rcc_collections_carrier_mapping } in
  (* We do not operate on a fresh instance of the type's identity scheme. We
     directly work on the type scheme, taking care to perform *no* unifications
     to prevent poluting it ! *)
  let type_def_params = type_descr.Env.TypeInformation.type_params in
  let (_, tydef_body) =
    Types.scheme_split type_descr.Env.TypeInformation.type_identity in
  (* Now, generates the type definition's body. *)
  match type_descr.Env.TypeInformation.type_kind with
   | Env.TypeInformation.TK_abstract ->
       Format.fprintf out_fmter "@[<2>def %a__t@ "
         Parsetree_utils.pp_vname_with_operators_expanded type_def_name;
       (* Print the parameter(s) stuff if any. *)
       print_types_parameters_sharing_vmapping_and_empty_carrier_mapping
         print_ctx out_fmter type_def_params;
       (* Since types are toplevel, the way "Self" is printed is non relevant.
          Indeed, "Self" can only appear inside the scope of a species, hence
          never at toplevel, hence we don't need to add any bindind in the
          [collection_carrier_mapping]. *)
       Format.fprintf out_fmter ":=@ %a.@]@\n"
         (Dk_pprint.pp_type_simple_to_dk print_ctx) tydef_body ;
       if not as_zenon_fact then
         Env.DkGenEnv.add_type
           ~loc: type_descr.Env.TypeInformation.type_loc type_def_name
           type_descr env
       else env
   | Env.TypeInformation.TK_external (external_expr, external_mapping) ->
       (begin
       Format.fprintf out_fmter "@[<2>def %a__t@ "
         Parsetree_utils.pp_vname_with_operators_expanded type_def_name ;
       (* Print the parameter(s) stuff if any. *)
       print_types_parameters_sharing_vmapping_and_empty_carrier_mapping
         print_ctx out_fmter type_def_params ;
       Format.fprintf out_fmter ":=@ " ;
       (* And now, bind the FoCaL identifier to the Dk one. *)
       (try
         let (_, dk_binding) =
           List.find
             (function
               | (Parsetree.EL_Dk, _) -> true
               | (Parsetree.EL_Caml, _)
               | (Parsetree.EL_Coq, _)
               | ((Parsetree.EL_external _), _) -> false)
             external_expr.Parsetree.ast_desc in
         Format.fprintf out_fmter "%s.@]@.@\n" dk_binding
       with Not_found ->
         (* We didn't find any correspondance for Dk. *)
         raise
           (Externals_generation_errs.No_external_type_def
              ("Dk", type_def_name, external_expr.Parsetree.ast_loc))) ;
       (* We build the extended code generation environment in which sum
          constructors or labels are recorded in order to be able to remind on
          what to map them when we will see them. *)
       let env_with_external_mapping =
         extend_dk_gen_env_with_type_external_mapping
           env external_mapping in
       if not as_zenon_fact then
         (* Finally add the type definition in the returned environment. *)
         Env.DkGenEnv.add_type
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
          that we keep the complete functionnal type since in Dk constructors
          are really "functions" : one must also write the return type of the
          constructor (that is always the type of the definition hosting the
          constructor.
          However we need the list of argument types to type destructors so we take the function from type_ml_generation.ml.
*)
       let sum_constructors_to_print =
         List.map
           (fun (sum_cstr_name, sum_cstr_arity, sum_cstr_scheme) ->
             (* Recover the body of the scheme of the constructor. *)
             let (_, sum_cstr_ty) = Types.scheme_split sum_cstr_scheme in
             let sum_cstr_args =
               (* We don't have anymore info about "Self"'s structure... *)
               if sum_cstr_arity = Env.TypeInformation.CA_some then
                 Types.extract_prod_ty
                   ~self_manifest: None
                   (Types.extract_fun_ty_arg ~self_manifest: None sum_cstr_ty)
               else []
             in

             (sum_cstr_name, sum_cstr_ty, sum_cstr_args))
           cstrs in
       Format.fprintf out_fmter "(; Inductive definition ;)@\n@[<2>%a__t : "
         Parsetree_utils.pp_vname_with_operators_expanded type_def_name;
       (* Print the parameter(s) stuff if any. Do it only now the unifications
          have been done with the sum constructors to be sure that thanks to
          unifications, "sames" variables will have the "same" name everywhere
          (i.e. in the the parameters enumeration of the type and in the sum
          constructors definitions). *)
       print_types_parameters_sharing_vmapping_and_empty_carrier_mapping_with_arrows
         print_ctx out_fmter type_def_params;
       Format.fprintf out_fmter "cc.uT.@\n(; Constructors ;)";
       (* Qualify constructor if we are printing a fact for Zenon and the
          location where we generate it is not in the compilation unit
          hosting the type definition. Drop the file-system full path (was
          bug #19). *)
       let tydef_comp_unit =
         Filename.chop_extension
           (Filename.basename
              (type_descr.Env.TypeInformation.type_loc.Location.
                 l_beg.Location.pos_fname)) in
       let qualif =
         if ctx.Context.rcc_current_unit <> tydef_comp_unit && as_zenon_fact
         then tydef_comp_unit ^ "."
         else "" in
       (* Then really print the constructors declarations. *)
       List.iter
         (fun (sum_cstr_name, cstr_ty, _) ->
           (* The sum constructor name. *)
           Format.fprintf out_fmter "@\n%s%a :@ @[<v 2>"
             qualif
             Parsetree_utils.pp_vname_with_operators_expanded sum_cstr_name ;
           (* The type of the constructor.
              Parameterized as the inductive. *)
           print_types_parameters_sharing_vmapping_and_empty_carrier_mapping_with_arrows
             print_ctx out_fmter type_def_params;
           Format.fprintf out_fmter "cc.eT (@[<1>%a@])@]."
             (Dk_pprint.pp_type_simple_to_dk print_ctx) cstr_ty)
         sum_constructors_to_print;
       (* And finally, the destructors. *)
       Format.fprintf out_fmter "@\n(; Destructors ;)";
       List.iter
         (fun (sum_cstr_name, _, cstr_args) ->
           (* The sum constructor name. *)
           Format.fprintf out_fmter "@\ndef %smatch__%a :@ @[<v 2>"
             qualif
             Parsetree_utils.pp_vname_with_operators_expanded sum_cstr_name ;
           (* The type of the destructor.
              Parameterized as the inductive. *)
           print_types_parameters_sharing_vmapping_and_empty_carrier_mapping_with_arrows
             print_ctx out_fmter type_def_params;
           Format.fprintf out_fmter
                          "Ret_type : cc.uT ->@ cc.eT (@[<1>%a__t%a@]) ->@ (@[%acc.eT Ret_type@]) ->@ cc.eT Ret_type ->@ cc.eT Ret_type.@]@\n"
                          Parsetree_utils.pp_vname_with_operators_expanded type_def_name
                          (print_types_parameters_with_spaces print_ctx) type_def_params
                          (* The type of the matching function.
                             Parameterized as the constructor. *)
                          (print_types_parameters_with_arrows print_ctx) cstr_args;
           List.iter (fun (curr_sum_cstr_name, _, curr_cstr_args) ->
                      if (sum_cstr_name = curr_sum_cstr_name) then
                        begin
                          Format.fprintf out_fmter
                          "[%a%aRet_type,@ pattern,@ default] %smatch__%a@ %aRet_type@ (%a@ %a@ %a)@ pattern@ default -->@ pattern%a.@\n"
                          (print_types_parameters_sharing_vmapping_and_empty_carrier_mapping_with_commas print_ctx)
                          type_def_params
                          (fun out ->
                           List.iteri (fun i _ -> Format.fprintf out "x_%d_,@ " i))
                          cstr_args
                          qualif
                          Parsetree_utils.pp_vname_with_operators_expanded sum_cstr_name
                          (print_types_parameters_sharing_vmapping_and_empty_carrier_mapping_with_spaces print_ctx)
                          type_def_params
                          Parsetree_utils.pp_vname_with_operators_expanded sum_cstr_name
                          (print_types_parameters_sharing_vmapping_and_empty_carrier_mapping_with_spaces print_ctx)
                          type_def_params
                          (fun out -> List.iteri (fun i _ -> Format.fprintf out "x_%d_@ " i))
                          cstr_args
                          (fun out -> List.iteri (fun i _ -> Format.fprintf out "@ x_%d_" i))
                          cstr_args;
                        end else begin
                          Format.fprintf out_fmter
                          "[%a%aRet_type,@ pattern,@ default] %smatch__%a@ %aRet_type@ (%a@ %a@ %a)@ pattern@ default -->@ default.@\n"
                          (print_types_parameters_sharing_vmapping_and_empty_carrier_mapping_with_commas print_ctx)
                          type_def_params
                          (fun out ->
                           List.iteri (fun i _ -> Format.fprintf out "x_%d_,@ " i))
                          curr_cstr_args
                          qualif
                          Parsetree_utils.pp_vname_with_operators_expanded sum_cstr_name
                          (print_types_parameters_sharing_vmapping_and_empty_carrier_mapping_with_spaces print_ctx)
                          type_def_params
                          Parsetree_utils.pp_vname_with_operators_expanded curr_sum_cstr_name
                          (print_types_parameters_sharing_vmapping_and_empty_carrier_mapping_with_spaces print_ctx)
                          type_def_params
                          (fun out -> List.iteri (fun i _ -> Format.fprintf out "x_%d_@ " i))
                          curr_cstr_args;
           end)
         sum_constructors_to_print
         )
         sum_constructors_to_print;
       Format.fprintf out_fmter "@]\n@\n";
       if not as_zenon_fact then
         (begin
             (* The call_by_value construction is higher-order so we don't give it to Zenon *)
             Format.fprintf out_fmter "@[<2>def call_by_value_%a__t : "
               Parsetree_utils.pp_vname_with_operators_expanded type_def_name;
       (* Print the parameter(s) stuff if any. Do it only now the unifications
          have been done with the sum constructors to be sure that thanks to
          unifications, "sames" variables will have the "same" name everywhere
          (i.e. in the the parameters enumeration of the type and in the sum
          constructors definitions). *)
       print_types_parameters_sharing_vmapping_and_empty_carrier_mapping_with_arrows
         print_ctx out_fmter type_def_params;
       Format.fprintf out_fmter "R : cc.uT ->@ (cc.eT (@[<1>%a__t%a@]) ->@ cc.eT R) ->@ cc.eT (@[<1>%a__t%a@]) ->@ cc.eT R.@]@\n"
                          Parsetree_utils.pp_vname_with_operators_expanded type_def_name
                          (print_types_parameters_with_spaces print_ctx) type_def_params
                          Parsetree_utils.pp_vname_with_operators_expanded type_def_name
                          (print_types_parameters_with_spaces print_ctx) type_def_params
       ;

         (* Recursive definitions need to enforce an evaluation order:
   some functions should only be applied to their arguments after
   they have been reduced to values. This is ensured by a special
   "call_by_value" higher-order and polymorphic function defined for
   each type:

   If T is a type with a polymorphic parameter poly_arg, then

   call_by_value : poly_arg : cc.uT ->
                   R : cc.uT ->
                   (cc.eT (T poly_args) -> cc.eT R) ->
                   cc.eT (T poly_arg) -> cc.eT R.

   and for each possible value v of type T,

   [ poly_arg : cc.uT, R : cc.uT, f : cc.eT (T poly_arg) -> cc.eT R ]
       call_by_value_T poly_arg R f v --> f v.

 *)
       List.iter
         (fun (sum_cstr_name, _, cstr_args) ->
           (* The sum constructor name. *)
           Format.fprintf out_fmter "@[(; CBV for type constructor %a  ;)@]@\n"
             Parsetree_utils.pp_vname_with_operators_expanded sum_cstr_name;
           Format.fprintf out_fmter "@[[";
           print_types_parameters_sharing_vmapping_and_empty_carrier_mapping_with_commas
             print_ctx out_fmter type_def_params;
           Format.fprintf out_fmter "R,@ f";
           List.iteri
             (fun i _ -> Format.fprintf out_fmter ",@ x_%d_" i)
             cstr_args;
           Format.fprintf out_fmter "] call_by_value_%a__t@ "
                          Parsetree_utils.pp_vname_with_operators_expanded type_def_name;
           List.iter (Format.fprintf out_fmter "%a " (Dk_pprint.pp_type_simple_to_dk print_ctx))
                     type_def_params;
           Format.fprintf out_fmter "R@ f@ (%a"
             Parsetree_utils.pp_vname_with_operators_expanded sum_cstr_name;
           List.iter (Format.fprintf out_fmter "@ %a" (Dk_pprint.pp_type_simple_to_dk print_ctx))
                     type_def_params;
           List.iteri
             (fun i _ -> Format.fprintf out_fmter "@ x_%d_" i)
             cstr_args;
           Format.fprintf out_fmter ")@ --> ";
           Format.fprintf out_fmter "f@ (%a"
             Parsetree_utils.pp_vname_with_operators_expanded sum_cstr_name;
           List.iter (Format.fprintf out_fmter "@ %a" (Dk_pprint.pp_type_simple_to_dk print_ctx))
                     type_def_params;
           List.iteri
             (fun i _ -> Format.fprintf out_fmter "@ x_%d_" i)
             cstr_args;
           Format.fprintf out_fmter ").@\n";
           (* (\* The type of the destructor. *)
           (*    Parameterized as the inductive. *\) *)
           (* print_types_parameters_sharing_vmapping_and_empty_carrier_mapping_with_arrows *)
           (*   print_ctx out_fmter type_def_params; *)
           (* Format.fprintf out_fmter *)
           (*                "Ret_type : cc.uT ->@ cc.eT (@[<1>%a__t%a@]) ->@ (%acc.eT Ret_type@]) ->@ cc.eT Ret_type ->@ cc.eT Ret_type.@]" *)
           (*                Parsetree_utils.pp_vname_with_operators_expanded type_def_name *)
           (*                (print_types_parameters_with_spaces print_ctx) type_def_params *)
           (*                (\* The type of the matching function. *)
           (*                   Parameterized as the constructor. *\) *)
           (*                (print_types_parameters_with_arrows print_ctx) cstr_args *)
         )
         sum_constructors_to_print;
         (* Since any variant type constructors must be inserted in the
            environment in order to know the number of extra leading "_" due to
            polymorphism, we return the extended environment. *)
         let env_with_value_constructors =
           List.fold_left
             (fun accu_env (sum_cstr_name, _, _) ->
               Env.DkGenEnv.add_constructor
                 sum_cstr_name
                 { Env.DkGenInformation.cmi_external_translation = None }
                 accu_env)
             env
             sum_constructors_to_print in
         (* Finally add the type definition in the returned environment. *)
         Env.DkGenEnv.add_type
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
          processed field is not the last of the list (Dk syntax need). *)
       let rec local_print_fields = function
         | [] -> ()
         | (field_name, field_mut, field_ty) :: q ->
             (* The mutable fields are not yet supported for Dk code. *)
             if field_mut = Env.TypeInformation.FM_mutable then
               raise
                 (Mutable_record_fields_not_in_dk
                    (type_descr.Env.TypeInformation.type_loc, field_name)) ;
             Format.fprintf out_fmter "%a :@ %a"
               Parsetree_utils.pp_vname_with_operators_expanded field_name
               (Dk_pprint.pp_type_simple_to_dk print_ctx) field_ty ;
             if q <> [] then Format.fprintf out_fmter ";" ;
             Format.fprintf out_fmter "@\n" ;
             local_print_fields q in
       (* Do the printing job... *)
       local_print_fields record_fields_to_print ;
       Format.fprintf out_fmter " }.@]@\n " ;
       if not as_zenon_fact then (
         (* Add the record labels in the environment like we do for constructors
            in sum types. Same remarks, same process. *)
         let env_with_record_labels =
           List.fold_left
             (fun accu_env (label_name, _, _) ->
               Env.DkGenEnv.add_label
                 label_name
                 { Env.DkGenInformation.lmi_external_translation = None }
                 accu_env)
             env
             record_fields_to_print in
         (* Finally add the type definition in the returned environment. *)
         Env.DkGenEnv.add_type
           ~loc: type_descr.Env.TypeInformation.type_loc type_def_name
           type_descr env_with_record_labels
        )
       else env
       end)
;;
