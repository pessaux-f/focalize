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

(* $Id: type_coq_generation.ml,v 1.4 2008-04-09 13:01:47 pessaux Exp $ *)



(* ************************************************************************* *)
(* Types.coq_print_context -> Format.formatter -> Types.type_simple list ->  *)
(*   unit                                                                    *)
(** {b  Descr} : Generate the parameters of a type definition. Each variable
    is generated as being of type "Set". We do not need any carrier mapping
    since type definitions are always outside a species, hence can never
    refer to a species parameter's carrier.
    We must share the variable-mapping for each printed type since the
    definition variables were inserted inside before we go on printing here.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let print_types_parameters_sharing_vmapping_and_empty_carrier_mapping
    print_ctx out_fmter tys =
  List.iter
    (fun ty ->
      Format.fprintf out_fmter "(%a : Set)@ "
        (Types.pp_type_simple_to_coq
           print_ctx ~reuse_mapping: true ~self_as: Types.CSR_self)
        ty)
    tys
;;



(** [nb_extra_args] : the number of extra argument of type "Set" used to
     represent the polymorphism in case where the constructor belongs to a
     polymorphic type. *)
let extend_coq_gen_env_with_type_external_bindings env nb_extra_args
    external_bindings =
  let rec rec_extend rec_env = function
    | [] -> rec_env
    | binding :: rem_bindings ->
        let (bound_name, bound_external_expr) = binding.Parsetree.ast_desc in
        let rec_env' =
          (match bound_name with
           | Parsetree.Vlident _ ->
(*
[Unsure]
               (* Starting by a lowercase letter means record field name. *)
               Env.CoqGenEnv.add_label
                 bound_name bound_external_expr.Parsetree.ast_desc rec_env
*) failwith "todo"
           | Parsetree.Vuident _ ->
               (* Starting by an uppercase letter means sum constructor. *)
               let cstr_mapping_info = {
                 Env.CoqGenInformation.cmi_num_polymorphics_extra_args =
                   nb_extra_args ;
                 Env.CoqGenInformation.cmi_external_expr =
                   bound_external_expr.Parsetree.ast_desc } in
               Env.CoqGenEnv.add_constructor
                 bound_name cstr_mapping_info rec_env
           | _ ->
               (* This syntactically should never arise. *)
               assert false) in
        rec_extend rec_env' rem_bindings in
  (* ********************** *)
  (* Now, let's do the job. *)
  rec_extend env external_bindings.Parsetree.ast_desc
;;



let type_def_compile ctx env type_def_name type_descr =
  let out_fmter = ctx.Context.rcc_out_fmter in
  (* Build the print context for the methods once for all. *)
  let print_ctx = {
    Types.cpc_current_unit = ctx.Context.rcc_current_unit ;
    Types.cpc_current_species = None ;
    Types.cpc_collections_carrier_mapping =
      ctx.Context.rcc_collections_carrier_mapping } in
  (* Get a fresh instance of the type's identity scheme directly   *)
  (* instanciated with the variables that will serve as parameters *)
  (* of the definition. We keep the list of these variables to be  *)
  (* able to print them in front of the type constructor in the    *)
  (* OCaml definition.                                             *)
  let type_def_params =
    List.map
      (fun _ -> Types.type_variable ())
      type_descr.Env.TypeInformation.type_params in
  let instanciated_body =
    Types.specialize_with_args
      type_descr.Env.TypeInformation.type_identity type_def_params in
  (* Useless, but just in case.... This does not hurt ! *)
  Types.purge_type_simple_to_ml_variable_mapping () ;
  (* Now, generates the type definition's body. *)
  match type_descr.Env.TypeInformation.type_kind with
   | Env.TypeInformation.TK_abstract ->
       Format.fprintf out_fmter "@[<2>Definition %a__t@ "
         Parsetree_utils.pp_vname_with_operators_expanded type_def_name ;
       (* Print the parameter(s) stuff if any. *)
       print_types_parameters_sharing_vmapping_and_empty_carrier_mapping
         print_ctx out_fmter type_def_params ;
       (* Since types are toplevel, the way "Self" is printed is non *)
       (* relevant. Indeed, "Self" can only appear inside the scope  *)
       (* of a species, hence never at toplevel, hence never in a    *)
       (* type definition.                                           *)
       Format.fprintf out_fmter ":=@ %a.@]@\n"
         (Types.pp_type_simple_to_coq
            print_ctx ~reuse_mapping: true ~self_as: Types.CSR_self)
         instanciated_body ;
       (* Not an external type definition, so nothing new in the environment. *)
       env
   | Env.TypeInformation.TK_external (external_expr, external_bindings) ->
       (begin
       Format.fprintf out_fmter "@[<2>Definition %a__t@ "
         Parsetree_utils.pp_vname_with_operators_expanded type_def_name ;
       (* Print the parameter(s) stuff if any. *)
       print_types_parameters_sharing_vmapping_and_empty_carrier_mapping
         print_ctx out_fmter type_def_params ;
       Format.fprintf out_fmter ":=@ " ;
       (* And now, bind the FoCaL identifier to the Coq one. *)
       (try
         let (_, coq_binding) =
           List.find
             (function
               | (Parsetree.EL_Coq, _) -> true
               | (Parsetree.EL_Caml, _)
               | ((Parsetree.EL_external _), _) -> false)
             external_expr.Parsetree.ast_desc in
         Format.fprintf out_fmter "%s@]@ ;;@\n" coq_binding
       with Not_found ->
         (* We didn't find any correspondance for Coq. *)
         raise
           (Externals_generation_errs.No_external_type_def
              ("Coq", type_def_name, external_expr.Parsetree.ast_loc))) ;
       (* Compute the number of extra polymorphic-induced *)
       (* arguments to the constructor.                   *)
       let nb_extra_args = List.length type_def_params in
        (* Finally, we return the extended code generation environment in *)
       (* which sum constructors or labels are recorded in order to be   *)
       (* able to remind on what to map them when we will see them.      *)
       extend_coq_gen_env_with_type_external_bindings
         env nb_extra_args external_bindings
       end)
   | _ ->
       (* [Unsure] *)
       Format.eprintf "Types in coq todo !!!!!!!!!!!!!!@." ;
       env
;;
