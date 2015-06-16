type coq_print_context = {
  cpc_current_unit : Types.fname ;
  cpc_current_species : Types.type_collection option ;
  cpc_collections_carrier_mapping : Types.collection_carrier_mapping
} ;;



let (pp_type_simple_to_coq, pp_type_variable_to_coq, pp_type_simple_args_to_coq,
     purge_type_simple_to_coq_variable_mapping
     (* DEBUG
     , debug_variable_mapping *)) =
  (* ************************************************************** *)
  (* ((type_simple * string) list) ref                              *)
  (** {b Descr} : The mapping giving for each variable already seen
      the name used to denote it while printing it.

      {b Rem} : Not exported. This mapping is purely local to the
      pretty-print function of type into the FoCaLize syntax. It is
      especially not shared with the type printing routine used to
      generate the OCaml code or the FoCaLize feedback.               *)
  (* ************************************************************* *)
  let type_variable_names_mapping =
    ref ([] : (Types.type_variable * string) list) in

  (* ************************************************************** *)
  (* int ref                                                        *)
  (** {b Descr} : The counter counting the number of different
      variables already seen hence printed. It serves to generate a
      fresh name to new variables to print.

      {b Rem} : Not exported. This counter is purely local to the
      pretty-print function of type into the FoCaLize syntax. It is
      especially not shared with the type printing routine used to
      generate the OCaml or Coq code.                               *)
  (* ************************************************************** *)
  let type_variables_counter = ref 0 in

  (* ************************************************************* *)
  (* unit -> unit                                                  *)
  (** {b Descr} : Resets the variables names mapping an counter.
      This allows to stop name-sharing between type prints.

      {b Rem} : Not exported. This counter is purely local to the
      pretty-print function of type into the FoCaLize syntax. It is
      especially not shared with the type printing routine used to
      generate the OCaml or Coq code.                              *)
  (* ************************************************************* *)
  let reset_type_variables_mapping_to_coq () =
    type_variable_names_mapping := [] ;
    type_variables_counter := 0 in

  let get_or_make_type_variable_name_to_coq ty_var =
    (* No need to repr, [rec_pp_to_coq] already did it. *)
    try List.assq ty_var !type_variable_names_mapping with
    | Not_found ->
        let name =
          (if not (Types.is_generalized_type_variable ty_var) then
            (* Attention this is a weak-polymorphic variable. Hence, it is
               *not* bound by any extra forall ! Generating a new variable
               name will lead to an unbound type variable !
               Instead, we "cheat" replacing this variable by the internal
               type we defined in Coq: 'coq_builtins.weak_poly_var_ty' *)
            "coq_builtins.weak_poly_var_ty"
          else
            let tmp =
              "__var_" ^ (Handy.int_to_base_26 !type_variables_counter) in
            incr type_variables_counter ;
            tmp) in
        type_variable_names_mapping :=
          (ty_var, name) :: !type_variable_names_mapping ;
        name in


  let pp_type_name_to_coq ~current_unit ppf (hosting_module, constructor_name) =
    let constructor_name' =
      Anti_keyword_conflict.string_to_no_keyword_string constructor_name in
    if current_unit = hosting_module then
      Format.fprintf ppf "%s__t" constructor_name'
    else
      (* In Coq, no file name capitalization ! *)
      Format.fprintf ppf "%s.%s__t" hosting_module constructor_name' in


  let internal_pp_var_to_coq ppf ty_var =
    let ty_variable_name = get_or_make_type_variable_name_to_coq ty_var in
    Format.fprintf ppf "%s" ty_variable_name
    (* DEBUG
    ; Format.fprintf ppf "(*%d,l:%d*)" ty_var.tv_debug ty_var.tv_level *)
    in


  let rec rec_pp_to_coq ctx prio ppf ty =
    match ty with
    | Types.ST_var ty_var -> internal_pp_var_to_coq ppf ty_var
    | Types.ST_arrow (ty1, ty2) ->
        (* Arrow priority: 2. *)
        if prio >= 2 then Format.fprintf ppf "@[<1>(" ;
        Format.fprintf ppf "@[<2>%a@ ->@ %a@]"
          (rec_pp_to_coq ctx 2) ty1
          (rec_pp_to_coq ctx 1) ty2 ;
        if prio >= 2 then Format.fprintf ppf ")@]"
    | Types.ST_sum_arguments tys ->
        (* In coq, constructors' arguments are curried, not tupled. *)
        if prio >= 3 then Format.fprintf ppf "@[<1>(" ;
        Format.fprintf ppf "@[<2>%a@]"
          (rec_pp_to_coq_sum_arguments ctx 3) tys ;
        if prio >= 3 then Format.fprintf ppf ")@]"
    | Types.ST_tuple tys ->
        (* Tuple priority: 3. *)
        if prio >= 3 then Format.fprintf ppf "@[<1>(" ;
        Format.fprintf ppf "@[<2>((%a)%%type)@]"
          (rec_pp_to_coq_tuple ctx 3) tys ;
        if prio >= 3 then Format.fprintf ppf ")@]"
    | Types.ST_construct (type_name, arg_tys) ->
        (begin
        (* Priority of arguments of a sum type constructor : like an regular
           application : 0. *)
        match arg_tys with
         | [] -> Format.fprintf ppf "%a"
               (pp_type_name_to_coq ~current_unit: ctx.cpc_current_unit)
               type_name
         | _ ->
             Format.fprintf ppf "@[<1>(%a@ %a)@]"
               (pp_type_name_to_coq ~current_unit: ctx.cpc_current_unit)
               type_name
               (Handy.pp_generic_separated_list " "
                  (rec_pp_to_coq ctx 0)) arg_tys
        end)
    | Types.ST_prop -> Format.fprintf ppf "coq_builtins.prop__t"
    | Types.ST_self_rep ->
        (begin
        match ctx.cpc_current_species with
         | None ->
             (* Referencing "Self" outside a species should have been caught
                earlier, i.e. at typechecking stage. *)
             assert false
         | Some (species_modname, _) ->
             (begin
             (* Obviously, Self should refer to the current species. This
                means that the CURRENT species MUST be in the CURRENT
                compilation unit ! *)
             assert (species_modname = ctx.cpc_current_unit) ;
             (* If "Self" is kept abstract, then it won't appear in the
                collection_carrier_mapping and must be printed like "abst_T"
                (for instance when printing in a field definition). Otherwise
                it may show the species from which it is the carrier (when
                printing the record type) and must appear in the
                collection_carrier_mapping. *)
             try
               let (self_as_string, _) =
                 List.assoc
                   (species_modname, "Self")
                   ctx.cpc_collections_carrier_mapping in
               Format.fprintf ppf "%s_T" self_as_string
             with Not_found ->  Format.fprintf ppf "abst_T"
             end)
        end)
    | Types.ST_species_rep (module_name, collection_name) ->
        (begin
        try
          let (coll_type_variable, kind) =
            List.assoc
              (module_name, collection_name)
              ctx.cpc_collections_carrier_mapping in
          match kind with
           | Types.CCMI_is -> Format.fprintf ppf "%s_T" coll_type_variable
           | Types.CCMI_in provenance -> (
               match provenance with
               | Types.SCK_toplevel_collection | Types.SCK_toplevel_species ->
                   Format.fprintf ppf "%s.me_as_carrier" collection_name
               | Types.SCK_species_parameter ->
                   Format.fprintf ppf "%s_T" coll_type_variable
              )
        with Not_found ->
          (* If the carrier is not in the mapping created for the species
             parameters, that's because the searched species carrier's is not
             a species parameter, i.e. it's a toplevel species.
             And as always, the type's name representing a species's carrier
             is the species's name + "me_as_carrier" with a possible module
             prefix qualification if the species belongs to a file that is not
             the currently compiled one. *)
          if ctx.cpc_current_unit = module_name then
            Format.fprintf ppf "%s.me_as_carrier" collection_name
          else
            Format.fprintf ppf "%s.%s.me_as_carrier" module_name collection_name
        end)

  (* ********************************************************************* *)
  (** {b Descr} : Encodes FoCaLize tuples into nested pairs because Coq
      doesn't have tuples with abitrary arity: it just has pairs.
      Associativity is on the left, i.e, a FoCaLize tuple "(1, 2, 3, 4)" will
      be mapped onto the Coq "(prod 1 (prod 2 (prod 3 4)))" data structure.

      {b Rem} : Not exported outside this module.                          *)
  (* ********************************************************************* *)
  and rec_pp_to_coq_tuple ctx prio ppf = function
    | [] -> assert false  (* Tuples should never have 0 component. *)
    | [last] ->
        Format.fprintf ppf "%a" (rec_pp_to_coq ctx prio) last
    | ty1 :: ty2 :: rem ->
        Format.fprintf ppf "%a@ * %a"
          (rec_pp_to_coq ctx prio) ty1
          (rec_pp_to_coq_tuple ctx prio)
          (ty2 :: rem)



  and rec_pp_to_coq_sum_arguments ctx prio ppf = function
    | [] -> ()
    | [last] ->
        Format.fprintf ppf "%a" (rec_pp_to_coq ctx prio) last
    | ty1 :: ty2 :: rem ->
        Format.fprintf ppf "%a@ -> %a"
          (rec_pp_to_coq ctx prio) ty1
          (rec_pp_to_coq_sum_arguments ctx prio)
          (ty2 :: rem)



  and rec_pp_to_coq_args ctx ppf t n =
    match t with
    | Types.ST_construct (_, arg_tys) ->
       Format.fprintf ppf " %a" (Handy.pp_generic_separated_list ""
                                  (rec_pp_to_coq ctx 0)) arg_tys
    | _ -> for _i = 0 to n - 1 do Format.fprintf ppf "@ _" done in



  (* ************************************************** *)
  (* Now, the real definition of the printing functions *)
  ((* pp_type_simple_to_coq *)
   (fun ctx ppf ty -> rec_pp_to_coq ctx 0 ppf (Types.view_type_simple ty)),
   (* pp_type_variable_to_coq *)
   (fun ppf ty_var -> internal_pp_var_to_coq ppf ty_var),
   (* pp_type_simple_args_to_coq *)
   (fun ctx ppf ty n -> rec_pp_to_coq_args ctx ppf (Types.view_type_simple ty) n),
   (* purge_type_simple_to_coq_variable_mapping *)
   (fun () -> reset_type_variables_mapping_to_coq ())
   (* DEBUG
   ,
   (* debug_variable_mapping *)
   (fun () ->
     List.iter
       (fun (var, name) ->
         Format.eprintf "(%d, %s) " var.tv_debug name)
       !type_variable_names_mapping ;
     Format.eprintf "@.") *)
  )
;;
