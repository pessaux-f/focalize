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

type dk_print_context = {
  dpc_current_unit : Types.fname ;
  dpc_current_species : Types.type_collection option ;
  dpc_collections_carrier_mapping : Types.collection_carrier_mapping
} ;;


let (pp_type_simple_to_dk, pp_type_variable_to_dk, pp_type_simple_args_to_dk,
     purge_type_simple_to_dk_variable_mapping, has_cbv, pp_for_cbv_type_simple_to_dk
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
  let reset_type_variables_mapping_to_dk () =
    type_variable_names_mapping := [] ;
    type_variables_counter := 0 in

  let get_or_make_type_variable_name_to_dk ty_var =
    (* No need to repr, [rec_pp_to_dk] already did it. *)
    try List.assq ty_var !type_variable_names_mapping with
    | Not_found ->
        let name =
          (if not (Types.is_generalized_type_variable ty_var) then
            (* Attention this is a weak-polymorphic variable. Hence, it is
               *not* bound by any extra forall ! Generating a new variable
               name will lead to an unbound type variable !
               Instead, we "cheat" replacing this variable by the internal
               type we defined in Dk: 'dk_builtins.weak_poly_var_ty' *)
            "dk_builtins.weak_poly_var_ty"
          else
            let tmp =
              "__var_" ^ (Handy.int_to_base_26 !type_variables_counter) in
            incr type_variables_counter ;
            tmp) in
        type_variable_names_mapping :=
          (ty_var, name) :: !type_variable_names_mapping ;
        name in

  let type_module ~current_unit (hosting_module, _) =
    if current_unit = hosting_module then None else Some hosting_module in

  let pp_type_name_to_dk_no_module ppf (_, constructor_name) =
    let constructor_name' =
      Anti_keyword_conflict.string_to_no_keyword_string constructor_name in
    Format.fprintf ppf "%s__t" constructor_name'
  in

  let pp_type_name_to_dk ~current_unit ppf ty_name =
    match type_module ~current_unit ty_name with
    | None -> pp_type_name_to_dk_no_module ppf ty_name
    | Some m ->
      (* In Dedukti, no file name capitalization ! *)
       Format.fprintf ppf "%s.%a" m
         pp_type_name_to_dk_no_module ty_name in

  let internal_pp_var_to_dk ppf ty_var =
    let ty_variable_name = get_or_make_type_variable_name_to_dk ty_var in
    Format.fprintf ppf "%s" ty_variable_name
    (* DEBUG
    ; Format.fprintf ppf "(*%d,l:%d*)" ty_var.tv_debug ty_var.tv_level *)
    in

  let to_dk_module ctx ty =
    match ty with
    | Types.ST_var _ -> None
    | Types.ST_arrow (Types.ST_sum_arguments _, _) -> None
    | Types.ST_arrow _ -> None (* We don't put "cc" here because parens get in the way *)
    | Types.ST_sum_arguments _ -> None
    | Types.ST_tuple _ -> None (* Same as arrow *)
    | Types.ST_construct (ty_name, []) ->
       type_module ~current_unit: ctx.dpc_current_unit ty_name
    | Types.ST_construct (_, _) -> None (* parens again *)
    | Types.ST_prop -> Some "dk_builtins"
    | Types.ST_self_rep -> None
    | Types.ST_species_rep (module_name, _) ->
       if module_name = ctx.dpc_current_unit then None else Some module_name
  in

  let rec rec_pp_to_dk_no_module ctx prio ppf ty =
    match ty with
    | Types.ST_var ty_var -> internal_pp_var_to_dk ppf ty_var
    | Types.ST_arrow (Types.ST_sum_arguments tys, ty2) ->
        if prio >= 3 then Format.fprintf ppf "@[<1>(" ;
        Format.fprintf ppf "@[<2>%a@]"
          (rec_pp_to_dk_sum_arguments ~ret_type:ty2 ctx 3) tys ;
        if prio >= 3 then Format.fprintf ppf ")@]"
    | Types.ST_arrow (ty1, ty2) ->
        Format.fprintf ppf "@[<1>(@[<2>cc.Arrow@ %a@ %a@])@]"
          (rec_pp_to_dk ctx 2) ty1
          (rec_pp_to_dk ctx 1) ty2 ;
    | Types.ST_sum_arguments _ ->
       failwith "In Dedukti, sum arguments are only allowed in the left part of an arrow@\n"
    | Types.ST_tuple tys ->
        (* Tuple priority: 3. *)
        if prio >= 3 then Format.fprintf ppf "@[<1>(" ;
        Format.fprintf ppf "@[<2>(%a)@]"
          (rec_pp_to_dk_tuple ctx 3) tys ;
        if prio >= 3 then Format.fprintf ppf ")@]"
    | Types.ST_construct (type_name, arg_tys) ->
        (begin
        (* Priority of arguments of a sum type constructor : like an regular
           application : 0. *)
        match arg_tys with
         | [] -> pp_type_name_to_dk_no_module ppf type_name
         | _ ->
             Format.fprintf ppf "@[<1>(%a@ %a)@]"
               (pp_type_name_to_dk ~current_unit: ctx.dpc_current_unit)
               type_name
               (Handy.pp_generic_separated_list " "
                  (rec_pp_to_dk ctx 0)) arg_tys
        end)
    | Types.ST_prop -> Format.fprintf ppf "prop"
    | Types.ST_self_rep ->
        (begin
        match ctx.dpc_current_species with
         | None ->
             (* Referencing "Self" outside a species should have been caught
                earlier, i.e. at typechecking stage. *)
             assert false
         | Some (species_modname, _) ->
             (begin
             (* Obviously, Self should refer to the current species. This
                means that the CURRENT species MUST be in the CURRENT
                compilation unit ! *)
             (* If "Self" is kept abstract, then it won't appear in the

                 (* /!\ Assertion failure!! *)

                 (* assert (species_modname = ctx.dpc_current_unit) ; *)
                collection_carrier_mapping and must be printed like "abst_T"
                (for instance when printing in a field definition). Otherwise
                it may show the species from which it is the carrier (when
                printing the record type) and must appear in the
                collection_carrier_mapping. *)
             try
               let (self_as_string, _) =
                 List.assoc
                   (species_modname, "Self")
                   ctx.dpc_collections_carrier_mapping in
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
              ctx.dpc_collections_carrier_mapping in
          match kind with
           | Types.CCMI_is -> Format.fprintf ppf "%s_T" coll_type_variable
           | Types.CCMI_in provenance -> (
               match provenance with
               | Types.SCK_toplevel_collection | Types.SCK_toplevel_species ->
                   Format.fprintf ppf "%s__me_as_carrier" collection_name
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
            Format.fprintf ppf "%s__me_as_carrier" collection_name
          end)

  and rec_pp_to_dk ctx prio ppf ty =
    match to_dk_module ctx ty with
    | None -> rec_pp_to_dk_no_module ctx prio ppf ty
    | Some m -> Format.fprintf ppf "%s.%a" m
                 (rec_pp_to_dk_no_module ctx prio) ty

  (* ********************************************************************* *)
  (** {b Descr} : Encodes FoCaLize tuples into nested pairs because Dk
      doesn't have tuples with abitrary arity: it just has pairs.
      Associativity is on the left, i.e, a FoCaLize tuple "(1, 2, 3, 4)" will
      be mapped onto the Dedukti "(prod 1 (prod 2 (prod 3 4)))" data structure.

      {b Rem} : Not exported outside this module.                          *)
  (* ********************************************************************* *)
  and rec_pp_to_dk_tuple ctx prio ppf = function
    | [] -> assert false  (* Tuples should never have 0 component. *)
    | [last] ->
        Format.fprintf ppf "%a" (rec_pp_to_dk ctx prio) last
    | ty1 :: ty2 :: rem ->
        Format.fprintf ppf "dk_tuple.prod@ %a@ (%a)"
          (rec_pp_to_dk ctx prio) ty1
          (rec_pp_to_dk_tuple ctx 0)
          (ty2 :: rem)



  and rec_pp_to_dk_sum_arguments ~ret_type ctx prio ppf = function
    | [] ->
        Format.fprintf ppf "%a" (rec_pp_to_dk ctx prio) ret_type
    | ty1 :: rem ->
        Format.fprintf ppf "(cc.Arrow@ %a@ %a)"
          (rec_pp_to_dk ctx prio) ty1
          (rec_pp_to_dk_sum_arguments ~ret_type ctx prio)
          rem



  and rec_pp_to_dk_args ctx ppf t n =
    match t with
    | Types.ST_construct (_, arg_tys) ->
       Format.fprintf ppf " %a" (Handy.pp_generic_separated_list ""
                                  (rec_pp_to_dk ctx 0)) arg_tys
    | Types.ST_tuple l ->
       List.iter (fun t -> Format.fprintf ppf "@ %a"
                                       (rec_pp_to_dk ctx 0) t)
                 l
    | t ->
       (* In Dedukti, we cannot print underscores
          instead of inferable type variables.
          Printing the type will not be satisfactory,
          it may be replaced by an error.
        *)
       Format.fprintf ppf "@ %a (; from %d underscores ;)"
                      (rec_pp_to_dk ctx 0) t n
  in

  let has_cbv ty =
    match ty with
    | Types.ST_tuple _ | Types.ST_construct _ -> true
    | Types.ST_var _ | Types.ST_arrow _ | Types.ST_sum_arguments _ | Types.ST_prop
    | Types.ST_self_rep | Types.ST_species_rep _ -> false
  in

  let rec_pp_cbv_to_dk_tuple ctx prio ppf = function
    | [] -> assert false  (* Tuples should never have 0 component. *)
    | [last] ->
        Format.fprintf ppf "%a" (rec_pp_to_dk ctx prio) last
    | ty1 :: ty2 :: rem ->
        Format.fprintf ppf "dk_tuple.call_by_value_prod@ %a@ %a"
          (rec_pp_to_dk ctx prio) ty1
          (rec_pp_to_dk_tuple ctx 0)
          (ty2 :: rem)
  in

  let rec_pp_cbv_to_dk_no_module ctx prio ppf ty =
    match ty with
    | Types.ST_tuple tys ->
        (* Tuple priority: 3. *)
        if prio >= 3 then Format.fprintf ppf "@[<1>(" ;
        Format.fprintf ppf "@[<2>(%a)@]"
          (rec_pp_cbv_to_dk_tuple ctx 3) tys ;
        if prio >= 3 then Format.fprintf ppf ")@]"
    | Types.ST_construct (type_name, arg_tys) ->
        (begin
        (* Priority of arguments of a sum type constructor : like an regular
           application : 0. *)
        match arg_tys with
         | [] -> Format.fprintf ppf "call_by_value_%a"
               pp_type_name_to_dk_no_module type_name
         | _ ->
             Format.fprintf ppf "@[<1>(call_by_value_%a@ %a)@]"
               pp_type_name_to_dk_no_module type_name
               (Handy.pp_generic_separated_list " "
                  (rec_pp_to_dk ctx 0)) arg_tys
        end)
    | _ -> assert false
                 (* rec_pp_cbv_to_dk should only be called when has_cbv returns true *)
  in

  let rec_pp_cbv_to_dk ctx prio ppf ty =
    match to_dk_module ctx ty with
    | None -> rec_pp_cbv_to_dk_no_module ctx prio ppf ty
    | Some m -> Format.fprintf ppf "%s.%a" m
                 (rec_pp_cbv_to_dk_no_module ctx prio) ty
  in

  (* ************************************************** *)
  (* Now, the real definition of the printing functions *)
  ((* pp_type_simple_to_dk *)
   (fun ctx ppf ty -> rec_pp_to_dk ctx 0 ppf (Types.view_type_simple ty)),
   (* pp_type_variable_to_dk *)
   (fun ppf ty_var -> internal_pp_var_to_dk ppf ty_var),
   (* pp_type_simple_args_to_dk *)
   (fun ctx ppf ty n -> rec_pp_to_dk_args ctx ppf (Types.view_type_simple ty) n),
   (* purge_type_simple_to_dk_variable_mapping *)
   (fun () -> reset_type_variables_mapping_to_dk ()),
   (* has_cbv *)
   (fun ty -> has_cbv (Types.view_type_simple ty)),
   (* pp_for_cbv_type_simple_to_dk *)
   (fun ctx ppf ty -> rec_pp_cbv_to_dk ctx 0 ppf (Types.view_type_simple ty))
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
