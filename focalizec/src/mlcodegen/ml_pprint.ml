
(** ****************************************************************************
    {b Descr} : "Compile", i.e. generate the OCaml source representation of
    a type. Basically, proceeds like the regular [pp_type_simple]
    except in 2 cases:
      - when encountering [Self] : in this case, generates the type variable
        name representing [Self], i.e. by convention "'abst_T",
      - when encountering a species carrier type : in this case, generate
        the type variable name representing this species (recover it thanks
        to the mapping between collections names and type variables names
        [collections_carrier_mapping]).

    Be carreful : because we generate OCaml code, remind that in OCaml
    expressions, variables that are present in "source code types" do not
    involve any notion of "generalized" or "not generalized". Hence, if we
    want to write a type variable in an OCaml source code, we always write
    it as "'a" and never as "'_a" otherwise it is lexically incorrect.
    OCaml will do the job itself to check whether the variable is
    generalizable or not.
    FOR THIS REASON, when we print type variables here, we only consider
    that they are generalised, to get a printing without any underscore
    in the variable's name.

    {b Args} :
      - [current_unit] : The string giving the name of the current
          compilation unit we are generating the OCaml code of. This is
          required to prevent, when printing types, to qualify type
          constructors with the OCaml module name if the type belongs
          to the currently compiled compilation unit. Hence this
          prevents things like in a "bar.foc" file containing
          [type t1 = ... ; type t2 = t1 * t2], getting in the generated
          OCaml file definitions like [type t1 = ... ;
          type t2 = Bar.t1 * Bar.t1] which would lead to an OCaml module
          depending of itself.
      - [collections_carrier_mapping] : Mapping giving for each collection
          in the scope of the printing session, which type variable name is
          used to represent in OCaml this collection carrier's type.
      - [ppf] : Out channel where to send the text of the printed type.
      - [whole_type] : Tye type expression to print.

    {b Visibility} : Exported outside this module.
 **************************************************************************** *)
let (pp_type_simple_to_ml, purge_type_simple_to_ml_variable_mapping) =
  (* ********************************************************************** *)
  (* ((type_variable * string) list) ref                                    *)
  (** {b Descr} : The mapping giving for each variable already seen the
                name used to denote it while printing it.

      {b Rem} : Not exported. This mapping is purely local to the
      pretty-print function of type into the OCaml syntax. It is especially
      not shared with the type printing routine used to generate the FoCaLize
      feedback and the Coq/Dedukti code.                *)
  (* ********************************************************************** *)
  let type_variable_names_mapping =
    ref ([] : (Types.type_variable * string) list) in

  (* ******************************************************************* *)
  (* int ref                                                             *)
  (** {b Descr} : The counter counting the number of different variables
      already seen hence printed. It serves to generate a fresh name to
      new variables to print.

      {b Rem} : Not exported. This counter is purely local to the
      pretty-print function of type into the FoCaLize syntax. It is
      especially not shared with the type printing routine used to
      generate the FoCaLize feedback and the Coq/Dedukti code.           *)
  (* ******************************************************************* *)
  let type_variables_counter = ref 0 in

  (* ************************************************************* *)
  (* unit -> unit                                                  *)
  (** {b Descr} : Resets the variables names mapping an counter.
      This allows to stop name-sharing between type prints.

     {b Rem} : Exported outside this module.
      However, this counter is purely local to the pretty-print
      function of type into the FoCaLize syntax. It is especially not
      shared with the type printing routine used to generate the
      FoCaLize feedback and the Coq/Dedukti code.                  *)
  (* ************************************************************* *)
  let reset_type_variables_mapping_to_ml () =
    type_variable_names_mapping := [] ;
    type_variables_counter := 0 in

  let get_or_make_type_variable_name_to_ml var =
    try List.assq var !type_variable_names_mapping with
    | Not_found ->
        let name = Handy.int_to_base_26 !type_variables_counter in
        incr type_variables_counter ;
        type_variable_names_mapping :=
          (var, name) :: !type_variable_names_mapping ;
        name in

  let pp_type_name_to_ml ~current_unit ppf (hosting_module, constructor_name) =
    let constructor_name' =
      Anti_keyword_conflict.string_to_no_keyword_string constructor_name in
    if current_unit = hosting_module then
      Format.fprintf ppf "_focty_%s" constructor_name'
    else
      Format.fprintf ppf "%s._focty_%s"
        (String.capitalize hosting_module) constructor_name' in

  let rec rec_pp_to_ml ~current_unit collections_carrier_mapping prio ppf ty =
    match ty with
    | Types.ST_var var ->
        (* Read the justification in the current function's header about the
           fact that we amways consider variables as generalized. *)
        let ty_variable_name = get_or_make_type_variable_name_to_ml var in
        Format.fprintf ppf "'%s" ty_variable_name
    | Types.ST_arrow (ty1, ty2) ->
        (* Arrow priority: 2. *)
        if prio >= 2 then Format.fprintf ppf "@[<1>(" ;
        Format.fprintf ppf "@[<2>%a@ ->@ %a@]"
          (rec_pp_to_ml ~current_unit collections_carrier_mapping 2) ty1
          (rec_pp_to_ml ~current_unit collections_carrier_mapping 1) ty2 ;
        if prio >= 2 then Format.fprintf ppf ")@]"
    | Types.ST_sum_arguments tys  (** Printed like tuples in OCaml. *)
    | Types.ST_tuple tys ->
        (* Tuple priority: 3. *)
        if prio >= 3 then Format.fprintf ppf "@[<1>(" ;
        Format.fprintf ppf "@[<2>%a@]"
          (Handy.pp_generic_separated_list " *"
             (rec_pp_to_ml ~current_unit collections_carrier_mapping 3)) tys ;
        if prio >= 3 then Format.fprintf ppf ")@]"
    | Types.ST_construct (type_name, arg_tys) ->
        (begin
        (* Priority of arguments of a sum type constructor :
           like tuples if only one argument : 3
           otherwise 0 if already a tuple because we force parens. *)
        match arg_tys with
         | [] ->
             Format.fprintf ppf "%a"
               (pp_type_name_to_ml ~current_unit) type_name
         | [one] ->
             Format.fprintf ppf "%a@ %a"
               (rec_pp_to_ml ~current_unit collections_carrier_mapping 3) one
               (pp_type_name_to_ml ~current_unit) type_name
         | _ ->
             Format.fprintf ppf "@[<1>(%a)@]@ %a"
               (Handy.pp_generic_separated_list ","
                  (rec_pp_to_ml ~current_unit collections_carrier_mapping 0))
               arg_tys
               (pp_type_name_to_ml ~current_unit) type_name
        end)
    | Types.ST_prop -> Format.fprintf ppf "Basics._focty_bool"
    | Types.ST_self_rep ->
        (* Here is the major difference with the regular [pp_type_simple].
           We print the type variable that represents our carrier in the
           OCaml translation. *)
        Format.fprintf ppf "'abst_T"
    | Types.ST_species_rep (module_name, collection_name) ->
        (begin
        try
          let (coll_type_variable, _) =
            List.assoc
              (module_name, collection_name) collections_carrier_mapping in
          Format.fprintf ppf "%s" coll_type_variable
        with Not_found ->
          (* If the carrier is not in the mapping created for the species
             parameters, that's because the searched species carrier's is not
             a species parameter, i.e. it's a toplevel species.
             And as always, the type's name representing a species's carrier
             is "me_as_carrier". *)
          if current_unit = module_name then
            Format.fprintf ppf "%s.me_as_carrier" collection_name
          else
            Format.fprintf ppf "%s.%s.me_as_carrier"
              (String.capitalize module_name) collection_name
        end) in

  (* ************************************************** *)
  (* Now, the real definition of the printing functions *)
  (
   (fun ~current_unit collections_carrier_mapping ppf whole_type ->
    rec_pp_to_ml
      ~current_unit collections_carrier_mapping 0 ppf
      (Types.view_type_simple whole_type)),

   (fun () -> reset_type_variables_mapping_to_ml ()))
;;
