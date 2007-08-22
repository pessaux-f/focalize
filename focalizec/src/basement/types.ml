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

(* $Id: types.ml,v 1.13 2007-08-22 14:17:08 pessaux Exp $ *)

(** Types of various identifiers in the abstract syntax tree. *)
type collection_name = string
     (** Collection name. *) ;;
type species_name = string
     (** Species name. *) ;;
type type_name = string
     (** Type name. *) ;;
type label_name = string
     (** Label name. *) ;;
type fname = string
     (** File (and "module") name. *) ;;



(* ****************************************************************** *)
(** {b Descr} : Binding level considered as describing the level of a
                generalized type.

    {b Rem} : Not exported outside this module.                       *)
(* ****************************************************************** *)
let generic_level = 100000000 ;;



(* ************************************************* *)
(* type_simple                                       *)
(** {b Descr} : Describes the type algebra of Focal.

    {b Rem} : Exported opaque outside this module.   *)
(* ************************************************* *)
type type_simple = {
  (* Binding level of the type. *)
  mutable ts_level : int ;
  (* The description of the type. *)
  ts_desc : type_simple_desc ;
  (* Value of the type. In fact, generalisation of the value of a type variable. This permits to make an already "non-variable" type equal to Self by side effect. *)
  mutable ts_link_value : type_link_value
}


(** Value of a type link (generalization of type variable's value. *)
and type_link_value =
  | TLV_unknown
  | TLV_known of type_simple


and type_simple_desc =
  | ST_var
  | ST_arrow of (type_simple * type_simple)   (** Functionnal type. *)
  | ST_tuple of type_simple list              (** Tuple type. *)
  | ST_construct of
      (** Type constructor, possibly with arguments. Encompass the types
	  related to records and sums. Any value of these types are typed as
	  a [ST_construct] whose name is the name of the record (or sum)
	  type. *)
      (type_name * type_simple list)
  | ST_self_rep       (** Carrier type of the currently analysed species. *)
  | ST_species_rep of (fname * collection_name)   (** Carrier type of a collection hosted in the specified module. *)

and type_species =
  | SPT_species_interface of (fname * species_name)
      (** Interface of a species:
	  It could be the list of its method'n'type'n'bodies, i.e.
	  ((string * type_simple * Parsetree.expr) list) but we don't want
	  a structural unification. That's not because 2 species have the
	  same signature that they have the same semantics.
	  Instead, one will get the type of the species via an environment
	  using the [species_name] and the [fname] as key. *)
  | SPT_parametrised_in of ((species_name * type_species) * type_species)
  | SPT_parametrised_is of ((collection_name * type_collection) * type_species)

and type_collection = (fname *  collection_name)
    (** Interface of a collection:
	It could be the list of its method'n'types, i.e.
	(string * type_simple) list but we don't want
	a structural unification. That's not because 2 collections have the
	same signature that they have the same semantics.
	Instead, one will get the type of the collection via an environment
	using the [collection_name] and the [fname] as key. *)
;;



(* ************************************************************************ *)
(* type_scheme                                                              *)
(** {b Descr} : Type schemes, i.e. model of types.
              Scheme parameters are silent inside the scheme. In fact, they
              are types in the body with a binding level equals to
              [generic_level].

    {b Rem} : Exported opaque outside this module.                          *)
(* ************************************************************************ *)
type type_scheme = {
  ts_nb_vars : int ;
  ts_body : type_simple ;
} ;;



exception Conflict of (type_simple * type_simple * Location.t)
  (* Those two types cannot be unified. *)
;;
exception Circularity of (type_simple * type_simple * Location.t)
  (* There is a circularity detected: the first type occurs in the second. *)
;;
exception Arity_mismatch of (type_name * int * int  * Location.t)
  (* A functional type constructor has been used with the wrong number of
  arguments. The exception carries on the name of the type and the conflicting
  arities. *)
;;

(* ******************************************************************** *)
(* type_simple -> type_simple                                           *)
(** {b Descr} : Returns the canonical representation of a type.
              Uncompression is performed only one level each time. The
              day the next levels may be needed, this will be during an
              unification, and [repr] will be called if needed to get a
              deeper canonical representation of the type (i.e. the
              canonical representation of its subterms).

    {b Rem} : Not exported outside this module.                         *)
(* ******************************************************************** *)
let rec repr ty =
  match ty with
   | { ts_link_value = TLV_known t } ->
       let val_of_t = repr t in
       ty.ts_link_value <- TLV_known val_of_t ;
       val_of_t
   | _ -> ty
;;


let (begin_definition, end_definition, current_binding_level, type_variable) =
  let current_binding_level = ref 0 in
  ((fun () -> incr current_binding_level),
   (fun () -> decr current_binding_level),
   (fun () -> !current_binding_level),
   (fun () ->
     { ts_level = !current_binding_level ;
       ts_desc = ST_var ;
       ts_link_value = TLV_unknown }))
;;

let type_basic type_name type_args =
  { ts_level = current_binding_level () ;
    ts_desc = ST_construct (type_name, type_args) ;
    ts_link_value = TLV_unknown }
;;

let type_int () = type_basic "int" [] ;;

let type_float () = type_basic "float" [] ;;

let type_bool () = type_basic "bool" [] ;;

let type_string () = type_basic "string" [] ;;

let type_char () = type_basic "char" [] ;;

let type_unit () = type_basic "unit" [] ;;

let type_arrow t1 t2 =
  { ts_level = current_binding_level () ;
    ts_desc = ST_arrow (t1, t2) ;
    ts_link_value = TLV_unknown }
;;

let type_prop () = type_basic "prop" [] ;;

let type_tuple tys =
  { ts_level = current_binding_level () ;
    ts_desc = ST_tuple tys ;
    ts_link_value = TLV_unknown }
;;



(* Generate the carrier type of the currently analysed species.  *)
let type_self () =
  { ts_level = current_binding_level () ;
    ts_desc = ST_self_rep ;
    ts_link_value = TLV_unknown }
;;


let type_rep_species ~species_module ~species_name =
  { ts_level = current_binding_level () ;
    ts_desc = ST_species_rep (species_module, species_name) ;
    ts_link_value = TLV_unknown }
;;



let type_variable_names_mapping = ref ([] : (type_simple * string) list) ;;
let type_variables_counter = ref 0 ;;

let reset_type_variables_mapping () =
  type_variable_names_mapping := [] ;
  type_variables_counter := 0
;;

let get_type_name ty ~generalized_p =
  (* No need to repr, [pp_type_simple] already did it. *)
  try List.assq ty !type_variable_names_mapping with
  | Not_found ->
    let name =
      String.make 1 (Char.chr (Char.code 'a' + !type_variables_counter)) in
    incr type_variables_counter ;
    let name' = if not generalized_p then "_" ^ name else name in
    type_variable_names_mapping :=
      (ty, name') :: !type_variable_names_mapping ;
    name'
;;


let (pp_type_simple, pp_type_scheme) =
  let rec rec_pp prio ppf ty =
    (* First of all get the "repr" guy ! *)
    let ty = repr ty in
    match ty.ts_desc with
    | ST_var ->
	let ty_variable_name =
	  get_type_name ty ~generalized_p: (ty.ts_level = generic_level) in
	Format.fprintf ppf "'%s" ty_variable_name
    | ST_arrow (ty1, ty2) ->
	(* Arrow priority: 2. *)
	if prio >= 2 then Format.fprintf ppf "@[<1>(" ;
        Format.fprintf ppf "@[<2>%a@ ->@ %a@]" (rec_pp 2) ty1 (rec_pp 1) ty2 ;
	if prio >= 2 then Format.fprintf ppf ")@]"
    | ST_tuple tys ->
	(* Tuple priority: 3. *)
	if prio >= 3 then Format.fprintf ppf "@[<1>(" ;
        Format.fprintf ppf "@[<2>%a@]"
          (Handy.pp_generic_separated_list " *" (rec_pp 3)) tys ;
	if prio >= 3 then Format.fprintf ppf ")@]"
    | ST_construct (type_name, arg_tys) ->
        (begin
	(* Priority of arguments of a sum type constructor :       *)
        (* like tuples if only one argument : 3                    *)
        (* otherwise 0 if already a tuple because we force parens. *)
	match arg_tys with
         | [] -> Format.fprintf ppf "%s" type_name
         | [one] -> Format.fprintf ppf "%a@ %s" (rec_pp 3) one type_name
         | _ ->
             Format.fprintf ppf "@[<1>(%a)@]@ %s"
               (Handy.pp_generic_separated_list "," (rec_pp 0)) arg_tys
	       type_name
	end)
    | ST_self_rep -> Format.fprintf ppf "Self"
    | ST_species_rep (module_name, collection_name) ->
	Format.fprintf ppf "%s#%s" module_name collection_name in

  (fun ppf ty ->
    reset_type_variables_mapping () ;
    rec_pp 0 ppf ty),
  (fun ppf the_scheme ->
    reset_type_variables_mapping () ;
    Format.fprintf ppf "%a" (rec_pp 0) the_scheme.ts_body)
;;



(** {b Rem} : Non exported oustide this module. *)
let occur_check ~loc var_ty ty =
  let rec test t =
    let t = repr t in
    match t.ts_desc with
     | ST_var -> if var_ty == t then raise (Circularity (var_ty, ty, loc))
     | ST_arrow (ty1, ty2) -> test ty1 ; test ty2
     | ST_tuple tys -> List.iter test tys
     | ST_construct (_, args) -> List.iter test args
     | ST_self_rep | ST_species_rep _ -> () in
  test ty
;;



let (specialize, specialize2) =
  let seen = ref [] in
  (* Internal recursive copy of a type scheme replacing its generalized
     variables by their associated new fresh type variables. *)
  let rec copy_type_simple ~abstractize ty =
    let ty = repr ty in
    (* First check if we already saw this type. *)
    if List.mem_assq ty !seen then List.assq ty !seen
    else
      (begin
      (* If the type is not generalized, then its copy is itself. *)
      if ty.ts_level <> generic_level then ty
      else
	(begin
	(* We must inform that "we saw ourself". The problem is we do  *)
	(* not have any "copied type" to bind to ourself in the [seen] *)
        (* list. Then we will temporarily bind to a type variable and  *)
        (* once we will get our opy, we will make this variable equal  *)
        (* to this copy.                                               *)
        let tmp_ty = {
	  ts_level = ty.ts_level ;
	  ts_desc = ST_var ;
	  ts_link_value = TLV_unknown } in
	seen := (ty, tmp_ty) :: !seen ;
	(* Build the type description copy of ourself. *)
	let copied_desc =
	  (match ty.ts_desc with
	   | ST_var -> ST_var
	   | ST_arrow (ty1, ty2) ->
               ST_arrow
		 (copy_type_simple ~abstractize ty1,
		  copy_type_simple ~abstractize ty2)
	   | ST_tuple tys ->
	       ST_tuple (List.map (copy_type_simple ~abstractize) tys)
	   | ST_construct (name, args) ->
               ST_construct
		 (name, List.map (copy_type_simple ~abstractize) args)
	   | ST_self_rep ->
	       (begin
	       match abstractize with
		| None -> ST_self_rep
		| Some coll_name -> ST_species_rep coll_name
	       end)
	   | (ST_species_rep _) as tdesc -> tdesc) in
	(* Build the type expression copy of ourself. *)
	let copied_ty = {
	  ts_level = current_binding_level () ;
	  ts_desc = copied_desc ;
	  ts_link_value = TLV_unknown } in
	(* Make our previous temporay variable equal to our copy. *)
	tmp_ty.ts_link_value <- TLV_known copied_ty ;
	(* And finally return our copy. *)
	copied_ty
	end)
      end) in



  ((* ******************************************************************** *)
   (* specialize                                                           *)
   (* type_scheme -> type_simple                                           *)
   (* {b Descr} : Instanciates a type scheme. This means that is makes a
                copy of its body, replacing generalized variables by fresh
                variables. If a generalized variable appears several times
                it will be replace by the same fresh variable at these
                different locations.

      {b Rem} : Exported oustide this module.                              *)
   (* ******************************************************************** *)
   (fun scheme ->
     (* Copy the type scheme's body. *)
     let instance = copy_type_simple ~abstractize: None scheme.ts_body in
     (* Clean up seen type for further usages. *)
     seen := [] ;
     instance),



   (* ******************************************************************* *)
   (* specialize2                                                         *)
   (* type_scheme -> type_simple list -> (type_simple * type_simple list) *)
   (* {b Descr} : Performs the same job than [specialize] on the type
                scheme. Also perform a copy of the types [tys], using the
                same mapping of generalized variables fto fresh variables
                to perform the copy.
                This means that if a variable appears in both the scheme
                and the types, then it will be replaced by the same fresh
                variable.
                Obviously, this in only interesting when the scheme and
                the types share some generalized variables !

      {b Rem} : Exported oustide this module.                             *)
   (* ******************************************************************* *)
   (fun scheme tys ->
     (* Copy the type scheme's body. *)
     let instance = copy_type_simple ~abstractize: None scheme.ts_body in
     (* Also copy the other types, using the same mapping provided in [seen]. *)
     let copied_tys = List.map (copy_type_simple ~abstractize: None) tys in
     (* Clean up seen type for further usages. *)
     seen := [] ;
     (instance, copied_tys))
  )
;;




(* ********************************************************************* *)
(* abstract_copy                                                         *)
(* (fname * collection_name) -> type_simple -> type_simple               *)
(** {b Descr} : Copies the [ty] type expression (hence breaking sharing
	      with the original one) and replaces occurrences of [Self]
	      by the given collection's [coll_name] carrier type.

    {b Rem} Exported outside this module.                                *)
(* ********************************************************************* *)
let abstract_copy coll_name =
  let seen = ref [] in
  (* Internal recursive copy same stuff than for [specialize] stuff. *)
  let rec rec_copy ty =
    let ty = repr ty in
    if List.mem_assq ty !seen then List.assq ty !seen
    else
      (begin
      let tmp_ty = {
	ts_level = ty.ts_level ;
	ts_desc = ST_var ;
	ts_link_value = TLV_unknown } in
      seen := (ty, tmp_ty) :: !seen ;
      let copied_desc =
	(match ty.ts_desc with
	 | ST_var -> ST_var
	 | ST_arrow (ty1, ty2) -> ST_arrow (rec_copy ty1, rec_copy ty2)
	 | ST_tuple tys -> ST_tuple (List.map rec_copy tys)
	 | ST_construct (name, args) ->
             ST_construct (name, List.map rec_copy args)
	 | ST_self_rep -> ST_species_rep coll_name
	 | (ST_species_rep _) as tdesc -> tdesc) in
      let copied_ty = {
	ts_level = current_binding_level () ;
	ts_desc = copied_desc ;
	ts_link_value = TLV_unknown } in
      tmp_ty.ts_link_value <- TLV_known copied_ty ;
      copied_ty
      end) in
  (* The function itself. *)
  (fun ty ->
    let copy = rec_copy ty in
    (* Clean up seen type for further usages. *)
    seen := [] ;
    copy)
;;



let (generalize, generalize2) =
  (* The number found generalizable variables. We count them by side effect. *)
  let nb_params = ref 0 in
  (* Internal recursive hunt for generalizable variables inside the type. *)
  let rec find_parameters ty =
    let ty = repr ty in
    if ty.ts_level > current_binding_level () &&
       ty.ts_level <> generic_level then
      (begin
      (* Make this generalized ! *)
      ty.ts_level <- generic_level ;
      match ty.ts_desc with
       | ST_var -> incr nb_params
       | ST_arrow (ty1, ty2) -> find_parameters ty1 ; find_parameters ty2
       | ST_tuple tys -> List.iter find_parameters tys
       | ST_construct (_, args) -> List.iter find_parameters args
       | ST_self_rep | ST_species_rep _ -> ()
      end) in



  ((* ********************************************************************** *)
   (* generalize                                                             *)
   (* type_simple -> type_scheme                                             *)
   (** {b Descr} : Generalize a type in order to create a type scheme having
                 the type's as body.
                 The scheme body physically shares the type's structure.

       {b Rem} : Exported oustide this module.                               *)
   (* ********************************************************************** *)
   (fun ty ->
     find_parameters ty ;
     let scheme = { ts_nb_vars = !nb_params ; ts_body = ty } in
     (* Clean up parameters counter for further usages. *)
     nb_params := 0 ;
     scheme),

   (* ********************************************************************* *)
   (* generalize2                                                           *)
   (* type_simple -> type_simple list -> (type_scheme * (type_simple list)) *)
   (** {b Descr} : Performs the same job than [generalize] on the first
                 type. Also perform generalize the types in place, but do
                 not explicitely return them as schemes. Note that the
                 argument counter is shared between these generalizations.

       {b Rem} : Exported oustide this module.                              *)
   (* ********************************************************************* *)
   (fun ty tys ->
     find_parameters ty ;
     List.iter find_parameters tys ;
     let scheme = { ts_nb_vars = !nb_params ; ts_body = ty } in
     (* Clean up parameters counter for further usages. *)
     nb_params := 0 ;
     (scheme, tys))
  )
;;



(** {b Rem} : Exported oustide this module. *)
let trivial_scheme ty = { ts_nb_vars = 0 ; ts_body = ty }
;;

(** {b Rem} : Non exported oustide this module. *)
let rec lowerize_levels max_level ty =
  let ty = repr ty in
  if ty.ts_level > max_level then
    begin
    ty.ts_level <- max_level ;
    match ty.ts_desc with
     | ST_var -> ()
     | ST_arrow (ty1, ty2) ->
	 lowerize_levels max_level ty1 ;
	 lowerize_levels max_level ty2
     | ST_tuple tys -> List.iter (lowerize_levels max_level) tys
     | ST_construct (_, args) -> List.iter (lowerize_levels max_level) args
     | ST_self_rep | ST_species_rep _ -> ()
    end
;;



(* ********************************************************************** *)
(* type_simple -> bool                                                    *)
(** {b Descr} : Check if a type is [Self]. This is only used to check
              if the type to what Self is equal is "Self". If so, this
	      means that by side effect, a successful unification changed
	      it hence, any unification attemp between a type and Self is
	      successfull again because it already succeeded.

    {b Rem} : Not exported outside this module.                           *)
(* ********************************************************************** *)
let is_self ty =
  let ty = repr ty in
  match ty.ts_desc with ST_self_rep -> true | _ -> false
;;




let unify ~loc ~self_manifest type1 type2 =
  let rec rec_unify ty1 ty2 =
    let ty1 = repr ty1 in
    let ty2 = repr ty2 in
    if ty1 == ty2 then () else
    match (ty1.ts_desc, ty2.ts_desc) with
     | (ST_var, _) ->
	 occur_check ~loc ty1 ty2 ;
	 lowerize_levels ty1.ts_level ty2 ;
	 ty1.ts_link_value <- TLV_known ty2
     | (_, ST_var) ->
	 occur_check ~loc ty2 ty1 ;
	 lowerize_levels ty2.ts_level ty1 ;
	 ty2.ts_link_value <- TLV_known ty1
     | ((ST_arrow (arg1, res1)), (ST_arrow (arg2, res2))) ->
	 lowerize_levels ty1.ts_level ty2 ;
	 ty1.ts_link_value <- TLV_known ty2 ;
	 rec_unify arg1 arg2 ;
	 rec_unify res1 res2
     | ((ST_tuple tys1), (ST_tuple tys2)) ->
	 lowerize_levels ty1.ts_level ty2 ;
	 ty1.ts_link_value <- TLV_known ty2 ;
	 (try List.iter2 rec_unify tys1 tys2 with
	 | Invalid_argument "List.iter2" ->
             (* In fact, that's an arity mismatch on the tuple. *)
             raise (Conflict (ty1, ty2, loc)))
     | (ST_construct (name, args), ST_construct (name', args')) ->
	 (if name <> name' then raise (Conflict (ty1, ty2, loc)) ;
	 lowerize_levels ty1.ts_level ty2 ;
	 ty1.ts_link_value <- TLV_known ty2 ;
	  try List.iter2 rec_unify args args' with
	  | Invalid_argument "List.iter2" ->
              (* In fact, that's an arity mismatch. *)
              raise
		(Arity_mismatch
		   (name, (List.length args), (List.length args'), loc)))
     | (ST_self_rep, ST_self_rep) ->
	 (begin
	 (* Trivial, but anyway, proceed as everywhere else. *)
	 lowerize_levels ty1.ts_level ty2 ;
	 ty1.ts_link_value <- TLV_known ty2
	 end)
     | (ST_self_rep, _) ->
	 (begin
	 match self_manifest with
	  | None -> raise (Conflict (ty1, ty2, loc))
	  | Some self_is_that ->
	      lowerize_levels ty1.ts_level ty2 ;
	      (* If the type to what Self is equal is "Self", this means   *)
	      (* that by side effect, a successful unification changed it  *)
	      (* hence, do not recurse endless, and accept the unification *)
	      (* again because it already succeeded.                       *)
	      if not (is_self self_is_that) then
		rec_unify self_is_that ty2 ;
	      (* Always return Self to keep abstraction ! *)
	      ty2.ts_link_value <- TLV_known ty1
	 end)
     | (_, ST_self_rep) ->
	 (begin
	 match self_manifest with
	  | None -> raise (Conflict (ty1, ty2, loc))
	  | Some self_is_that ->
	      lowerize_levels ty1.ts_level ty2 ;
	      (* If the type to what Self is equal is "Self", this means   *)
	      (* that by side effect, a successful unification changed it  *)
	      (* hence, do not recurse endless, and accept the unification *)
	      (* again because it already succeeded.                       *)
	      if not (is_self self_is_that) then
		rec_unify self_is_that ty1 ;
	      (* Always return Self to keep abstraction ! *)
	      ty1.ts_link_value <- TLV_known ty2
	 end)
     | ((ST_species_rep _), _) | (_, (ST_species_rep _)) -> failwith "todo9"
     | (_, _) -> raise (Conflict (ty1, ty2, loc)) in
  (* Now, let's work... *)
  rec_unify type1 type2
;;


(* species_name -> type_species *)
let type_species_interface ~species_module ~species_name =
  SPT_species_interface (species_module, species_name)
;;

(* (species_name * type_species) -> type_species -> type_species *)
let type_species_in species_name_n_type species_ty =
  SPT_parametrised_in (species_name_n_type, species_ty)
;;



(* ********************************************************************* *)
(* (collection_name * type_collection) -> type_species -> type_species   *)
(** {b Descr} : Build a "is-parameterized" species type.

    {b Args} :
      - coll_name_n_type : The couple (name of the parameter, collection
                         type of the parameter).
      - species_ty : Type of the parameterized species.

    {b Rem} : Exported outside this module.                              *)
(* ********************************************************************* *)
let type_species_is coll_name_n_type species_ty =
  SPT_parametrised_is (coll_name_n_type, species_ty)
;;


let __dirty_extract_coll_name = function
  | SPT_species_interface data -> data
  | SPT_parametrised_in (_, _) -> failwith "__dirty_extract_coll_name 0"
  | SPT_parametrised_is (_, _) -> failwith "__dirty_extract_coll_name 1"
;;



let ___dirty_chop_type_species = function
  | SPT_species_interface data -> failwith "___dirty_chop_type_specie 0"
  | SPT_parametrised_in ((_, _), ty)
  | SPT_parametrised_is ((_, _), ty) -> ty
;;



let subst_type_species (fname1, spe_name1) c2 ty_spe =
  let rec rec_subst = function
    | SPT_species_interface (fname, spe_name) ->
	if fname1 = fname && spe_name1 = spe_name then SPT_species_interface c2
	else SPT_species_interface (fname, spe_name)
    | SPT_parametrised_in ((n, ty1), ty2) ->
	let ty1' = rec_subst ty1 in
	let ty2' = rec_subst ty2 in
	SPT_parametrised_in ((n, ty1'), ty2')
    | SPT_parametrised_is ((n, (coll_fname, coll_name)), ty) ->
	let ty' = rec_subst ty in
	if fname1 = coll_fname && spe_name1 = coll_name then
	  SPT_parametrised_is ((n, c2), ty')
	else SPT_parametrised_is ((n, (coll_fname, coll_name)), ty') in
  rec_subst ty_spe
;;



(* (fname * collection_name) -> (fname * collection_name) -> type_simple -> *)
(*   type_simple                                                            *)
let subst_type_simple (fname1, spe_name1) c2 =
  let seen = ref [] in
  (* Internal recursive copy same stuff than for [specialize] stuff. *)
  let rec rec_copy ty =
    let ty = repr ty in
    if List.mem_assq ty !seen then List.assq ty !seen
    else
      (begin
      let tmp_ty = {
	ts_level = ty.ts_level ;
	ts_desc = ST_var ;
	ts_link_value = TLV_unknown } in
      seen := (ty, tmp_ty) :: !seen ;
      let copied_desc =
	(match ty.ts_desc with
	 | ST_var -> ST_var
	 | ST_arrow (ty1, ty2) -> ST_arrow (rec_copy ty1, rec_copy ty2)
	 | ST_tuple tys ->  ST_tuple (List.map rec_copy tys)
	 | ST_construct (name, args) ->
             ST_construct (name, List.map rec_copy args)
	 | ST_self_rep -> ST_self_rep
	 | ST_species_rep (fname, coll_name) ->
	     if fname = fname1 && coll_name = spe_name1 then ST_species_rep c2
	     else ty.ts_desc) in
      let copied_ty = {
	ts_level = current_binding_level () ;
	ts_desc = copied_desc ;
	ts_link_value = TLV_unknown } in
      tmp_ty.ts_link_value <- TLV_known copied_ty ;
      copied_ty
      end) in
  (* The function itself. *)
  (fun ty ->
    (* Copy the type scheme's body. *)
    let copy = rec_copy ty in
    (* Clean up seen type for further usages. *)
    seen := [] ;
    copy)
;;



(* ************************************************************** *)
(* Format.formatter -> type_species -> unit                       *)
(** {b Descr} : Pretty prints a species' type (not carrier type).

    {b Rem} : Exported outside this module.                       *)
(* ************************************************************** *)
let pp_type_species ppf species_type =
  let rec rec_print nb_printed_params local_ppf = function
    | SPT_species_interface (sp_module, sp_name) ->
	(begin
	if nb_printed_params > 0 then Format.fprintf local_ppf ") " ;
	Format.fprintf local_ppf "%s#%s" sp_module sp_name
	end)
    | SPT_parametrised_in ((param_name, param_ty), ty) ->
	(begin
	if nb_printed_params = 0 then Format.fprintf local_ppf "("
	else Format.fprintf local_ppf ", " ;
	Format.fprintf local_ppf "%s in %a%a"
	  param_name (rec_print 0) param_ty
	  (rec_print (nb_printed_params + 1)) ty
	end)
    | SPT_parametrised_is ((param_name, (param_module, param_ty)), ty) ->
	(begin
	if nb_printed_params = 0 then Format.fprintf local_ppf "("
	else Format.fprintf local_ppf ", " ;
	Format.fprintf local_ppf "%s in %s#%s%a"
	  param_name param_module param_ty
	  (rec_print (nb_printed_params + 1)) ty
	end) in
  rec_print 0 ppf species_type
;;

