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

(* $Id: types.ml,v 1.27 2007-09-28 08:40:10 pessaux Exp $ *)

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
  (** Binding level of the type. *)
  mutable ts_level : int ;
  (* The description of the type. *)
  ts_desc : type_simple_desc ;
  (** Value of the type. In fact, generalisation of the value of a type variable. This permits to make an already "non-variable" type equal to Self by side effect. *)
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
;;



(* ************************************************************************ *)
(** {b Descr} : Interface of a collection. It could be the list of its
         method'n'types, i.e. (string * type_simple) list but we don't want
	a structural unification. That's not because 2 collections have the
	same signature that they have the same semantics.
	Instead, one will get the type of the collection via an environment
	using the [collection_name] and the [fname] as key.

    {b Rem} : Exported outside this module.                                 *)
(* ************************************************************************ *)
type type_collection =
  (fname *               (** The "module" hosting the collection' code. *)
  collection_name)       (** The name of the collection. *)
;;



(* ************************************************************************ *)
(** {b Descr} : Type schemes, i.e. model of types.
              Scheme parameters are silent inside the scheme. In fact, they
              are types in the body with a binding level equals to
              [generic_level].

    {b Rem} : Exported opaque outside this module.                          *)
(* ************************************************************************ *)
type type_scheme = {
  ts_nb_vars : int ;       (** Number of parameters in the scheme. *)
  ts_body : type_simple    (** Body of the scheme where generalized types
			       have a level equal to [generic_level]. *)
} ;;



(* *********************************************************************** *)
(** {b Descr} : Exception meaning that the 2 arguments types cannot be
              unified. The location related to the point where unification
              occured is provided for error reporting purposes.            *)
(* *********************************************************************** *)
exception Conflict of (type_simple * type_simple * Location.t) ;;



(* ****************************************************************** *)
(** {b Descr} : Exception meaning that a circularity would occur if
             the unification of these 2 types was performed.
             In other words, the first type occurs inside the second. *)
(* ****************************************************************** *)
exception Circularity of (type_simple * type_simple * Location.t) ;;



(* ********************************************************************** *)
(** {b Descr} : A functional type constructor has been used with the wrong
            number of arguments. The exception carries on the name of the
            type and the conflicting arities.                             *)
(* ********************************************************************** *)
exception Arity_mismatch of (type_name * int * int  * Location.t) ;;



(* ******************************************************************** *)
(* type_simple -> type_simple                                           *)
(** {b Descr} : Returns the canonical representation of a type.
              Compression is performed only one level each time. The
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



let (pp_type_simple, pp_type_scheme) =
  (* ********************************************************************* *)
  (* ((type_simple * string) list) ref                                     *)
  (** {b Descr} : The mapping giving for each variable already seen the
                name used to denote it while printing it.

      {b Rem} : Not exported. This mapping is purely local to the
              pretty-print function of type into the FoCal syntax. It is
              especially not shared with the type printing routine used to
              generate the OCaml or Coq code.                              *)
  (* ********************************************************************* *)
  let type_variable_names_mapping = ref ([] : (type_simple * string) list) in

  (* ********************************************************************* *)
  (* int ref                                                               *)
  (** {b Descr} : The counter counting the number of different variables
                already seen hence printed. It serves to generate a fresh
                name to new variables to print.

      {b Rem} : Not exported. This counter is purely local to the
              pretty-print function of type into the FoCal syntax. It is
              especially not shared with the type printing routine used to
              generate the OCaml or Coq code.                              *)
  (* ********************************************************************* *)
  let type_variables_counter = ref 0 in

  (* ******************************************************************** *)
  (* unit -> unit                                                         *)
  (** {b Descr} : Resets the variables names mapping an counter. This
               allows to stop name-sharing between type prints.

     {b Rem} : Not exported. This counter is purely local to the
              pretty-print function of type into the FoCal syntax. It is
              especially not shared with the type printing routine used to
              generate the OCaml or Coq code.                              *)
  (* ********************************************************************* *)
  let reset_type_variables_mapping () =
    type_variable_names_mapping := [] ;
    type_variables_counter := 0 in

  let get_or_make_type_variable_name ty ~generalized_p =
    (* No need to repr, [rec_pp] already did it. *)
    try List.assq ty !type_variable_names_mapping with
    | Not_found ->
	let name =
	  String.make 1 (Char.chr (Char.code 'a' + !type_variables_counter)) in
	incr type_variables_counter ;
	let name' = if not generalized_p then "_" ^ name else name in
	type_variable_names_mapping :=
	  (ty, name') :: !type_variable_names_mapping ;
	name' in

  let rec rec_pp prio ppf ty =
    (* First of all get the "repr" guy ! *)
    let ty = repr ty in
    match ty.ts_desc with
    | ST_var ->
	let ty_variable_name =
	  get_or_make_type_variable_name
	    ty ~generalized_p: (ty.ts_level = generic_level) in
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
  let rec copy_type_simple ty =
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
	  ts_level = min ty.ts_level (current_binding_level ()) ;
	  ts_desc = ST_var ;
	  ts_link_value = TLV_unknown } in
	seen := (ty, tmp_ty) :: !seen ;
	(* Build the type description copy of ourself. *)
	let copied_desc =
	  (match ty.ts_desc with
	   | ST_var -> ST_var
	   | ST_arrow (ty1, ty2) ->
               ST_arrow
		 (copy_type_simple ty1, copy_type_simple ty2)
	   | ST_tuple tys ->  ST_tuple (List.map copy_type_simple tys)
	   | ST_construct (name, args) ->
               ST_construct (name, List.map copy_type_simple args)
	   | ST_self_rep -> ST_self_rep
	   | (ST_species_rep _) as tdesc -> tdesc) in
	(* Build the type expression copy of ourself. *)
	let copied_ty = {
	  ts_level = min ty.ts_level (current_binding_level ()) ;
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
     let instance = copy_type_simple scheme.ts_body in
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
     let instance = copy_type_simple scheme.ts_body in
     (* Also copy the other types, using the same mapping provided in [seen]. *)
     let copied_tys = List.map copy_type_simple tys in
     (* Clean up seen type for further usages. *)
     seen := [] ;
     (instance, copied_tys))
  )
;;



(* ********************************************************************* *)
(* and_abstract: (fname * collection_name) option -> type_simple ->      *)
(*   type_simple                                                         *)
(** {b Descr} : Copies the [ty] type expression (hence breaking sharing
	      with the original one except for variables : these one are
              NOT "freshly copied" but remain shared between the copied
              and original types) and replaces occurrences of [Self] by
              the given collection's [~and_abstract] collection's
              carrier type if provided (i.e. different from [None]).

    {b Rem} Exported outside this module.                                *)
(* ********************************************************************* *)
let copy_type_simple_but_variables ~and_abstract =
  let seen = ref [] in
  (* Internal recursive copy same stuff than for [specialize] stuff. *)
  let rec rec_copy ty =
    let ty = repr ty in
    if List.mem_assq ty !seen then List.assq ty !seen
    else
      (begin
      match ty.ts_desc with
       | ST_var ->
	   (* The abstraction must never change     *)
	   (* variables to prevent sharing breaks ! *)
	   seen := (ty, ty) :: !seen ;
	   ty
       | _ ->
	   (begin
	   let tmp_ty = {
	     ts_level = min ty.ts_level (current_binding_level ()) ;
	     ts_desc = ST_var ;
	     ts_link_value = TLV_unknown } in
	   seen := (ty, tmp_ty) :: !seen ;
	   let copied_desc =
	     (match ty.ts_desc with
	      | ST_var -> assert false      (* Caught above. *)
	      | ST_arrow (ty1, ty2) -> ST_arrow (rec_copy ty1, rec_copy ty2)
	      | ST_tuple tys -> ST_tuple (List.map rec_copy tys)
	      | ST_construct (name, args) ->
		  ST_construct (name, List.map rec_copy args)
	      | ST_self_rep ->
		  (begin
		  match and_abstract with
		   | Some coll_name -> ST_species_rep coll_name
		   | None -> ST_self_rep
		  end)
	      | (ST_species_rep _) as tdesc -> tdesc) in
	   let copied_ty = {
	     (* Be careful, it's a copy, not a specialization ! Hence *)
	     (* the original level of the type must be kept !         *)
	     ts_level = min ty.ts_level (current_binding_level ()) ;
	     ts_desc = copied_desc ;
	     ts_link_value = TLV_unknown } in
	   tmp_ty.ts_link_value <- TLV_known copied_ty ;
	   copied_ty
	   end)
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



(* ************************************************************************** *)
(** {b Descr} : Transforms the argument type [ty] into a type scheme that
              will never anymore be generalizable. To do this, the type's
              levels are inductively set to 0.
              Such schemes are even more restricted that "trivial type
              schemes" because there will never exist a binding level where
              they will be generalizable.
              Such schemes are required because species methods are not
              polymorphic (c.f. Virgile Prevosto's Phd section 3.3, page 24).
              A "trivial type scheme" means only a scheme tht won't be
              generalizable at the current and higher binding levels.
              In case of species methods, the binding level at the method
              definition point can also appears while typechecking another
              method. In this case, the identifier that was considered as
              monomorphic now appears as polymorphic.
              Hence, setting a 0 level in the scheme body, this
              inconsistence cannot appear anymore.

    {b Rem} : Exported oustide this module.                                   *)
(* ************************************************************************** *)
let never_generalizable_scheme ty =
  let rec rec_deep_lowerize ty =
    let ty = repr ty in
    begin
    ty.ts_level <- 0 ;
    match ty.ts_desc with
     | ST_var -> ()
     | ST_arrow (ty1, ty2) ->
	 rec_deep_lowerize ty1 ;
	 rec_deep_lowerize ty2
     | ST_tuple tys -> List.iter rec_deep_lowerize tys
     | ST_construct (_, args) -> List.iter rec_deep_lowerize args
     | ST_self_rep | ST_species_rep _ -> ()
    end in
    rec_deep_lowerize ty ;
  { ts_nb_vars = 0 ; ts_body = ty }
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
	      (* First, make a fully separated copy of the type "Sefl" has    *)
	      (* to prevent any pollution of its structure during unification *)
	      (* (especially, making it [TLV_known ST_self_rep] which would ! *)
              (* enable to unify anything).                                   *)
	      let self_is_that_copied =
		copy_type_simple_but_variables
		  ~and_abstract: None self_is_that in
	      lowerize_levels ty1.ts_level ty2 ;
	      (* If the type to what Self is equal is "Self", this means   *)
	      (* that by side effect, a successful unification changed it  *)
	      (* hence, do not recurse endless, and accept the unification *)
	      (* again because it already succeeded.                       *)
	      if not (is_self self_is_that_copied) then
		rec_unify self_is_that_copied ty2 ;
	      (* Always return Self to keep abstraction ! *)
	      ty2.ts_link_value <- TLV_known ty1
	 end)
     | (_, ST_self_rep) ->
	 (begin
	 match self_manifest with
	  | None -> raise (Conflict (ty1, ty2, loc))
	  | Some self_is_that ->
	      (* Same remark than in the mirror case above. *)
	      let self_is_that_copied =
		copy_type_simple_but_variables
		  ~and_abstract: None self_is_that in
	      lowerize_levels ty1.ts_level ty2 ;
	      (* If the type to what Self is equal is "Self", this means   *)
	      (* that by side effect, a successful unification changed it  *)
	      (* hence, do not recurse endless, and accept the unification *)
	      (* again because it already succeeded.                       *)
	      if not (is_self self_is_that_copied) then
		rec_unify self_is_that_copied ty1 ;
	      (* Always return Self to keep abstraction ! *)
	      ty1.ts_link_value <- TLV_known ty2
	 end)
     | ((ST_species_rep c1), (ST_species_rep c2)) ->
	 (begin
	 if c1 = c2 then
	   (begin
	   lowerize_levels ty1.ts_level ty2 ;
	   ty1.ts_link_value <- TLV_known ty2
	   end)
	 else raise (Conflict (ty1, ty2, loc))
	 end)
     | (_, _) -> raise (Conflict (ty1, ty2, loc)) in
  (* ****************** *)
  (* Now, let's work... *)
  rec_unify type1 type2
;;



(* ************************************************************************* *)
(* type_collection -> type_collection -> type_simple -> type_simple          *)
(** {b Descr} : Performs the collection name substitution
              [(fname1, spe_name1)] <- [c2] inside a [type_simple].

    {b Args} :
      - [(fname1, spe_name1)] : The "collection type" to replace.
      - [c2] : The "collection type" to put everywhere [(fname1, spe_name1)]
             is found.
    {b Rem} : Exported outside this module.                                  *)
(* ************************************************************************* *)
let subst_type_simple (fname1, spe_name1) c2 =
  let seen = ref [] in
  (* Internal recursive copy same stuff than for [specialize] stuff. *)
  let rec rec_copy ty =
    let ty = repr ty in
    if List.mem_assq ty !seen then List.assq ty !seen
    else
      (begin
      let tmp_ty = {
	ts_level = min ty.ts_level (current_binding_level ()) ;
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
	(* Be careful, it's a copy, not a specialization ! Hence *)
	(* the original level of the type must be kept !         *)
	ts_level = min ty.ts_level (current_binding_level ()) ;
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



(* ***************************************************************** *)
(* Format.formatter -> type_collection_name -> unit                  *)
(** {b Descr} : Pretty prints a collection' type (not carrier type).

    {b Rem} : Exported outside this module.                          *)
(* ***************************************************************** *)
let pp_type_collection ppf (coll_module, coll_name) =
  Format.fprintf ppf "%s#%s" coll_module coll_name
;;



(* ************************************************************************* *)
(* reuse_mapping: bool -> (type_collection * string) list ->                 *)
(*   Format.formatter -> type_simple -> unit                                 *)

(** {b Descr} : "Compile", i.e. generate the OCaml source representation of
              a type. Basically, proceeds like the regular [pp_type_simple]
              except in 2 cases:
		- when encountering [Self] : in this case, generates the
		  type variable name representing [Self], i.e. by convention
		  "'me_as_carrier",
                - when encountering a species carrier type : in this case,
                  generate the type variable name representing this
                  species (recover it thanks to the mapping between
                  collections names and type variables names
                  [collections_carrier_mapping]).

              Be carreful : because we generate OCaml code, remind that in
              OCaml expressions, variables that are present in "source code
              types" do not involve any notion of "generalized" or "not
              generalized". Hence, if we want to write a type variable in
              an OCaml source code, we always write it as "'a" and never as
              "'_a" otherwise it is lexically incorrect. OCaml will do the
              job itself to check whether the variable is generalizable or
              not. FOR THIS REASON, when we print type variables here, we
              only consider that they are generalised, to get a printing
              without any underscore in the variable's name.

              To be able to print separately parts of a same type, hence
              keep sharing of variables names, this function has an extra
	      argument telling whether the local variables names mapping
              must be kept from previous printing calls.

    {b Args} :
      - [reuse_mapping] : Boolean telling if the print session must keep
          active the previous variables names mapping. If so, then sharing
          of variables names will be active between the previous prints and
          the current one. If no, then all the variables of the current type
          will be considered a new compared to those "seen" during previous
          calls to the printing function.
      - [collections_carrier_mapping] : Mapping giving for each collection
          in the scope of the printing session, which type variable name is
          used to represent in OCaml this collection carrier's type.
      - [ppf] : Out channel where to send the text of the printed type.
      - [whole_type] : Tye type expression to print.

    {b Rem} : Exported outside this module.                                  *)
(* ************************************************************************* *)
let (pp_type_simple_to_ml, purge_type_simple_to_ml_variable_mapping) =
  (* ********************************************************************* *)
  (* ((type_simple * string) list) ref                                     *)
  (** {b Descr} : The mapping giving for each variable already seen the
                name used to denote it while printing it.

      {b Rem} : Not exported. This mapping is purely local to the
              pretty-print function of type into the OCaml syntax. It is
              especially not shared with the type printing routine used to
              generate the FoCaL feedback and the Coq code.                *)
  (* ********************************************************************* *)
  let type_variable_names_mapping = ref ([] : (type_simple * string) list) in

  (* ********************************************************************* *)
  (* int ref                                                               *)
  (** {b Descr} : The counter counting the number of different variables
                already seen hence printed. It serves to generate a fresh
                name to new variables to print.

      {b Rem} : Not exported. This counter is purely local to the
              pretty-print function of type into the FoCal syntax. It is
              especially not shared with the type printing routine used to
              generate the FoCaL feedback and the Coq code.                *)
  (* ********************************************************************* *)
  let type_variables_counter = ref 0 in

  (* ******************************************************************** *)
  (* unit -> unit                                                         *)
  (** {b Descr} : Resets the variables names mapping an counter. This
               allows to stop name-sharing between type prints.

     {b Rem} : Exported outside this module.
             Hoever, this counter is purely local to the pretty-print
             function of type into the FoCal syntax. It is especially not
             shared with the type printing routine used to generate the
             FoCaL feedback and the Coq code.                             *)
  (* ******************************************************************** *)
  let reset_type_variables_mapping () =
    type_variable_names_mapping := [] ;
    type_variables_counter := 0 in

  let get_or_make_type_variable_name ty =
    (* No need to repr, [rec_pp] already did it. *)
    try List.assq ty !type_variable_names_mapping with
    | Not_found ->
	let name =
	  String.make 1 (Char.chr (Char.code 'a' + !type_variables_counter)) in
	incr type_variables_counter ;
	type_variable_names_mapping :=
	  (ty, name) :: !type_variable_names_mapping ;
	name in


  let rec rec_pp collections_carrier_mapping prio ppf ty =
    (* First of all get the "repr" guy ! *)
    let ty = repr ty in
    match ty.ts_desc with
    | ST_var ->
	(* Read the justification in the current function's header about *)
        (* the fact that we amways consider variables as generalized.    *)
	let ty_variable_name = get_or_make_type_variable_name ty in
	Format.fprintf ppf "'%s" ty_variable_name
    | ST_arrow (ty1, ty2) ->
	(* Arrow priority: 2. *)
	if prio >= 2 then Format.fprintf ppf "@[<1>(" ;
        Format.fprintf ppf "@[<2>%a@ ->@ %a@]"
	  (rec_pp collections_carrier_mapping 2) ty1
	  (rec_pp collections_carrier_mapping 1) ty2 ;
	if prio >= 2 then Format.fprintf ppf ")@]"
    | ST_tuple tys ->
	(* Tuple priority: 3. *)
	if prio >= 3 then Format.fprintf ppf "@[<1>(" ;
        Format.fprintf ppf "@[<2>%a@]"
          (Handy.pp_generic_separated_list " *"
	     (rec_pp collections_carrier_mapping 3)) tys ;
	if prio >= 3 then Format.fprintf ppf ")@]"
    | ST_construct (type_name, arg_tys) ->
        (begin
	(* Priority of arguments of a sum type constructor :       *)
        (* like tuples if only one argument : 3                    *)
        (* otherwise 0 if already a tuple because we force parens. *)
	match arg_tys with
         | [] -> Format.fprintf ppf "%s" type_name
         | [one] ->
	     Format.fprintf ppf "%a@ %s"
	       (rec_pp collections_carrier_mapping 3) one type_name
         | _ ->
             Format.fprintf ppf "@[<1>(%a)@]@ %s"
               (Handy.pp_generic_separated_list ","
		  (rec_pp collections_carrier_mapping 0)) arg_tys
	       type_name
	end)
    | ST_self_rep ->
	(* Here is the major difference with the regular [pp_type_simple]. *)
        (* We print the type variable that represents our carrier in the   *)
        (* OCaml translation.                                              *)
	Format.fprintf ppf "'me_as_carrier"
    | ST_species_rep (module_name, collection_name) ->
        (begin
        try
	  let coll_type_variable =
	    List.assoc
	      (module_name, collection_name) collections_carrier_mapping in
	  Format.fprintf ppf "%s" coll_type_variable
	with Not_found ->
	  (* Thank's to the all bunch of analyses performed, we should *)
	  (* never be not able to find the type variable representing  *)
	  (* the carrier type of a species in the OCaml translation.   *)
	  assert false
        end) in

  (* ************************************************* *)
  (* Now, the real definition of the printing function *)
  (
   (fun ~reuse_mapping collections_carrier_mapping ppf whole_type ->
     (* Only reset the variable mapping if we were not told the opposite. *)
     if not reuse_mapping then reset_type_variables_mapping () ;
     rec_pp collections_carrier_mapping 0 ppf whole_type),

   (fun () -> reset_type_variables_mapping ()))
;;
