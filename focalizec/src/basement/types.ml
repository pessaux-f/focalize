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

(* $Id: types.ml,v 1.34 2007-10-30 21:14:30 weis Exp $ *)


(* **************************************************************** *)
(** {b Descr} : File ("module") name (without the ".foc" extension).
 
    {b Rem} : Clearly exported outside this module.                  *)
(* **************************************************************** *)
type fname = string ;;



(* ************************************************ *)
(** {b Descr} : Collection / species name.

    {b Rem} : Clearly exported outside this module. *)
(* ************************************************ *)
type collection_name = string



(* ************************************************************************ *)
(** {b Descr} : Type constructot name. Records both the constructor's name
              and it's hosting module (file) name).

    {b Rem} : Abstract outside this module..                                *)
(* ************************************************************************ *)
type type_name = (fname * string) ;;



(** Label name. *)
type label_name = string ;;



(* ****************************************************************** *)
(** {b Descr} : Binding level considered as describing the level of a
                generalized type.

    {b Rem} : Not exported outside this module.                       *)
(* ****************************************************************** *)
let generic_level = 100000000 ;;



(* ************************************************* *)
(** {b Descr} : Describes the type algebra of Focal.

    {b Rem} : Exported opaque outside this module.   *)
(* ************************************************* *)
type type_simple =
  | ST_var of type_variable                   (** Type variable. *)
  | ST_arrow of (type_simple * type_simple)   (** Functionnal type. *)
  | ST_tuple of type_simple list              (** Tuple type. *)
  | ST_construct of
      (** Type constructor, possibly with arguments. Encompass the types
	  related to records and sums. Any value of these types are typed as
	  a [ST_construct] whose name is the name of the record (or sum)
	  type. *)
      (type_name * type_simple list)
  | ST_self_rep
    (** Carrier type of the currently analysed species. *)
  | ST_species_rep of (fname * collection_name)
    (** Carrier type of a collection hosted in the specified module. *)



(** Variable of type. Must be repr'ed. *)
and type_variable = {
  (** Binding level of the type. *)
  mutable tv_level : int ;
  (** Value of the type variable. *)
  mutable tv_value : type_variable_value
}


(** Value of a type link (generalization principle of type variable's value. *)
and type_variable_value =
  | TVV_unknown
  | TVV_known of type_simple
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
  (fname *           (** The "module" hosting the collection' code. *)
  collection_name)   (** The name of the collection as a string (not a
			 [Parsetree.vname] to prevent mutual depency between
		         the [types.ml] and [parsetree.ml] modules. And indeed,
		         a simple string is eally sufficient ! *)
;;



(* ************************************************************************ *)
(** {b Descr} : Type schemes, i.e. model of types.
              Scheme parameters are silent inside the scheme. In fact, they
              are types in the body with a binding level equals to
              [generic_level].

    {b Rem} : Exported opaque outside this module.                          *)
(* ************************************************************************ *)
type type_scheme = {
  ts_vars : type_variable list ;          (** Parameters in the scheme. *)
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
let rec repr = function
  | ST_var ({ tv_value = TVV_known ty1 } as var) ->
      let val_of_ty1 = repr ty1 in
      var.tv_value <- TVV_known val_of_ty1 ;
      val_of_ty1
  | ty -> ty
;;



let (begin_definition, end_definition, current_binding_level, type_variable) =
  let current_binding_level = ref 0 in
  ((* ****************************************************************** *)
   (* begin_definition : unit -> unit                                    *)
   (* {b Descr} : Must be called BEFORE every potentially generalizable
                definition. It increases the binding level to enable
                generalization once we go back to a lower level.

      {b Rem} : Exported outside this module.                            *) 
   (* ****************************************************************** *)
   (fun () -> incr current_binding_level),



   (* ****************************************************************** *)
   (* end_definition : unit -> unit                                    *)
   (* {b Descr} : Must be called AFTER every potentially generalizable
                definition. It decreases the binding level to prevent
                generalization of higher binding level type variables.

      {b Rem} : Exported outside this module.                            *) 
   (* ****************************************************************** *)
   (fun () -> decr current_binding_level),



   (* ***************************************************************** *)
   (* current_binding_level: unit -> int                                *)
   (* {b Descr} : Returns the current binding level, i.e. the level of
                variables than be generalized if they are of a level
                strictly greater than the current binding level.
      {b Rem} : Not exported outside this module.                       *)
   (* ***************************************************************** *)
   (fun () -> !current_binding_level),



   (* ******************************************************************* *)
   (* type_variable : unit -> type_simple                                 *)
   (* {b Descr} : Generates a type variable at the current binding level.

      {b Rem} : Exported outside this module.                             *)
   (* ******************************************************************* *)
   (fun () ->
     ST_var { tv_level = !current_binding_level ; tv_value = TVV_unknown }))
;;



(* ************************************************************************* *)
(* fname -> string -> type_name                                              *)
(** { b Descr } : Creates a type constructor whose basic name is
                [constructor_name] and hosting module is [hosting_module].
                For instance, "int" coming from the module "basics.foc"
                will be represented by [("basics", "int")].
                This allows to record in a type constructor both the
                constructor name and the hosting file where this constructor
                was defined.                                                 *)
(* ************************************************************************* *)
let make_type_constructor hosting_module constructor_name =
  (hosting_module, constructor_name)
;;



let type_basic type_name type_args = ST_construct (type_name, type_args) ;;

let type_int () = type_basic ("basics", "int") [] ;;

let type_float () = type_basic ("basics", "float") [] ;;

let type_bool () = type_basic ("basics", "bool") [] ;;

let type_string () = type_basic ("basics", "string") [] ;;

let type_char () = type_basic ("basics", "char") [] ;;

let type_unit () = type_basic ("basics", "unit") [] ;;

let type_arrow t1 t2 = ST_arrow (t1, t2) ;;

let type_prop () = type_basic ("basics", "prop") [] ;;

let type_tuple tys = ST_tuple tys ;;

let type_list t1 = type_basic ("basics", "list") [t1] ;;

(* Generate the carrier type of the currently analysed species.  *)
let type_self () = ST_self_rep ;;


let type_rep_species ~species_module ~species_name =
  ST_species_rep (species_module, species_name)
;;



let pp_type_name ppf (hosting_module, constructor_name) =
  Format.fprintf ppf "%s#%s" hosting_module constructor_name
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
    match ty with
    | ST_var ty_var ->
	let ty_variable_name =
	  get_or_make_type_variable_name
	    ty ~generalized_p: (ty_var.tv_level = generic_level) in
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
         | [] -> Format.fprintf ppf "%a" pp_type_name type_name
         | [one] ->
	     Format.fprintf ppf "%a@ %a" (rec_pp 3) one pp_type_name type_name
         | _ ->
             Format.fprintf ppf "@[<1>(%a)@]@ %a"
               (Handy.pp_generic_separated_list "," (rec_pp 0)) arg_tys
	       pp_type_name type_name
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
let occur_check ~loc var ty =
  let rec test t =
    let t = repr t in
    match t with
     | ST_var var' ->
	 if var == var' then raise (Circularity (t, ty, loc))
     | ST_arrow (ty1, ty2) -> test ty1 ; test ty2
     | ST_tuple tys -> List.iter test tys
     | ST_construct (_, args) -> List.iter test args
     | ST_self_rep | ST_species_rep _ -> () in
  test ty
;;



let (specialize, specialize_with_args) =
  let seen = ref [] in
  (* Internal recursive copy of a type scheme replacing its generalized
     variables by their associated new fresh type variables. *)
  let rec copy_type_simple ty =
    let ty = repr ty in
    match ty with
     | ST_var var ->
	 (* If the type is not generalized, then its copy is itself. *)
	 if var.tv_level <> generic_level then ty
	 else
	   (begin
	   (* If the variable was not yet seen, generate a fresh copy *)
	   (* and remind that the variable is now already seen.       *)
	   try List.assq var !seen
	   with Not_found ->
	     let fresh_var = type_variable () in
	     seen := (var, fresh_var) :: !seen ;
	     fresh_var
	   end)
     | ST_arrow (ty1, ty2) ->
         ST_arrow
	   (copy_type_simple ty1, copy_type_simple ty2)
     | ST_tuple tys ->  ST_tuple (List.map copy_type_simple tys)
     | ST_construct (name, args) ->
         ST_construct (name, List.map copy_type_simple args)
     | ST_self_rep -> ST_self_rep
     | ST_species_rep _ -> ty in



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
   (* specialize_with_args                                                *)
   (* type_scheme -> type_simple list -> type_simple                      *)
   (* {b Descr} : Performs the same job than [specialize] on the type
                scheme but directly instanciate the scheme's parameters
                by the types provided in the list.

      {b Rem} : Exported oustide this module.                             *)
   (* ******************************************************************* *)
   (fun scheme tys ->
     (* Initialize the variable mapping with the types to simulate the *)
     (* fact that these variables have already be seen and are bound   *)
     (* the types we want them to be instanciated with.                *)
     List.iter2 (fun var ty -> seen := (var, ty) :: !seen) scheme.ts_vars tys ;
     (* Copy the type scheme's body. *)
     let instance = copy_type_simple scheme.ts_body in
     (* Clean up seen type for further usages. *)
     seen := [] ;
     instance)
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
    match ty with
     | ST_var var ->
	 (begin
	 (* The abstraction must never change     *)
	 (* variables to prevent sharing breaks ! *)
	 try List.assq var !seen
	 with Not_found ->
	   seen := (var, ty) :: !seen ;
	   ty
	 end)
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
     | (ST_species_rep _) as tdesc -> tdesc in
  (* ******************** *)
  (* The function itself. *)
  (fun ty ->
    let copy = rec_copy ty in
    (* Clean up seen type for further usages. *)
    seen := [] ;
    copy)
;;


let (generalize, generalize2) =
  (* The list of found generalizable variables. *)
  (* We accumulate inside it by side effect.    *)
  let found_ty_parameters = ref ([] : type_variable list) in
  (* Internal recursive hunt for generalizable variables inside the type. *)
  let rec find_parameters ty =
    let ty = repr ty in
    match ty with
     | ST_var var ->
	 if var.tv_level > current_binding_level () &&
	    not (List.memq var !found_ty_parameters) then
	   begin
	   var.tv_level <- generic_level ;
	   found_ty_parameters := var :: !found_ty_parameters
	   end
     | ST_arrow (ty1, ty2) -> find_parameters ty1 ; find_parameters ty2
     | ST_tuple tys -> List.iter find_parameters tys
     | ST_construct (_, args) -> List.iter find_parameters args
     | ST_self_rep | ST_species_rep _ -> () in



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
     let scheme = { ts_vars = !found_ty_parameters ; ts_body = ty } in
     (* Clean up found parameters for further usages. *)
     found_ty_parameters := [] ;
     scheme),

   (* ********************************************************************* *)
   (* generalize2                                                           *)
   (* type_simple -> type_simple list -> (type_scheme * (type_simple list)) *)
   (** {b Descr} : Performs the same job than [generalize] on the first
                 type. Also perform generalization in place on the types in
                 [tys] , but do not explicitly return them as schemes.
                 Note that the argument counter is shared between these
                 generalizations.

       {b Rem} : Exported oustide this module.                              *)
   (* ********************************************************************* *)
   (fun ty tys ->
     find_parameters ty ;
     List.iter find_parameters tys ;
     let scheme = { ts_vars = !found_ty_parameters ; ts_body = ty } in
     (* Clean up found parameters for further usages. *)
     found_ty_parameters := [] ;
     (scheme, tys))
  )
;;



(** {b Rem} : Exported oustide this module. *)
let trivial_scheme ty = { ts_vars = [] ; ts_body = ty }
;;

(** {b Rem} : Non exported oustide this module. *)
let rec lowerize_levels max_level ty =
  let ty = repr ty in
  match ty with
   | ST_var var -> if var.tv_level > max_level then var.tv_level <- max_level ;
   | ST_arrow (ty1, ty2) ->
       lowerize_levels max_level ty1 ;
       lowerize_levels max_level ty2
   | ST_tuple tys -> List.iter (lowerize_levels max_level) tys
   | ST_construct (_, args) -> List.iter (lowerize_levels max_level) args
   | ST_self_rep | ST_species_rep _ -> ()
;;



(* ************************************************************************ *)
(** {b Descr} : Checks if a scheme contains type variables (generalized or
       not generalized).
       Such a check is required because species methods are not polymorphic
       (c.f. Virgile Prevosto's Phd section 3.3, page 24).

    {b Rem} : Exported oustide this module.                                 *)
(* ************************************************************************ *)
let scheme_contains_variable_p scheme =
  let rec rec_check ty =
    let ty = repr ty in
    match ty with
     | ST_var _ -> true
     | ST_arrow (ty1, ty2) ->
	 (rec_check ty1) || (rec_check ty2)
     | ST_tuple tys -> List.exists rec_check tys
     | ST_construct (_, args) -> List.exists rec_check args
     | ST_self_rep | ST_species_rep _ -> false in
  rec_check scheme.ts_body
;;



(* ************************************************************************ *)
(* type_simple -> type_simple                                               *)
(** {b Descr} : Extracts from a functionnal type the right-hand part of the
      arrow. This function assumes that the type IS an arrow and must be
      called only with an arrow type. It is designed to recover the part of
      a functional type that results from a unification. Hence, if the
      unification didn't provide a functional type, it will have failed,
      hence the current function will not be called. If the unification
      succeeded in creating a functionnal type, then the current function
      will be called and will really be provided a functionnal type and
      will not fail.
      This mechanism is needed because our unification principle is not
      only "in place". In fact the assignments performed by side effect
      implement correct usual unification but do not priviligiate Self to
      be returned. This last point is ensured by the returned type after
      a unification (and not by the physical equality ensured by the
      "in-place" modifications of the unification).

    {b Rem} : Exported outside this module.                                 *)
(* ************************************************************************ *)
let extract_fun_ty_result ty =
  let ty = repr ty in
  match ty with
   | ST_arrow (_, res) -> res
   | _ -> assert false
;;



(* ************************************************************************ *)
(* type_simple -> type_simple                                               *)
(** {b Descr} : Extracts from a functionnal type the left-hand part of the
      arrow. This function assumes that the type IS an arrow and must be
      called only with an arrow type. It is designed to recover the part of
      a functional type that results from a unification. Hence, if the
      unification didn't provide a functional type, it will have failed,
      hence the current function will not be called. If the unification
      succeeded in creating a functionnal type, then the current function
      will be called and will really be provided a functionnal type and
      will not fail.
      This mechanism is needed because our unification principle is not
      only "in place". In fact the assignments performed by side effect
      implement correct usual unification but do not priviligiate Self to
      be returned. This last point is ensured by the returned type after
      a unification (and not by the physical equality ensured by the
      "in-place" modifications of the unification).

    {b Rem} : Exported outside this module.                                 *)
(* ************************************************************************ *)
let extract_fun_ty_arg ty =
  let ty = repr ty in
  match ty with
   | ST_arrow (arg, _) -> arg
   | _ -> assert false
;;



let unify ~loc ~self_manifest type1 type2 =
  let rec rec_unify ty1 ty2 =
    let ty1 = repr ty1 in
    let ty2 = repr ty2 in
    if ty1 == ty2 then ty1 else
    match (ty1, ty2) with
     | (ST_var var, _) ->
	 occur_check ~loc var ty2 ;
	 lowerize_levels var.tv_level ty2 ;
	 var.tv_value <- TVV_known ty2 ;
	 ty2
     | (_, ST_var var) ->
	 occur_check ~loc var ty1 ;
	 lowerize_levels var.tv_level ty1 ;
	 var.tv_value <- TVV_known ty1 ;
	 ty1
     | ((ST_arrow (arg1, res1)), (ST_arrow (arg2, res2))) ->
	 let arg3 = rec_unify arg1 arg2 in
	 let res3 = rec_unify res1 res2 in
	 ST_arrow (arg3, res3)
     | ((ST_tuple tys1), (ST_tuple tys2)) ->
	 let tys3 =
	   (try List.map2 rec_unify tys1 tys2 with
	   | Invalid_argument "List.iter2" ->
               (* In fact, that's an arity mismatch on the tuple. *)
               raise (Conflict (ty1, ty2, loc))) in
	 ST_tuple tys3
     | (ST_construct (name, args), ST_construct (name', args')) ->
	 (if name <> name' then raise (Conflict (ty1, ty2, loc))) ;
	 let args'' =
	   (try List.map2 rec_unify args args' with
	   | Invalid_argument "List.iter2" ->
	       (* In fact, that's an arity mismatch. *)
	       raise
		 (Arity_mismatch
		    (name, (List.length args), (List.length args'), loc))) in
	 ST_construct (name, args'')
     | (ST_self_rep, ST_self_rep) ->
	 (begin
	 (* Trivial, but anyway, proceed as everywhere else. *)
	 ST_self_rep
	 end)
     | (ST_self_rep, _) ->
	 (begin
	 match self_manifest with
	  | None -> raise (Conflict (ty1, ty2, loc))
	  | Some self_is_that ->
	      ignore (rec_unify self_is_that ty2) ;
              (* Always prefer Self ! *)
              ST_self_rep
	 end)
     | (_, ST_self_rep) ->
	 (begin
	 match self_manifest with
	  | None -> raise (Conflict (ty1, ty2, loc))
	  | Some self_is_that ->
	      (* Same remarks than in the mirror case above. *)
	      ignore (rec_unify self_is_that ty1) ;
	      ST_self_rep
	 end)
     | ((ST_species_rep c1), (ST_species_rep c2)) ->
	 if c1 = c2 then ty1 else raise (Conflict (ty1, ty2, loc))
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
  (* Internal recursive copy same stuff than for [specialize] stuff *)
  (* except that the generalization possibility does't matter here. *)
  let rec rec_copy ty =
    let ty = repr ty in
    match ty with
     | ST_var var ->
	 (begin
	 try List.assq var !seen
	 with Not_found ->
	   let fresh_var = type_variable () in
	   (* Be careful, it's a copy, not a specialization ! Hence   *)
	   (* the original level of the type must be kept ! The fresh *)
	   (* variable is create at the [current_binding_level], then *)
           (* in case the original one was created at a lower generic *)
           (* level, we [lowerize_levels] taking the level max equal  *)
           (* to the level of the original variable.                  *)
           lowerize_levels var.tv_level fresh_var ;
	   seen := (var, fresh_var) :: !seen ;
	   fresh_var
	 end)
     | ST_arrow (ty1, ty2) -> ST_arrow (rec_copy ty1, rec_copy ty2)
     | ST_tuple tys ->  ST_tuple (List.map rec_copy tys)
     | ST_construct (name, args) ->
	 ST_construct (name, List.map rec_copy args)
     | ST_self_rep -> ST_self_rep
     | ST_species_rep (fname, coll_name) ->
	 if fname = fname1 && coll_name = spe_name1 then ST_species_rep c2
	 else ty in
  (* ******************** *)
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
      - [current_unit] : The string giving the name of the current
          compilation unit we are generating the OCaml code of. This is
          required to prevent, when printin types, to qualify type
          constructors with the OCaml module name if the type belongs
          to the currently compiled compilation unit. Hence this
          prevents things like in a "bar.foc" file containing
          [type t1 = ... ; type t2 = t1 * t2], getting in the generated
          OCaml file definitions like [type t1 = ... ;
	  type t2 = Bar.t1 * Bar.t1] which would lead to an OCaml module
          depending of itself.
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
  (* ((type_variable * string) list) ref                                   *)
  (** {b Descr} : The mapping giving for each variable already seen the
                name used to denote it while printing it.

      {b Rem} : Not exported. This mapping is purely local to the
              pretty-print function of type into the OCaml syntax. It is
              especially not shared with the type printing routine used to
              generate the FoCaL feedback and the Coq code.                *)
  (* ********************************************************************* *)
  let type_variable_names_mapping = ref ([] : (type_variable * string) list) in

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

  let get_or_make_type_variable_name var =
    (* No need to repr, [rec_pp] already did it. *)
    try List.assq var !type_variable_names_mapping with
    | Not_found ->
	let name =
	  String.make 1 (Char.chr (Char.code 'a' + !type_variables_counter)) in
	incr type_variables_counter ;
	type_variable_names_mapping :=
	  (var, name) :: !type_variable_names_mapping ;
	name in

  let pp_type_name_to_ml ~current_unit ppf (hosting_module, constructor_name) =
    if current_unit = hosting_module then
      Format.fprintf ppf "_focty_%s" constructor_name
    else
      Format.fprintf ppf "%s._focty_%s"
	(String.capitalize hosting_module) constructor_name in

  let rec rec_pp ~current_unit collections_carrier_mapping prio ppf ty =
    (* First of all get the "repr" guy ! *)
    let ty = repr ty in
    match ty with
    | ST_var var ->
	(* Read the justification in the current function's header about *)
        (* the fact that we amways consider variables as generalized.    *)
	let ty_variable_name = get_or_make_type_variable_name var in
	Format.fprintf ppf "'%s" ty_variable_name
    | ST_arrow (ty1, ty2) ->
	(* Arrow priority: 2. *)
	if prio >= 2 then Format.fprintf ppf "@[<1>(" ;
        Format.fprintf ppf "@[<2>%a@ ->@ %a@]"
	  (rec_pp ~current_unit collections_carrier_mapping 2) ty1
	  (rec_pp ~current_unit collections_carrier_mapping 1) ty2 ;
	if prio >= 2 then Format.fprintf ppf ")@]"
    | ST_tuple tys ->
	(* Tuple priority: 3. *)
	if prio >= 3 then Format.fprintf ppf "@[<1>(" ;
        Format.fprintf ppf "@[<2>%a@]"
          (Handy.pp_generic_separated_list " *"
	     (rec_pp ~current_unit collections_carrier_mapping 3)) tys ;
	if prio >= 3 then Format.fprintf ppf ")@]"
    | ST_construct (type_name, arg_tys) ->
        (begin
	(* Priority of arguments of a sum type constructor :       *)
        (* like tuples if only one argument : 3                    *)
        (* otherwise 0 if already a tuple because we force parens. *)
	match arg_tys with
         | [] ->
	     (* Just the special case for type "prop" that maps onto bool... *)
	     (* The problem is that we can't really "define" "prop" in the   *)
	     (* file "basic.foc" because "prop" is a keyword. Hence, we make *)
             (* directmy the shortcut between "prop" and the type "bool"     *)
	     (* defined in the "basic.foc" file .                            *)
	     if type_name = ("basics", "prop") then
	       Format.fprintf ppf "Basics._focty_bool"
	     else
	       Format.fprintf ppf "%a"
		 (pp_type_name_to_ml ~current_unit) type_name
         | [one] ->
	     Format.fprintf ppf "%a@ %a"
	       (rec_pp ~current_unit collections_carrier_mapping 3) one
	       (pp_type_name_to_ml ~current_unit) type_name
         | _ ->
             Format.fprintf ppf "@[<1>(%a)@]@ %a"
               (Handy.pp_generic_separated_list ","
		  (rec_pp ~current_unit collections_carrier_mapping 0)) arg_tys
	       (pp_type_name_to_ml ~current_unit) type_name
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
	  (* If the carrier is not in the mapping created for the species *)
	  (* parameters, that's because the searched species carrier's is *)
          (* not a species parameter, i.e. it's a toplevel species.       *)
	  (* And as always, the type's name representing a species's      *)
	  (* carrier is "me_as_carrier".                                  *)
	  if current_unit = module_name then
	    Format.fprintf ppf "%s.me_as_carrier" collection_name
	  else
	    Format.fprintf ppf "%s.%s.me_as_carrier"
	      (String.capitalize module_name) collection_name
        end) in

  (* ************************************************* *)
  (* Now, the real definition of the printing function *)
  (
   (fun ~current_unit ~reuse_mapping collections_carrier_mapping ppf
       whole_type ->
     (* Only reset the variable mapping if we were not told the opposite. *)
     if not reuse_mapping then reset_type_variables_mapping () ;
     rec_pp ~current_unit collections_carrier_mapping 0 ppf whole_type),

   (fun () -> reset_type_variables_mapping ()))
;;
