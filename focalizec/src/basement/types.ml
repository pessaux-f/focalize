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

(* $Id: types.ml,v 1.3 2007-07-30 08:07:44 weis Exp $ *)

(** Types of various identifiers in the abstract syntax tree. *)
type collection_name = string
     (** Collection name. *) ;;
type species_name = string
     (** Species name. *) ;;
type type_name = string
     (** Type name. *) ;;
type label_name = string
     (** Label name. *) ;;

(* ************************************************* *)
(* type_simple                                       *)
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
  | ST_self_rep       (** Carrier type of the currently analysed species. *)
  | ST_species_rep of collection_name   (** Carrier type of a collection. *)

and type_species =
  | SPT_collection_interface of collection_type
  | SPT_species_interface of species_name
      (** Interface of a species:
	  It could be the list of its method'n'type'n'bodies, i.e.
	  ((string * type_simple * Parsetree.expr) list) but we don't want
	  a structural unification. That's not because 2 species have the
	  same signature that they have the same semantics.
	  Instead, one will get the type of the species via an environment
	  using the [species_name] as key. *)
  | SPT_parametrised_in of (species_name * collection_type)
  | SPT_parametrised_is of (collection_name * collection_type)

and collection_type = collection_name
    (** Interface of a collection:
	It could be the list of its method'n'types, i.e.
	(string * type_simple) list but we don't want
	a structural unification. That's not because 2 collections have the
	same signature that they have the same semantics.
	Instead, one will get the type of the collection via an environment
	using the [collection_name] as key. *)

(** Variable of type. Must be repr'ed. *)
and type_variable = {
  mutable tv_level : int;
  mutable tv_value : type_variable_value;
}

(** Value of a type variable. *)
and type_variable_value =
  | TVV_unknown
  | TVV_known of type_simple
;;

(* *********************************************** *)
(* type_scheme                                    *)
(** {b Descr} : Type schemes, i.e. type with parameters (as usual parameters
    are variables, hence here type variables), also types with unknowns,
    or model of types.

    {b Rem} : Exported opaque outside this module. *)
(* *********************************************** *)
type type_scheme = {
  ts_parameters : type_variable list;
  ts_body : type_simple;
}
;;

exception Conflict of type_simple * type_simple
  (* Those two types cannot be unified. *)
;;
exception Circularity of type_simple * type_simple
  (* There is a circularity detected: the first type occurs in the second. *)
;;
exception Arity_mismatch of type_name * int * int
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
let rec repr = function
  | ST_var ({ tv_value = TVV_known ty1 } as var) ->
      let val_of_ty1 = repr ty1 in
      var.tv_value <- TVV_known val_of_ty1 ;
      val_of_ty1
  | ty -> ty
;;

let (begin_definition, end_definition, current_binding_level, type_variable) =
  let current_binding_level = ref 0 in
  ((fun () -> incr current_binding_level),
   (fun () -> decr current_binding_level),
   (fun () -> !current_binding_level),
   (fun () ->
     ST_var { tv_level = !current_binding_level ; tv_value = TVV_unknown }))
;;

let type_basic type_name type_args = ST_construct (type_name, type_args)
;;

let type_int () = type_basic "int" []
;;

let type_float () = type_basic "float" []
;;

let type_bool () = type_basic "bool" []
;;

let type_string () = type_basic "string" []
;;

let type_char () = type_basic "char" []
;;

let type_unit () = type_basic "unit" []
;;

let type_arrow t1 t2 = ST_arrow (t1, t2)
;;

let type_tuple tys = ST_tuple tys
;;

(* Generate the carrier type of the currently analysed species.  *)
let type_self () = ST_self_rep
;;

let type_prop () = type_basic "prop" []
;;

(** {b Rem} : Non exported oustide this module. *)
let rec occur_check var ty =
  let rec test t =
    match repr t with
     | ST_var var' ->
         if var == var' then raise (Circularity (ST_var var, ty))
     | ST_arrow (ty1, ty2) -> test ty1 ; test ty2
     | ST_tuple tys -> List.iter test tys
     | ST_construct (_, args) -> List.iter test args
     | ST_self_rep | ST_species_rep _ -> () in
  test ty
;;

(** {b Rem} : Exported oustide this module. *)
let instanciate scheme parameters_mapping =

  (* Internal recursive copy of a type scheme replacing its generalized
     variables by their associated new fresh type variables. *)
  let rec copy_type_simple ty =
    match repr ty with
    | ST_var var as ty ->
      (* Check in the mapping if this variable was generalized,
         and if so replace it by its fresh correspondance. *)
        (try List.assq var parameters_mapping with Not_found -> ty)
    (* The rest is homomorphic copying. *)
    | ST_arrow (ty1, ty2) ->
        ST_arrow (copy_type_simple ty1, copy_type_simple ty2)
    | ST_tuple tys -> ST_tuple (List.map copy_type_simple tys)
    | ST_construct (name, args) ->
        ST_construct (name, List.map copy_type_simple args)
    | (ST_self_rep | ST_species_rep _) as ty -> ty in

  (* Now really copy the type scheme's body, replacing its type parameters
     on the fly. *)
  copy_type_simple scheme.ts_body
;;

let specialize scheme =
  (* Remember the mapping between type parameters (a.k.a. generalized type
     variables of a type scheme) and fresh type variables. *)
  (* Create, for each generalized variable of the
     type scheme, a fresh corresponding one. *)
  let parameters_mapping =
    List.map (fun var -> var, type_variable ()) scheme.ts_parameters in
  instanciate scheme parameters_mapping
;;

let instanciate_parameters scheme args =
  let parameters_mapping =
    let parameters = scheme.ts_parameters in
    try
      List.map2 (fun var ty -> var, ty) parameters args with
    | Invalid_argument "List.map2" ->
      (* This should not occur: check the arguments before calling
         this function! *)
      assert false in
  instanciate scheme parameters_mapping
;;

  

(** {b Rem} : Exported oustide this module. *)
let generalize ty =
  (* The list of found generalizable variables.
     We accumulate inside it by side effect. *)
  let found_ty_parameters = ref ([] : type_variable list) in
  (* Internal recursive hunt for generalizable variables inside the type. *)
  let rec find_parameters ty =
    match repr ty with
    | ST_var var ->
        if var.tv_level > (current_binding_level ()) &&
           not (List.memq var !found_ty_parameters) then
          found_ty_parameters := var :: !found_ty_parameters
    | ST_arrow (ty1, ty2) -> find_parameters ty1 ; find_parameters ty2
    | ST_tuple tys -> List.iter find_parameters tys
    | ST_construct (_, args) -> List.iter find_parameters args
    | ST_self_rep | ST_species_rep _ -> () in
  find_parameters ty;
  { ts_parameters = !found_ty_parameters; ts_body = ty; }
;;

(** {b Rem} : Exported oustide this module. *)
let closed_scheme ty = { ts_parameters = [] ; ts_body = ty }
;;

(** {b Rem} : Non exported oustide this module. *)
let rec lowerize_levels max_level ty =
  match repr ty with
  | ST_var var -> if var.tv_level > max_level then var.tv_level <- max_level
  | ST_arrow (ty1, ty2) ->
      lowerize_levels max_level ty1 ;
      lowerize_levels max_level ty2
  | ST_tuple tys -> List.iter (lowerize_levels max_level) tys
  | ST_construct (_, args) -> List.iter (lowerize_levels max_level) args
  | ST_self_rep | ST_species_rep _ -> ()
;;

let rec unify ~self_manifest ty1 ty2 =
  let val_of_ty1 = repr ty1 in
  let val_of_ty2 = repr ty2 in
  if val_of_ty1 == val_of_ty2 then () else
  match val_of_ty1, val_of_ty2 with
  | ST_var var, ty ->
      occur_check var ty ;
      lowerize_levels var.tv_level ty ;
      var.tv_value <- TVV_known ty
  | ty, ST_var var ->
      occur_check var ty ;
      lowerize_levels var.tv_level ty ;
      var.tv_value <- TVV_known ty
  | ST_arrow (arg1, res1), ST_arrow (arg2, res2) ->
      unify ~self_manifest arg1 arg2 ;
      unify ~self_manifest res1 res2
  | ST_tuple tys1, ST_tuple tys2 ->
     (try List.iter2 (unify ~self_manifest) tys1 tys2 with
      | Invalid_argument "List.iter2" ->
        (* In fact, that's an arity mismatch on the tuple. *)
        raise (Conflict (val_of_ty1, val_of_ty2)))
  | ST_construct (name, args), ST_construct (name', args') ->
      (if name <> name' then raise (Conflict (val_of_ty1, val_of_ty2)) ;
       try List.iter2 (unify ~self_manifest) args args' with
       | Invalid_argument "List.iter2" ->
         (* In fact, that's an arity mismatch. *)
         raise
          (Arity_mismatch (name, (List.length args), (List.length args'))))
  | (ST_self_rep, _) | (_, ST_self_rep) -> failwith "todo8"
  | ((ST_species_rep _), _) | (_, (ST_species_rep _)) -> failwith "todo9"
  | (_, _) -> raise (Conflict (val_of_ty1, val_of_ty2))
;;

let type_variables_mapping = ref ([] : (type_variable * string) list)
;;
let type_variables_counter = ref 0
;;

let reset_type_variables_mapping () =
  type_variables_mapping := [];
  type_variables_counter := 0;
;;

let get_type_variable_name var =
  try List.assq var !type_variables_mapping with
  | Not_found ->
    let name =
      String.make 1 (Char.chr (Char.code 'a' + !type_variables_counter)) in
    incr type_variables_counter;
    type_variables_mapping := (var, name) :: !type_variables_mapping;
    name
;;

(* Format.pp_formatter -> type_variable -> unit *)
let pp_type_variable ppf var =
  Format.fprintf ppf "'%s" (get_type_variable_name var)
;;

let (pp_type_simple, pp_type_scheme) =
  let rec rec_pp ppf ty =
    (* First of all get the "repr" guy ! *)
    let ty = repr ty in
    match ty with
    | ST_var v -> Format.fprintf ppf "%a" pp_type_variable v
    | ST_arrow (ty1, ty2) ->
        Format.fprintf ppf "(@[<2>%a@ ->@ %a@])" rec_pp ty1 rec_pp ty2
    | ST_tuple tys ->
        Format.fprintf ppf "(@[<2>%a@])"
          (Handy.pp_generic_separated_list " *" rec_pp) tys
    | ST_construct (type_name, arg_tys) ->
        (match arg_tys with
         | [] -> Format.fprintf ppf "%s" type_name
         | [one] -> Format.fprintf ppf "%a@ %s" rec_pp one type_name
         | _ ->
             Format.fprintf ppf "(@[<1>%a)@]@ %s"
               (Handy.pp_generic_separated_list " ," rec_pp) arg_tys type_name)
    | ST_self_rep -> Format.fprintf ppf "Self"
    | ST_species_rep collection_name ->
      Format.fprintf ppf "%s" collection_name in

  (fun ppf ty ->
    reset_type_variables_mapping ();
    rec_pp ppf ty),
  (fun ppf the_scheme ->
    reset_type_variables_mapping ();
    if the_scheme.ts_parameters <> [] then
      Format.fprintf ppf "forall %a.@ "
	(Handy.pp_generic_separated_list "," pp_type_variable)
	the_scheme.ts_parameters ;
    Format.fprintf ppf "%a" rec_pp the_scheme.ts_body)
;;
