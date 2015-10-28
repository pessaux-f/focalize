(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            Pierre Weis                                                     *)
(*            Damien Doligez                                                  *)
(*            Fran�ois Pessaux                                                *)
(*                                                                            *)
(*               LIP6  --  INRIA Rocquencourt -- ENSTA ParisTech              *)
(*                                                                            *)
(*  Copyright 2007 - ... LIP6 and INRIA                                       *)
(*            2012 - ... ENSTA ParisTech                                      *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)



(* ***************************************************************** *)
(** {b Descr} : File ("module") name (without the ".fcl" extension).

    {b Exported} : Yes.                                              *)
(* ***************************************************************** *)
type fname = string ;;



(* ***************************************************** *)
(** {b Descr} : Represents a collection or species name.

    {b Exported} : Yes.                                  *)
(* ***************************************************** *)
type collection_name = string ;;



(* *********************************************************************** *)
(** {b Descr} : Type constructor name. Records both the constructor's name
    and it's hosting module (file) name).

    {b Exported} : Abstract.                                               *)
(* *********************************************************************** *)
type type_name =
  (fname *      (** Compilation unit where the type constructor was defined. *)
   string)      (** Name of the constructor (for instance "int"). *)
;;



(* ****************************************************************** *)
(** {b Descr} : Binding level considered as describing the level of a
    generalized type variable.

    {b Exported} : No.                                                *)
(* ****************************************************************** *)
let generic_level = 100000000 ;;



(* **************************************************** *)
(** {b Descr} : Describes the type algebra of FoCaLize.
    Actually, this is a view of it. Internally type_simple
    and type_simple_view are the same type but only
    the definition of type_simple_view is (privately)
    exported.

    {b Exported} : Private.                             *)
(* **************************************************** *)
type type_simple_view =
  | ST_var of type_variable                             (** Type variable. *)
  | ST_arrow of (type_simple_view * type_simple_view)   (** Functional type. *)
  | ST_tuple of type_simple_view list                   (** Tuple type. *)
  | ST_sum_arguments of type_simple_view list
      (** Type of sum type value constructor's arguments. To prevent them from
          being confused with tuples. *)
  | ST_prop                                             (** The type of logical
                                                            formulae *)
  | ST_construct of
      (** Type constructor, possibly with arguments. Encompass the types
          related to records and sums. Any value of these types are typed as
          a [ST_construct] whose name is the name of the record (or sum)
          type. *)
      (type_name * type_simple_view list)
  | ST_self_rep     (** Carrier type of the currently analysed species. *)
  | ST_species_rep of
      (** Carrier type of a collection hosted in the specified module. *)
      (fname * collection_name)


(* *********************************************************** *)
(** {b Descr} : Non-canonical internal version of simple types.

    {b Exported} : Abstract.                                   *)
(* *********************************************************** *)
and type_simple = type_simple_view

(* ************************************************************************** *)
(** {b Descr} : Variable of type (type variable).
    Attention, they introduce the requirement for types to be repr'ed in
    order to get their canonical representation !

    {b Exported} : No.
 **************************************************************************** *)
and type_variable = {
  (** Binding level of the type. *)
  mutable tv_level : int ;
  (** Value of the type variable. *)
  mutable tv_value : type_variable_value
  (** Unique integer to trace identity of variables when debugging. *)
  (* DEBUG
  ; tv_debug : int *)
}



(* ******************************************************************* *)
(** {b Descr} : Value of a type link (generalization principle of type
    variable's value.

    {b Exported} : No.                                                 *)
(* ******************************************************************* *)
and type_variable_value =
  | TVV_unknown
  | TVV_known of type_simple
;;



(* ************************************************************************** *)
(** {b Descr} : Interface of a collection. It could be the list of its
    method'n'types, i.e. (string * type_simple) list but we don't want a
    structural unification. That's not because 2 collections have the same
    signature that they have the same semantics.
    Instead, one will get the type of the collection via an environment using
    the [collection_name] and the [fname] as key.

    {b Exported} : Yes.                                                       *)
(* ************************************************************************** *)
type type_collection =
  (fname *           (** The "module" hosting the collection' code. *)
  collection_name)   (** The name of the collection as a string (not a
                         [Parsetree.vname] to prevent mutual depency between
                         the [types.ml] and [parsetree.ml] modules. And indeed,
                         a simple string is eally sufficient ! *)
;;



(* ************************************************************************ *)
(** {b Descr} : Type schemes, i.e. model of types.
    Scheme parameters are silent inside the scheme. In fact, they are types
    in the body with a binding level equals to [generic_level].

    {b Exported} : Abstract.                                                *)
(* ************************************************************************ *)
type type_scheme = {
  ts_vars : type_variable list ;          (** Parameters in the scheme. *)
  ts_body : type_simple    (** Body of the scheme where generalized types
                               have a level equal to [generic_level]. *)
} ;;



(* ************************************************************************* *)
(** {b Descr} : Exception meaning that a sum type constructor was used with
    no arguments although it requires some. In this case, since constructors
    arguments are stored a a tuple, one of the type is a tuple with an
    empty list of types. Hence the error message reported  "Types and ...
    are not compatible" (c.f. bub report #180). We prefer a more specific
    error message.

    {b Exported} : Yes.                                                      *)
(* ************************************************************************* *)
exception Arity_mismatch_unexpected_args of Location.t ;;



(* ********************************************************************* *)
(** {b Descr} : Exception meaning that the 2 arguments types cannot be
    unified. The location related to the point where unification occured
    is provided for error reporting purposes.

    {b Exported} : Yes.                                                  *)
(* ********************************************************************* *)
exception Conflict of (type_simple * type_simple * Location.t) ;;



(* ******************************************************************** *)
(** {b Descr} : Exception meaning that a circularity would occur if the
    unification of these 2 types was performed.
    In other words, the first type occurs inside the second.

    {b Exported} : Yes.                                                 *)
(* ******************************************************************** *)
exception Circularity of (type_simple * type_simple * Location.t) ;;



(* *********************************************************************** *)
(** {b Descr} : A functional type constructor has been used with the wrong
    number of arguments. The exception carries on the name of the type and
    the conflicting arities.

    {b Exported} : Yes.                                                    *)
(* *********************************************************************** *)
exception Arity_mismatch of (type_name * int * int  * Location.t) ;;



(* ********************************************************************* *)
(* type_simple -> type_simple                                            *)
(** {b Descr} : Returns the canonical representation of a type.
    Compression is performed only one level each time. The day the next
    levels may be needed, this will be during an unification, and [repr]
    will be called if needed to get a deeper canonical representation of
    the type (i.e. the canonical representation of its subterms).

    {b Exported} : No                                                    *)
(* ********************************************************************* *)
let rec repr = function
  | ST_var ({ tv_value = TVV_known ty1; _ } as var) ->
      let val_of_ty1 = repr ty1 in
      var.tv_value <- TVV_known val_of_ty1 ;
      val_of_ty1
  | ty -> ty
;;

(* ********************************************************************* *)
(* type_simple -> type_simple_view                                       *)
(** {b Descr} : Recursively call `repr`.
    This is useful for printing since we have to call repr at each step
    when printing. Since the output of this function is canonical, we can
    use it as a view function.

    {b Exported} : Yes                                                   *)
(* ********************************************************************* *)
let rec view_type_simple ty =
  match repr ty with
    | ST_arrow (ty1, ty2) ->
       ST_arrow (view_type_simple ty1, view_type_simple ty2)
    | ST_sum_arguments tys ->
       ST_sum_arguments (List.map view_type_simple tys)
    | ST_tuple tys ->
       ST_tuple (List.map view_type_simple tys)
    | ST_construct (type_name, arg_tys) ->
       ST_construct (type_name, List.map view_type_simple arg_tys)
    | ST_var _
    | ST_prop
    | ST_self_rep
    | ST_species_rep _ as ty' -> ty'
;;



(* ************************************************************************* *)
(** {b Descr}: Generate a fresh integer used a unique identifier of a type
    variable. Designed for debug purpose only, to inspect broken sharing.    *)
(* ************************************************************************* *)
(* DEBUG
let gen_tyvar_debug =
 let cpt = ref 0 in
 fun () -> incr cpt ; ! cpt
;; *)



let (begin_definition, end_definition, current_binding_level, type_variable) =
  let current_binding_level = ref 0 in
  ((* ******************************************************************* *)
   (* begin_definition : unit -> unit                                     *)
   (* {b Descr} : Must be called BEFORE every potentially generalizable
      definition. It increases the binding level to enable generalization
      once we go back to a lower level.

      {b Exported} : Yes.                                                 *)
   (* ******************************************************************* *)
   (fun () -> incr current_binding_level),



   (* ******************************************************************** *)
   (* end_definition : unit -> unit                                        *)
   (* {b Descr} : Must be called AFTER every potentially generalizable
      definition. It decreases the binding level to prevent generalization
      of higher binding level type variables.

      {b Exported} : Yes.                                                  *)
   (* ******************************************************************** *)
   (fun () -> decr current_binding_level),



   (* **************************************************************** *)
   (* current_binding_level: unit -> int                               *)
   (* {b Descr} : Returns the current binding level, i.e. the level of
      variables than be generalized if they are of a level strictly
      greater than the current binding level.

      {b Exported} : No.                                               *)
   (* **************************************************************** *)
   (fun () -> !current_binding_level),



   (* ******************************************************************* *)
   (* type_variable : unit -> type_simple                                 *)
   (* {b Descr} : Generates a type variable at the current binding level.

      {b Exported} : Yes.                                                 *)
   (* ******************************************************************* *)
   (fun () ->
     ST_var { tv_level = !current_binding_level ; tv_value = TVV_unknown ;
              (* DEBUG
              tv_debug = gen_tyvar_debug () *)
             }))
;;



(* ************************************************************************* *)
(* fname -> string -> type_name                                              *)
(** {b Descr} : Creates a type constructor whose basic name is
    [constructor_name] and hosting module is [hosting_module].
    For instance, "int" coming from the module "basics.foc" will be
    represented by [("basics", "int")].
    This allows to record in a type constructor both the constructor name
    and the hosting file where this constructor was defined.

    {b Exported} : Yes.                                                      *)
(* ************************************************************************* *)
let make_type_constructor hosting_module constructor_name =
  (hosting_module, constructor_name)
;;



(* ************************************************************************** *)
(* {b Descr}: Dissecate a type constructor to provide separately its hosting
   module and the constructor's name. Obviously, this is identity !           *)
(* ************************************************************************** *)
let split_type_constructor type_name = (type_name : (fname * string)) ;;



let type_basic type_name type_args = ST_construct (type_name, type_args) ;;

let type_int () = type_basic ("basics", "int") [] ;;

let type_float () = type_basic ("basics", "float") [] ;;

let type_bool () = type_basic ("basics", "bool") [] ;;

let type_string () = type_basic ("basics", "string") [] ;;

let type_char () = type_basic ("basics", "char") [] ;;

let type_unit () = type_basic ("basics", "unit") [] ;;

let type_arrow t1 t2 = ST_arrow (t1, t2) ;;

let type_prop () = ST_prop ;;

let type_tuple tys = ST_tuple tys ;;

let type_sum_arguments tys = ST_sum_arguments tys ;;

let type_list t1 = type_basic ("basics", "list") [t1] ;;

let type_sum_arguments_from_type_tuple ty =
  let ty = repr ty in
  match ty with
   | ST_tuple tys -> ST_sum_arguments tys
   | _ -> assert false
;;



let is_generalized_type_variable ty_var = (ty_var.tv_level = generic_level);;


(* ************************************************************************* *)
(* unit -> type_simple                                                       *)
(* {b Descr} : Generates the carrier type of the currently analysed species.
   This builds a type we usually call "Self".

   {b Exported} : Yes.                                                       *)
(* ************************************************************************* *)
let type_self () = ST_self_rep ;;



(* ************************************************************************* *)
(* unit -> type_simple                                                       *)
(* {b Descr} : Generates the carrier type of the a particular species whose
   hosting compilation unit and name are provided as arguments.

   {b Exported} : Yes.                                                       *)
(* ************************************************************************* *)
let type_rep_species ~species_module ~species_name =
  ST_species_rep (species_module, species_name)
;;



(* ************************************************************************** *)
(** {b Descr}: Verifies if the type is "bool" or "Self". This is used in Coq and
    Dedukti generation to determine if an expression must be surrounded by a
    wrapper applying Coq's and Dedukti's "Is_true".
    See the comment in species_record_type_generation.ml in the function
    [rec_generate_logical_expr] to understand why the type Self must also be
    considered.

    {b Exported}: Yes.                                                        *)
(* ************************************************************************** *)
let is_bool_or_self_type ty =
  let ty = repr ty in
  match ty with
   | ST_construct (("basics", "bool"), []) -> true
   | ST_self_rep -> true
   | _ -> false
;;



(* ************************************************************************** *)
(** {b Descr}: Checks if the type is a construct type (i.e. a named type) and
    if so, returns its "type name" (i.e. module and type constructor names).
    Otherwise (not a named type): returns None.
    {b Rem}: This is used for pattern-matching checks to finally get the value
    constructors of a type if it has some.                                    *)
(* ************************************************************************** *)
let of_which_construct_type ty =
  match repr ty with
  | ST_construct (type_name, _) -> Some type_name
  | _ -> None
;;



let pp_type_name ppf (hosting_module, constructor_name) =
  Format.fprintf ppf "%s#%s" hosting_module constructor_name
;;



let (pp_type_simple, pp_type_scheme) =
  (* *********************************************************************** *)
  (* ((type_simple * string) list) ref                                       *)
  (** {b Descr} : The mapping giving for each variable already seen the
                name used to denote it while printing it.

      This mapping is purely local to the pretty-print function of type into
      the FoCaLize syntax. It is especially not shared with the type
      printing routine used to generate the OCaml, Coq or Dedukti code.
      {b Exported} : No.                                                     *)
  (* *********************************************************************** *)
  let type_variable_names_mapping = ref ([] : (type_variable * string) list) in

  (* ************************************************************************ *)
  (* int ref                                                                  *)
  (** {b Descr} : The counter counting the number of different variables
      already seen hence printed. It serves to generate a fresh name to
      new variables to print.

      This counter is purely local to the pretty-print function of type into
      the FoCaLize syntax. It is especially not shared with the type printing
      routine used to generate the OCaml, Coq or Dedukti code.

      {b Exported} : No.                                                      *)
  (* ************************************************************************ *)
  let type_variables_counter = ref 0 in

  (* ************************************************************* *)
  (* unit -> unit                                                  *)
  (** {b Descr} : Resets the variables names mapping an counter.
      This allows to stop name-sharing between type prints.

     {b Rem} : Not exported. This counter is purely local to the
      pretty-print function of type into the FoCaLize syntax. It is
      especially not shared with the type printing routine used to
      generate the OCaml, Coq or Dedukti code.                     *)
  (* ************************************************************* *)
  let reset_type_variables_mapping () =
    type_variable_names_mapping := [] ;
    type_variables_counter := 0 in

  let get_or_make_type_variable_name ty_var ~generalized_p =
    (* No need to repr, [rec_pp] already did it. *)
    try List.assq ty_var !type_variable_names_mapping with
    | Not_found ->
        let name = Handy.int_to_base_26 !type_variables_counter in
        incr type_variables_counter ;
        let name' = if not generalized_p then "_" ^ name else name in
        type_variable_names_mapping :=
          (ty_var, name') :: !type_variable_names_mapping ;
        name' in

  let rec rec_pp prio ppf ty =
    (* First of all get the "repr" guy ! *)
    let ty = repr ty in
    match ty with
    | ST_var ty_var ->
        let ty_variable_name =
          get_or_make_type_variable_name
            ty_var ~generalized_p: (ty_var.tv_level = generic_level) in
        Format.fprintf ppf "'%s" ty_variable_name ;
        (* DEBUG
        ; Format.fprintf ppf "(*%d,l:%d*)" ty_var.tv_debug ty_var.tv_level *)
    | ST_arrow (ty1, ty2) ->
        (* Arrow priority: 2. *)
        if prio >= 2 then Format.fprintf ppf "@[<1>(" ;
        Format.fprintf ppf "@[<2>%a@ ->@ %a@]" (rec_pp 2) ty1 (rec_pp 1) ty2 ;
        if prio >= 2 then Format.fprintf ppf ")@]"
    | ST_sum_arguments tys ->  (* Printed like tuple. *)
        (* Like tuple priority: 3. *)
        if prio >= 3 then Format.fprintf ppf "@[<1>(" ;
        Format.fprintf ppf "@[<2>%a@]"
          (Handy.pp_generic_separated_list ", " (rec_pp 3)) tys ;
        if prio >= 3 then Format.fprintf ppf ")@]"
    | ST_tuple tys ->
        (* Tuple priority: 3. *)
        if prio >= 3 then Format.fprintf ppf "@[<1>(" ;
        Format.fprintf ppf "@[<2>%a@]"
          (Handy.pp_generic_separated_list " *" (rec_pp 3)) tys ;
        if prio >= 3 then Format.fprintf ppf ")@]"
    | ST_construct (type_name, arg_tys) ->
        (begin
        (* Priority of arguments of a sum type constructor :
           like tuples if only one argument : 3
           otherwise 0 if already a tuple because we force parens. *)
        match arg_tys with
         | [] -> Format.fprintf ppf "%a" pp_type_name type_name
         | _ ->
             Format.fprintf ppf "%a@ @[<1>(%a)@]"
               pp_type_name type_name
               (Handy.pp_generic_separated_list "," (rec_pp 0)) arg_tys
        end)
    | ST_prop -> Format.fprintf ppf "prop"
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
           (* If the variable was not yet seen, generate a fresh copy and
              remind that the variable is now already seen. *)
           try List.assq var !seen
           with Not_found ->
             let fresh_var = type_variable () in
             seen := (var, fresh_var) :: !seen ;
             fresh_var
           end)
     | ST_arrow (ty1, ty2) ->
         ST_arrow
           (copy_type_simple ty1, copy_type_simple ty2)
     | ST_sum_arguments tys ->
         ST_sum_arguments (List.map copy_type_simple tys)
     | ST_tuple tys ->  ST_tuple (List.map copy_type_simple tys)
     | ST_construct (name, args) ->
         ST_construct (name, List.map copy_type_simple args)
     | ST_self_rep -> ST_self_rep
     | ST_prop
     | ST_species_rep _ -> ty in



  ((* ********************************************************************* *)
   (* specialize                                                            *)
   (* type_scheme -> type_simple                                            *)
   (* {b Descr} : Instanciates a type scheme. This means that is makes a
      copy of its body, replacing generalized variables by fresh variables.
      If a generalized variable appears several times it will be replace
      by the same fresh variable at these different locations.

      {b Rem} : Exported oustide this module.                               *)
   (* ********************************************************************* *)
   (fun scheme ->
     (* Copy the type scheme's body. *)
     let instance = copy_type_simple scheme.ts_body in
     (* Clean up seen type for further usages. *)
     seen := [] ;
     instance),



   (* *************************************************************** *)
   (* specialize_with_args                                            *)
   (* type_scheme -> type_simple list -> type_simple                  *)
   (* {b Descr} : Performs the same job than [specialize] on the type
      scheme but directly instanciate the scheme's parameters by the
      types provided in the list.

      {b Rem} : Exported oustide this module.                         *)
   (* *************************************************************** *)
   (fun scheme tys ->
     (* Initialize the variable mapping with the types to simulate the fact
        that these variables have already be seen and are bound the types we
        want them to be instanciated with. *)
     List.iter2 (fun var ty -> seen := (var, ty) :: !seen) scheme.ts_vars tys ;
     (* Copy the type scheme's body. *)
     let instance = copy_type_simple scheme.ts_body in
     (* Clean up seen type for further usages. *)
     seen := [] ;
     instance)
  )
;;



(* ******************************************************************** *)
(* and_abstract: (fname * collection_name) option -> type_simple ->     *)
(*   type_simple                                                        *)
(** {b Descr} : Copies the [ty] type expression (hence breaking sharing
    with the original one except for variables : these one are NOT
    "freshly copied" but remain shared between the copied and original
    types) and replaces occurrences of [Self] by the given collection's
    [~and_abstract] collection's carrier type if provided
    (i.e. different from [None]).

    {b Rem} Exported outside this module.                               *)
(* ******************************************************************** *)
let copy_type_simple_but_variables ~and_abstract =
  let seen = ref [] in
  (* Internal recursive copy same stuff than for [specialize] stuff. *)
  let rec rec_copy ty =
    let ty = repr ty in
    match ty with
     | ST_var var ->
         (begin
         (* The abstraction must never change variables to prevent sharing
            breaks ! *)
         try List.assq var !seen
         with Not_found ->
           seen := (var, ty) :: !seen ;
           ty
         end)
     | ST_arrow (ty1, ty2) -> ST_arrow (rec_copy ty1, rec_copy ty2)
     | ST_sum_arguments tys -> ST_sum_arguments (List.map rec_copy tys)
     | ST_tuple tys -> ST_tuple (List.map rec_copy tys)
     | ST_construct (name, args) ->
         ST_construct (name, List.map rec_copy args)
     | ST_self_rep ->
         (begin
         match and_abstract with
          | Some coll_name -> ST_species_rep coll_name
          | None -> ST_self_rep
         end)
     | ST_prop
     | (ST_species_rep _) as tdesc -> tdesc in
  (* ******************** *)
  (* The function itself. *)
  (fun ty ->
    let copy = rec_copy ty in
    (* Clean up seen type for further usages. *)
    seen := [] ;
    copy)
;;



(* ************************************************************************ *)
(* type_collection -> type_scheme -> type_scheme                            *)
(** {b Descr} : Like [copy_type_simple_but_variables] but operate on the
    [type_simple] making the scheme's body. This function is needed for
    the collection substitution because we make it also operating on the
    [Parsetree.ast_type] field of the AST.

    {b Rem} : Because the compiler doesn't use the [Parsetree.ast_type] field
    of the AST, the result of this function will never be used internally
    by the compiler.
    Exported outside this module.                                           *)
(* ************************************************************************ *)
let abstract_in_scheme coll_to_abstract sch =
  let body' =
    copy_type_simple_but_variables
      ~and_abstract: (Some coll_to_abstract) sch.ts_body in
  { sch with ts_body = body' }
;;



(* *************************************************************** *)
(* type_simple -> type_scheme                                      *)
(** {b Descr} : Generalize a type in order to create a type scheme
    having the type's as body. The scheme body physically shares
    the type's structure.

    {b Rem} : Exported oustide this module.                        *)
(* *************************************************************** *)
let generalize =
  (* The list of found generalizable variables. We accumulate inside it by
     side effect. *)
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
     | ST_sum_arguments tys -> List.iter find_parameters tys
     | ST_tuple tys -> List.iter find_parameters tys
     | ST_construct (_, args) -> List.iter find_parameters args
     | ST_prop | ST_self_rep | ST_species_rep _ -> () in
  (* Let's do the job now. *)
  (fun ty ->
    find_parameters ty ;
    (* Attention to reverse the list of the variables found otherwise they
       will be in the wrong order compared to the (_ : Set)'s generated for
       Coq. This was a long lasting bug that has never been found !
       In effect, in a type scheme all 'a, 'b, 'c, 'a -> 'b -> 'c
       correscponding to a function fun x y z -> ... we find the variables
       in the order 'a, then 'b, then 'c. Since we add them in head of the
       accumulation list, they are in reverse order. Hence, when binding
       variable to their type (using bind_parameters_to_types_from_type_scheme
       from MiscHelpers), not reversing this list would bind the variables to
       their type variable in reverse order. *)
    let scheme = { ts_vars = List.rev !found_ty_parameters ; ts_body = ty } in
    (* Clean up found parameters for further usages. *)
    found_ty_parameters := [] ;
    scheme)
;;



let build_type_def_scheme ~variables ~body =
  (* Mark the variables as generalized. *)
  let vars =
    List.map
      (fun ty ->
        let ty = repr ty in
        match ty with
         | ST_var var -> var.tv_level <- generic_level ; var
         | _ -> assert false)
      variables in
  (* And make a scheme with the type [ty] as body and the variables [var]
     as parameter, KEEPING their order ! In effect, the position of the
     variables are important in a type definition:
        "type ('a, 'b) = ('a * 'b)" is different of type
        "type ('b * 'a) = ('a * 'b)". *)
  { ts_vars = vars ; ts_body = body }
;;



(* ************************************************************************ *)
(* type_simple -> type_scheme                                               *)
(** {b Descr} : Creates a type scheme whose body is the type passed as
    argument in which we do not perform any generalisation. Hence, the type
    scheme is trivially non-polymorphic.
    This is very useful to insert the scheme of a recursive function in the
    typing environment used to type-check the body of this function.

   {b Exported} : Yes.                                                      *)
(* ************************************************************************ *)
let trivial_scheme ty = { ts_vars = [] ; ts_body = ty } ;;



(* ************************************************************************** *)
(** {b Descr} : Splits a type scheme in two parts:
    - the list of generalized variables
    - the body
    WITHOUT doing any specialization ! In other words, what we get with this
    function, is not an instance of the scheme, but simply the scheme itself
    dissecated in its 2 components.
   {b Exported} : Yes.                                                        *)
(* ************************************************************************** *)
let scheme_split sch = sch.ts_vars, sch.ts_body ;;



(** {b Rem} : Non exported oustide this module. *)
let rec lowerize_levels max_level ty =
  let ty = repr ty in
  match ty with
   | ST_var var -> if var.tv_level > max_level then var.tv_level <- max_level ;
   | ST_arrow (ty1, ty2) ->
       lowerize_levels max_level ty1 ;
       lowerize_levels max_level ty2
   | ST_sum_arguments tys -> List.iter (lowerize_levels max_level) tys
   | ST_tuple tys -> List.iter (lowerize_levels max_level) tys
   | ST_construct (_, args) -> List.iter (lowerize_levels max_level) args
   | ST_prop | ST_self_rep | ST_species_rep _ -> ()
;;



(* *********************************************************************** *)
(** {b Descr} : Checks if a scheme contains type variables (generalized or
    not generalized).
    Such a check is required because species methods are not polymorphic
    (c.f. Virgile Prevosto's Phd section 3.3, page 24).

    {b Rem} : Exported oustide this module.                                *)
(* *********************************************************************** *)
let scheme_contains_variable_p scheme =
  let rec rec_check ty =
    let ty = repr ty in
    match ty with
     | ST_var _ -> true
     | ST_arrow (ty1, ty2) -> (rec_check ty1) || (rec_check ty2)
     | ST_sum_arguments tys -> List.exists rec_check tys
     | ST_tuple tys -> List.exists rec_check tys
     | ST_construct (_, args) -> List.exists rec_check args
     | ST_prop | ST_self_rep | ST_species_rep _ -> false in
  rec_check scheme.ts_body
;;



(* *********************************************************************** *)
(* type_simple -> type_simple                                              *)
(** {b Descr} : Extracts from a functionnal type the right-hand part of
    the arrow. This function assumes that the type IS an arrow and must be
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

    {b Rem} : Exported outside this module.                                *)
(* *********************************************************************** *)
let extract_fun_ty_result ~self_manifest ty =
  let ty = repr ty in
  match ty with
   | ST_arrow (_, res) -> res
   | ST_self_rep ->
       (begin
       (* We must then ensure that "Self" is currently known as being a
          functional type. *)
       match self_manifest with
        | None -> assert false
        | Some t ->
            let t = repr t in
            match t with
             | ST_arrow (_, res) -> res
             | _ -> assert false
       end)
   | _ -> assert false
;;

(* *********************************************************************** *)
(* type_simple -> type_simple                                              *)
(** {b Descr} : Extracts from a product type the list of simple types it is composed of.
                Sum arguments are considered as a product
    {b Rem} : Exported outside this module.                                *)
(* *********************************************************************** *)
let extract_prod_ty ~self_manifest ty =
  let ty = repr ty in
  match ty with
   | ST_tuple l -> l
   | ST_sum_arguments l -> l
   | ST_self_rep ->
       (begin
       match self_manifest with
        | None -> [ST_self_rep]
        | Some t ->
            let t = repr t in
            match t with
             | ST_tuple l -> l
             | _ -> [t]
       end)
   | _ -> [ty]
;;



(* *********************************************************************** *)
(* type_simple -> type_simple                                              *)
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

    {b Rem} : Exported outside this module.                                *)
(* *********************************************************************** *)
let extract_fun_ty_arg ~self_manifest ty =
  let ty = repr ty in
  match ty with
   | ST_arrow (arg, _) -> arg
   | ST_self_rep ->
       (begin
       (* We must then ensure that "Self" is currently known as being a
          functional type. *)
       match self_manifest with
        | None -> assert false
        | Some t ->
            let t = repr t in
            match t with
             | ST_arrow (arg, _) -> arg
             | _ -> assert false
       end)
   | _ -> assert false
;;



(* ********************************************************************* *)
(* type_simple -> bool                                                   *)
(** {b Descr} : Check if a [type_simple] contains a reference to "Self".

[Unsure] Ne devra plus �tre export�e une fois que les d�pendances sur "rep"
seront correctement utilis�es.
    {b Rem} : Exported oustide this module.                              *)
(* ********************************************************************* *)
let refers_to_self_p ty =
  let rec test t =
    let t =  repr t in
    match t with
     | ST_var _ -> false
     | ST_arrow (ty1, ty2) -> test ty1 || test ty2
     | ST_sum_arguments tys -> List.exists test tys
     | ST_tuple tys -> List.exists test tys
     | ST_construct (_, args) -> List.exists test args
     | ST_self_rep -> true
     | ST_prop | ST_species_rep _ -> false in
  test ty
;;



(* ********************************************************************* *)
(* type_simple -> bool                                                   *)
(** {b Descr} : Check if a [type_simple] contains a reference to "prop".

    {b Rem} : Exported oustide this module.                              *)
(* ********************************************************************* *)
let refers_to_prop_p ty =
  let rec test t =
    let t =  repr t in
    match t with
     | ST_var _ -> false
     | ST_arrow (ty1, ty2) -> test ty1 || test ty2
     | ST_sum_arguments tys -> List.exists test tys
     | ST_tuple tys -> List.exists test tys
     | ST_construct (_, args) -> List.exists test args
     | ST_prop -> true
     | ST_self_rep -> false
     | ST_species_rep _ -> false in
  test ty
;;


let (reset_deps_on_rep,
     get_def_dep_on_rep, set_def_dep_on_rep,
     get_decl_dep_on_rep, set_decl_dep_on_rep,
     check_for_decl_dep_on_self) =
  let found_decl = ref false in
  let found_def = ref false in
  (
   (* *********************************************************************** *)
   (* reset_deps_on_rep : unit -> unit                                        *)
   (** {b Descr} : Reset to [false] the flags telling if decl and def
       dependencies on the carrier were found. Because research of such
       dependencies are performed in the scope of a species field (i.e. "let",
       "let ...rec", "property", "theorem"), the liveness of these 2 flags is
       the same. Hence they always get reset at the same time.

       {b Rem} : Exported outside this module.                                *)
   (* *********************************************************************** *)
   (fun () ->
     found_decl := false ;
     found_def := false),
   (* get_def_dep_on_rep : unit -> bool *)
   (fun () -> !found_def),
   (* ********************************************************************* *)
   (* set_def_dep_on_rep : unit -> unit                                     *)
   (** {b Descr} : Turns on the flag telling that a def-dependency on the
       carrier was found.
       This function is only called by the unification function when one of
       the [SELF] rules of Virgile Prevosto's Phd is used (c.f. Definition
       28 page 50) and rules in Definition 9 page 27).

       [Rem] : Not exported outside this module.                            *)
   (* ********************************************************************* *)
   (fun () -> found_def := true),
   (* get_decl_dep_on_rep : unit -> bool *)
   (fun () -> !found_decl),
   (* ********************************************************************* *)
   (* set_decl_dep_on_rep : unit -> unit                                    *)
   (** {b Descr} : Turns on the flag telling that a decl-dependency on the
       carrier was found.

       [Rem] : Not exported outside this module.                            *)
   (* ********************************************************************* *)
   (fun () -> found_decl := true),
   (* ********************************************************************** *)
   (* check_for_decl_dep_on_self : type_simple -> unit                       *)
   (** {b Descr} : Turns on the flag telling that a decl-dependency on the
       carrier was found.
       This function is called by the type inference on expressions to
       check if the type of the expression is "Self". If so, then there is a
       decl-dependency and the flag is turned on.

       [Rem] : Exported outside this module .                                *)
   (* ********************************************************************** *)
   (fun ty -> if refers_to_self_p ty then found_decl := true)
  )
;;



(* ************************************************************************* *)
(** {b Descr} : Test if [var] occurs inside the structure of [ty] to prevent
    creating cyclic types. This is used when unifying a variable with
    something else.
    By the way, since it performs a walk on the whole type's structure, we
    take benefit of this to check if the unified type [ty] involved "Self",
    hence has a dependency on the carrier.

    {b Rem} : Non exported oustide this module.                              *)
(* ************************************************************************* *)
let occur_check ~loc var ty =
  let rec test t =
    let t = repr t in
    match t with
     | ST_var var' ->
         if var == var' then raise (Circularity (t, ty, loc))
     | ST_arrow (ty1, ty2) -> test ty1 ; test ty2
     | ST_sum_arguments tys -> List.iter test tys
     | ST_tuple tys -> List.iter test tys
     | ST_construct (_, args) -> List.iter test args
     | ST_prop
     | ST_species_rep _ -> ()
     | ST_self_rep ->
         (* There is a dependency on the carrier. Note it ! *)
         set_decl_dep_on_rep () in
  test ty
;;



let unify ~loc ~self_manifest type1 type2 =
  let rec rec_unify ty1 ty2 =
    let ty1 = repr ty1 in
    let ty2 = repr ty2 in
    if ty1 == ty2 then ty1 else
    match (ty1, ty2) with
     | (ST_var var, _) ->
         (* BE CAREFUL: [occur_check] performs the setting of decl-dependencies
            on the carrier ! In effect, if [ty2] involved Self then we have a
            dependency on the carrier and that must be taken into account !
            The interest to make [occur_check] doing this work is that it
            walk all along the type so it's a good idea to take benefit of this
            walk to avoid one more walk. *)
         occur_check ~loc var ty2 ;
         lowerize_levels var.tv_level ty2 ;
         var.tv_value <- TVV_known ty2 ;
         ty2
     | (_, ST_var var) ->
         (* BE CAREFUL: Same remark than above for [occur_check]. *)
         occur_check ~loc var ty1 ;
         lowerize_levels var.tv_level ty1 ;
         var.tv_value <- TVV_known ty1 ;
         ty1
     | ((ST_arrow (arg1, res1)), (ST_arrow (arg2, res2))) ->
         let arg3 = rec_unify arg1 arg2 in
         let res3 = rec_unify res1 res2 in
         ST_arrow (arg3, res3)
     | ((ST_sum_arguments tys1), (ST_sum_arguments tys2)) ->
         let tys3 =
           (try List.map2 rec_unify tys1 tys2 with
           | Invalid_argument "List.map2" ->
               (* In fact, that's an arity mismatch on the types. There is a
                  strange case appearing when using a sum type constructor that
                  requires arguments without arguments. The type of the
                  constructor's arguments is an ampty list. Then the conflict is
                  reported as "Types and ... are not compatible". Hence one of
                  the type is printed as nothing (c.f. bub report #180).
                  In this case, we generate a special error message. *)
               if (List.length tys1) = 0 || (List.length tys2) = 0 then
                 raise (Arity_mismatch_unexpected_args (loc))
               else raise (Conflict (ty1, ty2, loc))) in
         ST_sum_arguments tys3
     | ((ST_sum_arguments _), (ST_tuple _))
     | ((ST_tuple _), (ST_sum_arguments _)) ->
         (* Special cases to handle confusion between sum type value
            constructor's that take SEVERAL arguments and not 1 argument that
            is a tuple. *)
         raise (Arity_mismatch_unexpected_args (loc))
     | ((ST_tuple tys1), (ST_tuple tys2)) ->
         let tys3 =
           (try List.map2 rec_unify tys1 tys2 with
           | Invalid_argument "List.map2" ->
               (* In fact, that's an arity mismatch on the tuple. *)
               raise (Conflict (ty1, ty2, loc))) in
         ST_tuple tys3
     | (ST_construct (name, args), ST_construct (name', args')) ->
         (if name <> name' then raise (Conflict (ty1, ty2, loc))) ;
         let args'' =
           (try List.map2 rec_unify args args' with
           | Invalid_argument "List.map2" ->
               (* In fact, that's an arity mismatch. *)
               raise
                 (Arity_mismatch
                    (name, (List.length args), (List.length args'), loc))) in
         ST_construct (name, args'')
     | (ST_self_rep, ST_self_rep) ->
         (begin
         (* Trivial, but anyway, proceed as everywhere else. *)
         set_decl_dep_on_rep () ;
         ST_self_rep
         end)
     | (ST_self_rep, _) ->
         (begin
         match self_manifest with
          | None -> raise (Conflict (ty1, ty2, loc))
          | Some self_is_that ->
              ignore (rec_unify self_is_that ty2) ;
              set_def_dep_on_rep () ;
              (* Always prefer Self ! *)
              ST_self_rep
         end)
     | (_, ST_self_rep) ->
         (begin
         match self_manifest with
          | None -> raise (Conflict (ty1, ty2, loc))
          | Some self_is_that ->
              ignore (rec_unify self_is_that ty1) ;
              set_def_dep_on_rep () ;
              (* Always prefer Self ! *)
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
(** {b Descr} : Returns a copy of the variable [v].

    {b Rem} : Not exported oustide this module.                              *)
(* ************************************************************************* *)
let clone_variable v =
  {tv_value = v.tv_value;
   tv_level = v.tv_level}
;;

(* ************************************************************************* *)
(** {b Descr} : Copy the content of variable [v1] in [v2].

    {b Rem} : Not exported oustide this module.                              *)
(* ************************************************************************* *)
let copy_variable v1 v2 =
  v2.tv_value <- v1.tv_value;
  v2.tv_level <- v1.tv_level
;;

(* ************************************************************************* *)
(** {b Descr} : Unifies the type scheme [ts]
    with the simple type [st].

    [st] is assumed to be an instance of [ts].

    This functions returns a list of simple types
    corresponding to the instantiations of the variables in the scheme.

    This funcition is used in the Dedukti backend to fill
    type arguments which are passed as underscores to Coq.

    {b Rem} : Exported oustide this module.                              *)
(* ************************************************************************* *)
let unify_with_instance ts st =
  (* First make a copy because we don't want to change the scheme. *)
  let ts_vars = List.map clone_variable ts.ts_vars in
  (* Perform the unification *)
  ignore (unify ~loc:Location.none ~self_manifest:None ts.ts_body st);
  let results =
    List.map (fun v -> match v.tv_value with
                    | TVV_known t -> t
                    | TVV_unknown -> ST_var v)
             ts.ts_vars
  in
  (* Put the copied variables back in the scheme. *)
  List.iter2 copy_variable ts_vars ts.ts_vars;
  results
;;


(* ********************************************************************* *)
(** {b Descr} : Describes the kind of collection must be used to replace
    (i.e. that will be inserted instead of the replaced one) while
    performing a substitution on types.
    This type definition should be in the collection management module,
    but since it also operate on types' structure (which is abstract
    outside here) the only solution is to put it here.

    {b Rem} : Exported outside this module.                              *)
(* ********************************************************************* *)
type substitution_by_replacement_collection_kind =
  (** The collection to put instead of the replaced is the one named in
      the argument. *)
  | SBRCK_coll of type_collection
  | SBRCK_self  (** The collection to put instead of the replaced is Self. *)
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
  (* Internal recursive copy same stuff than for [specialize] stuff except
     that the generalization possibility does't matter here. *)
  let rec rec_copy ty =
    let ty = repr ty in
    match ty with
     | ST_var var ->
         (begin
         try List.assq var !seen
         with Not_found ->
           let fresh_var = type_variable () in
           (* Be careful, it's a copy, not a specialization ! Hence the
              original level of the type must be kept ! The fresh variable is
              create at the [current_binding_level], then in case the original
              one was created at a lower generic level, we [lowerize_levels]
              taking the level max equal to the level of the original
              variable. *)
           lowerize_levels var.tv_level fresh_var ;
           seen := (var, fresh_var) :: !seen ;
           fresh_var
         end)
     | ST_arrow (ty1, ty2) -> ST_arrow (rec_copy ty1, rec_copy ty2)
     | ST_sum_arguments tys -> ST_sum_arguments (List.map rec_copy tys)
     | ST_tuple tys ->  ST_tuple (List.map rec_copy tys)
     | ST_construct (name, args) ->
         ST_construct (name, List.map rec_copy args)
     | ST_self_rep -> ST_self_rep
     | ST_prop -> ST_prop
     | ST_species_rep (fname, coll_name) ->
         if fname = fname1 && coll_name = spe_name1 then
           (begin
           match c2 with
            | SBRCK_coll c2_ty -> ST_species_rep c2_ty
            | SBRCK_self -> ST_self_rep
           end)
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



let subst_type_scheme (fname1, spe_name1) c2 scheme =
  let body' = subst_type_simple (fname1, spe_name1) c2 scheme.ts_body in
  { scheme with ts_body = body' }
;;



(* ***************************************************************** *)
(* Format.formatter -> type_collection_name -> unit                  *)
(** {b Descr} : Pretty prints a collection' type (not carrier type).

    {b Rem} : Exported outside this module.                          *)
(* ***************************************************************** *)
let pp_type_collection ppf (coll_module, coll_name) =
  Format.fprintf ppf "%s#%s" coll_module coll_name
;;



type species_collection_kind =
  | SCK_toplevel_collection
  | SCK_toplevel_species
  | SCK_species_parameter
;;



(* ********************************************************************** *)
(* {b Descr}: Describes in the [scc_collections_carrier_mapping] the kind
   of species parameter.
   It can either be a "IS" parameter.
   Otherwise, it is a "IN" parameter. For Coq and Dedukti, we hence need to know
   if the type of this parameter is built from another of our species
   parameters of from a toplevel species/collection.

   {b Rem} : Exported outside this module.                                *)
(* ********************************************************************** *)
type collection_carrier_mapping_info =
    (** The parameter is a "is" parameter. *)
  | CCMI_is
    (** The parameter is a "in" parameter or is not a parameter. *)
  | CCMI_in of species_collection_kind
;;



(** Correspondance between collection parameters names and
    the names they are mapped onto in the Caml/Coq/Dedukti code and their kind.
    Note that in Coq/Dedukti, the mapped name doesn't have the trailing "_T". *)
type collection_carrier_mapping =
  (type_collection * (string * collection_carrier_mapping_info)) list
;;



(* ************************************************************************ *)
(** {b Descr} : Just for debug purpose, dumps the content of a collection
    carrier mapping.

    {b Rem} : Exported outside this module when needed, but should disapear
    once no more need to debug.                                             *)
(* ************************************************************************ *)
let debug_collection_carrier_mapping cmap =
  Format.eprintf "debug_collection_carrier_mapping START:@." ;
  List.iter
    (fun (ty_coll, (mapped_on, cm_info)) ->
      Format.eprintf "Carrier of '%a' is mapped onto '%s' and is "
        pp_type_collection ty_coll mapped_on ;
      match cm_info with
       | CCMI_is -> Format.eprintf "a IS parameter@."
       | CCMI_in sp_col_kind ->
           (begin
           Format.eprintf "a IN parameter of kind " ;
           match sp_col_kind with
            | SCK_toplevel_collection ->
                Format.eprintf "SCK_toplevel_collection@."
            | SCK_toplevel_species -> Format.eprintf "SCK_toplevel_species@."
            | SCK_species_parameter -> Format.eprintf "SCK_species_parameter@."
           end))
    cmap ;
  Format.eprintf "debug_collection_carrier_mapping END.@." ;
;;


module SpeciesCarrierType = struct
  type t =  (fname * collection_name)
  let compare = compare
end ;;



module SpeciesCarrierTypeSet = Set.Make (SpeciesCarrierType) ;;



(* ******************************************************************** *)
(* type_simple -> SpeciesCarrierTypeSet.t                               *)
(** {b Descr} : This function searches for species carrier types inside
    a [simple_type]. This will serve to determine which extra argument
    will have to be added in Coq and Dedukti to make these species carrier
    type abstracted by a parameter of type "Set".

    {b Rem} : Exported outside this module.                             *)
(* ******************************************************************** *)
let rec get_species_types_in_type ty =
  match repr ty with
   | ST_var _ -> SpeciesCarrierTypeSet.empty
   | ST_arrow (ty1, ty2) ->
       SpeciesCarrierTypeSet.union
         (get_species_types_in_type ty1) (get_species_types_in_type ty2)
   | ST_sum_arguments tys
   | ST_tuple tys
   | ST_construct (_, tys) ->
       List.fold_left
         (fun accu t ->
           SpeciesCarrierTypeSet.union accu (get_species_types_in_type t))
         SpeciesCarrierTypeSet.empty
         tys
   | ST_prop
   | ST_self_rep -> SpeciesCarrierTypeSet.empty
   | ST_species_rep st -> SpeciesCarrierTypeSet.singleton st
;;





let (pp_type_simple_to_xml, pp_type_variable_to_xml,
     purge_type_simple_to_xml_variable_mapping) =
  (* ********************************************************************* *)
  (* ((type_simple * string) list) ref                                     *)
  (** {b Descr} : The mapping giving for each variable already seen the
                name used to denote it while printing it.

      {b Rem} : Not exported. This mapping is purely local to the
              pretty-print function of type into the FoCaLize syntax. It is
              especially not shared with the type printing routine used to
              generate the OCaml, Coq or Dedukti code.                     *)
  (* ********************************************************************* *)
  let type_variable_names_mapping = ref ([] : (type_variable * string) list) in

  (* ******************************************************************* *)
  (* int ref                                                             *)
  (** {b Descr} : The counter counting the number of different variables
      already seen hence printed. It serves to generate a fresh name to
      new variables to print.

      {b Rem} : Not exported. This counter is purely local to the
      pretty-print function of type into the FoCaLize syntax. It is
      especially not shared with the type printing routine used to
      generate the OCaml, Coq or Dedukti code.                           *)
  (* ******************************************************************* *)
  let type_variables_counter = ref 0 in

  (* ************************************************************* *)
  (* unit -> unit                                                  *)
  (** {b Descr} : Resets the variables names mapping an counter.
      This allows to stop name-sharing between type prints.

     {b Rem} : Not exported. This counter is purely local to the
      pretty-print function of type into the FoCaLize syntax. It is
      especially not shared with the type printing routine used to
      generate the OCaml, Coq or Dedukti code.                     *)
  (* ************************************************************* *)
  let reset_type_variables_mapping () =
    type_variable_names_mapping := [] ;
    type_variables_counter := 0 in

  let get_or_make_type_variable_name ty_var =
    (* No need to repr, [rec_pp] already did it. *)
    try List.assq ty_var !type_variable_names_mapping with
    | Not_found ->
        let name = Handy.int_to_base_26 !type_variables_counter in
        incr type_variables_counter ;
        type_variable_names_mapping :=
          (ty_var, name) :: !type_variable_names_mapping ;
        name in

  let internal_pp_var_to_xml ppf ty_var =
    let ty_variable_name = get_or_make_type_variable_name ty_var in
    Format.fprintf ppf "<foc:tvar>%s</foc:tvar>@\n" ty_variable_name in


  let rec rec_pp ppf ty =
    (* First of all get the "repr" guy ! *)
    let ty = repr ty in
    match ty with
    | ST_var ty_var -> internal_pp_var_to_xml ppf ty_var
    | ST_arrow (ty1, ty2) ->
        Format.fprintf ppf "@[<h 2><foc:fct>@\n%a%a@]</foc:fct>@\n"
          rec_pp  ty1 rec_pp ty2 ;
    | ST_sum_arguments tys ->
        Format.fprintf ppf "@[<h 2><foc:sum_args>@\n" ;
        List.iter (rec_pp ppf) tys ;
        Format.fprintf ppf "@]</foc:sum_args>@\n"
    | ST_tuple tys ->
        Format.fprintf ppf "@[<h 2><foc:prod>@\n" ;
        List.iter (rec_pp ppf) tys ;
        Format.fprintf ppf "@]</foc:prod>@\n"
    | ST_construct ((mod_name, cstr_name), arg_tys) ->
        (begin
        match arg_tys with
         | [] ->
             (* order = "first" because the atom does not represent a
                species. *)
             Format.fprintf ppf
               "<foc:atom order=\"first\" infile=\"%s\">%s</foc:atom>@\n"
               mod_name cstr_name
         | _ ->
             (* order = "first" because the atom does not represent a
                species. *)
             Format.fprintf ppf "@[<h 2><foc:prm order=\"first\">" ;
             List.iter (rec_pp ppf) arg_tys ;
             Format.fprintf ppf
               "<foc:foc-name infile=\"%s\">%s</foc:foc-name>@\n"
               mod_name cstr_name ;
             Format.fprintf ppf "@]</foc:prm>@\n"
        end)
    | ST_prop -> Format.fprintf ppf
               "<foc:atom order=\"first\" infile=\"coq_builtins\">prop__t</foc:atom>@\n"
    | ST_self_rep ->
        (* order = "first" because the atom does not represent a species. *)
        Format.fprintf ppf "<foc:self order=\"first\"/>@\n"
    | ST_species_rep (mod_name, collection_name) ->
        (* order = "first" because the atom does not represent a species.
           Here it represents a **carrier** of species. *)
        Format.fprintf ppf
          "<foc:atom order=\"first\" infile=\"%s\">%s</foc:atom>@\n"
          mod_name collection_name in


  ((* pp_type_simple_to_xml *)
   (fun ppf ty -> rec_pp ppf ty),
   (* pp_type_variable_to_xml *)
   internal_pp_var_to_xml,
   (* purge_type_simple_to_xml_variable_mapping *)
   (fun () -> reset_type_variables_mapping ())
  )
;;








type local_type =
  | Lt_var of int
  | Lt_fun of local_type * local_type
  | Lt_tuple of local_type list
  | Lt_constr of (string * string) * local_type list
  | Lt_prop
  | Lt_self
  | Lt_species of (string * string)
;;

let rec type_simple_to_local_type ty =
  let ty = repr ty in
  match ty with
   | ST_var type_variable ->
       begin
       match type_variable.tv_value with
        | TVV_unknown ->
            (* [julius:] it's a "true" polymorphic value *)
            Lt_var type_variable.tv_level
        | TVV_known ts ->
            (* [julius:] type has been deduced *)
            type_simple_to_local_type ts
       end

   | ST_arrow (l, r) ->
       (* [julius:] no changes. *)
       Lt_fun (type_simple_to_local_type l, type_simple_to_local_type r)

   | ST_sum_arguments _ -> failwith "Julien, � toi de voir..."

   | ST_tuple l ->
       (* [julius:] no changes. *)
       Lt_tuple (List.map type_simple_to_local_type l)

   | ST_construct ((f, id), l) ->
       Lt_constr ((f, id), (List.map type_simple_to_local_type l))

   | ST_prop -> Lt_prop

   | ST_self_rep ->
       Lt_self

   | ST_species_rep (f, c) ->
       Lt_species (f, c)
;;



let type_scheme_to_local_type ts = type_simple_to_local_type ts.ts_body ;;

(* for focaltest : *)
let extract_type_simple 
        (fvar : unit -> 'a)
        (farrow : type_simple -> type_simple -> 'a)
        (ftuple : type_simple list -> 'a)
        (fsum : type_simple list -> 'a)
        (fconstruct : string -> string -> type_simple list -> 'a)
        (fprop : unit -> 'a)
        (frep : unit -> 'a)
        (fspecrep : fname -> collection_name -> 'a) ts =
         let ts = repr ts in
         match ts with
         | ST_var _ -> fvar ()
         | ST_arrow ((t1,t2)) -> farrow t1 t2
         | ST_tuple(t_l) -> ftuple t_l
         | ST_sum_arguments(t_l) -> fsum t_l
         | ST_construct((t, t_l)) -> fconstruct (fst t) (snd t) t_l
         | ST_prop -> fprop ()
         | ST_self_rep -> frep ()
         | ST_species_rep((fn, cn)) -> fspecrep fn cn;;

let nb_variable_type_scheme t = List.length t.ts_vars;;
let type_variable_eq t1 t2 =
  match t1, t2 with
  | ST_var e2, ST_var e1 -> Some (e1 == e2)
  | _, _ -> None;;

(* *************** *)
