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


(* $Id: dep_analysis.ml,v 1.1 2007-08-29 12:47:48 pessaux Exp $ *)

module VnameMod = struct type t = Parsetree.vname let compare = compare end ;;
module VnameSet = Set.Make (VnameMod) ;;



(* ********************************************************************** *)
(* current_species: Types.collection_name -> Parsetree.expr -> VnameSet.t *)
(** {b Descr} : Compute the set of vnames the expression [expression]
              depends of in the species [~current_species]

    {b Rem} : Not exported outside this module.                           *)
(* ********************************************************************** *)
let expr_dependencies ~current_species expression =
  (* Let's just make a local function to save the stack, avoid *)
  (* passing each time the parameter [~current_species].       *)
  let rec rec_depend expr =
    match expr.Parsetree.ast_desc with
     | Parsetree.E_self
     | Parsetree.E_const _ -> VnameSet.empty
     | Parsetree.E_fun (_, body) -> rec_depend body
     | Parsetree.E_var ident ->
	 (begin
	 match ident.Parsetree.ast_desc with
	  | Parsetree.I_local _ ->
	      (* Because scoping pass already renamed all the identfiers that *)
	      (* "looked like" local identifiers into "method identifiers" if *)
	      (* they indeed denoted methods, we can safely consider that     *)
              (* remaining "local identifiers" are really local and introduce *)
	      (* no dependency.                                               *)
	      VnameSet.empty
	  | Parsetree.I_global (_, _) -> VnameSet.empty
	  | Parsetree.I_method (coll_name_opt, vname) ->
	      (begin
	      match coll_name_opt with
	       | None ->
                   (* Case c!x in Virgile Prevosto's Phd, section 3.5, *)
		   (* page 30, definition 12.                          *)
		   VnameSet.singleton vname
	       | Some coll_name when coll_name = current_species ->
		   (* Case c!x in Virgile Prevosto's Phd, section 3.5, *)
		   (* page 30, definition 12.                          *)
		   VnameSet.singleton vname
	       | Some _ -> VnameSet.empty
	      end)
	 end)
     | Parsetree.E_app (fun_expr, args_exprs) ->
	 let fun_expr_deps = rec_depend fun_expr in
	 List.fold_left
	   (fun accu_deps e -> VnameSet.union (rec_depend e) accu_deps)
	   fun_expr_deps
	   args_exprs
     | Parsetree.E_constr (_, args_exprs) ->
	 List.fold_left
	   (fun accu_deps e -> VnameSet.union (rec_depend e) accu_deps)
	   VnameSet.empty
	   args_exprs
     | Parsetree.E_match (matched_e, pats_exprs) ->
	 let matched_e_deps = rec_depend matched_e in
	 List.fold_left
	   (fun accu_deps (_, e) -> VnameSet.union (rec_depend e) accu_deps)
	   matched_e_deps
	   pats_exprs
     | Parsetree.E_if (if_expr, then_expr, else_expr) ->
	 let if_expr_deps = rec_depend if_expr in
         let then_expr_deps = rec_depend  then_expr in
	 let else_expr_deps = rec_depend else_expr in
	 VnameSet.union
	   if_expr_deps (VnameSet.union then_expr_deps else_expr_deps)
     | Parsetree.E_let (let_def, in_expr) ->
	 (* No substration here because the let-definition *)
         (* is NOT a field definition !                    *)
	 let in_expr_deps = rec_depend in_expr in
	 List.fold_left
	   (fun accu_deps binding ->
	     VnameSet.union
	       (rec_depend binding.Parsetree.ast_desc.Parsetree.b_body)
	       accu_deps)
	   in_expr_deps
	   let_def.Parsetree.ast_desc.Parsetree.ld_bindings
     | Parsetree.E_record labels_exprs ->
	 List.fold_left
	   (fun accu_deps (_, e) -> VnameSet.union (rec_depend e) accu_deps)
	   VnameSet.empty
	   labels_exprs
     | Parsetree.E_record_access (e, _) -> rec_depend e
     | Parsetree.E_record_with (e, labels_exprs) ->
	 let e_deps = rec_depend e in
	 List.fold_left
	   (fun accu_deps (_, e) -> VnameSet.union (rec_depend e) accu_deps)
	   e_deps
	   labels_exprs
     | Parsetree.E_tuple exprs ->
	 List.fold_left
	   (fun accu_deps e -> VnameSet.union (rec_depend e) accu_deps)
	   VnameSet.empty
	   exprs
     | Parsetree.E_external _ -> VnameSet.empty
     | Parsetree.E_paren e -> rec_depend e in
  rec_depend expression
;;



(* ******************************************************************** *)
(* current_species: Types.collection_name ->                            *)
(*   Env.TypeInformation.species_field -> VnameSet.t                    *)
(** {b Descr} : Compute the set of vnames the argument field depends of
              in the species [~current_species]

    {b Rem} : Exported outside this module.                             *)
(* ******************************************************************** *)
let field_dependencies ~current_species = function
  | Env.TypeInformation.SF_sig (_, _) -> VnameSet.empty
  | Env.TypeInformation.SF_let (_, _, body) ->
      expr_dependencies ~current_species body
  | Env.TypeInformation.SF_let_rec l ->
      (* Create the set of names to remove afterwards. *)
      let names_of_l =
	List.fold_left
	  (fun accu_set (n, _, _) -> VnameSet.add n accu_set)
	  VnameSet.empty
	  l in
      (* Now, compute the dependencies on all the rec-bound-names. *)
      let deps_of_l =
	List.fold_left
	  (fun accu_deps (_, _, body) ->
	    let d = expr_dependencies ~current_species body in
	    VnameSet.union d accu_deps)
	  VnameSet.empty
	  l in
      (* And now, remove the rec-bound-names from the dependencies. *)
      VnameSet.diff deps_of_l names_of_l
;;



(* ********************************************************************* *)
(* Parsetree.vname -> Env.TypeInformation.species_field list ->          *)
(*   Env.TypeInformation.species_field list                              *)
(** {b Descr} : Compute the set of all the names involved in the
              "clockwise arrow" relation (c.f. Virgile Prevosto's
              Pdh, section 3.5, page 30) for the name [field_name]
              in the fields [fields] that must include the inherited
              fields of the analysed species.
              Instead of just returning the names, it returns the fields
              these names belong to. If one really need the names, then
              we will just extract them afterwards.
              Returns the list of fields in their order of apparition in
              the original list. This means that if oldest inherited
              fields are in head of the list, so they will be in the
              resulting list.

    {b Rem} : MUST BE used on a fields list containing all the
             **normalized inherited fields** of the species and the
             **non normalized fields of the current inheritance level**.
             This means that the inherited fields must already be
             fusionned if they needed, and the current level fields may
             not be fusionned this the inherited ones.
             This enables not to have to seach all along the inheritance
             tree because let rec will be inductively correct (i.e. all
             the recursive ident will be in ONE field) for inherited
             fields and keeps the current level fields present in order
             to check for let rec at this level.                         *)
(* ********************************************************************* *)
let clockwise_arrow field_name fields =
  List.fold_right
    (fun field accu ->
      match field with
       | Env.TypeInformation.SF_sig (vname, _)
       | Env.TypeInformation.SF_let (vname, _, _) ->
	   if vname = field_name then field :: accu else accu
       | Env.TypeInformation.SF_let_rec l ->
	   (* Check if the searched field name is among those in this     *)
           (* recursive let definition. If so, then the relation includes *)
           (* all the bound names of this recursive let definition.       *)
	   if List.exists (fun (vname, _, _) -> vname = field_name) l then
	     field :: accu
	   else accu)
    fields
    []
;;



(* ********************************************************************* *)
(* Parsetree.vname -> Env.TypeInformation.species_field list ->          *)
(*   Env.TypeInformation.species_field list                              *)
let where field_name fields =
  (* First, compute the FIELD-INTRO rule of the relation. Because *)
  (* [clockwise_arrow] directly returns a fields list instead of  *)
  (* simply names, the set of phis is trivialy obtained.          *)
  let phi_for_field_intro = clockwise_arrow field_name fields in
  (* Now, there is nothing to do for the FIELD-INH rule of the relation. *)
  (* Thanks to our incremental process because fields of the current     *)
  (* level ARE NOT normalized, them, they will lie in another field with *)
  (* its bindings that are not already fusionned with the inherited and  *)
  (* already normalized fields.                                          *)
  phi_for_field_intro
;;



(* Just helper. *)
let names_set_of_field = function
  | Env.TypeInformation.SF_sig (vname, _)
  | Env.TypeInformation.SF_let (vname, _, _) -> VnameSet.singleton vname
  | Env.TypeInformation.SF_let_rec l ->
      List.fold_left
	(fun accu_names (n, _, _) -> VnameSet.add n accu_names)
	VnameSet.empty
	l
;;


(* *********************************************************************** *)
(** {b Descr} : Implements the second case of the definition 16 in Virgile
              Prevosto's Phd, section 3.5, page 32.

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let union_y_clock_x_etc ~current_species x_name fields =
  let all_ys = clockwise_arrow x_name fields in
  List.fold_left
    (fun accu_deps field_y ->
      (* First compute the great union. *)
      let u =
	VnameSet.union
	  (field_dependencies ~current_species field_y) accu_deps in
      (*  Then remove the recursive bound names. *)
      let rec_bound_names = names_set_of_field field_y in
      VnameSet.diff u rec_bound_names)
    VnameSet.empty
    all_ys
;;



(* current_species: Types.collection_name ->                         *)
(*   (Parsetree.vname * Parsetree.expr) ->                           *)
(*     Env.TypeInformation.species_field list -> VnameSet.t          *)
(* Just helper. *)
let in_species_dependencies_for_one_name ~current_species (name, body) fields =
  let where_x = where name fields in
  (* Check if Where (x) does NOT contain Let_rec fields. *)
  if List.for_all
      (function
	| Env.TypeInformation.SF_let_rec _ -> false
	| _ -> true)
      where_x then expr_dependencies ~current_species body
  else union_y_clock_x_etc ~current_species name fields
;;


(* current_species: Types.collection_name ->                 *)
(*   Env.TypeInformation.species_field list -> VnameSet.t    *)
let in_species_dependencies ~current_species fields =
  List.fold_left
    (fun accu_deps field ->
      match field with
       | Env.TypeInformation.SF_sig (_, _) -> 
	   (* The field is not defined, hence it doe not belong to D(s). *)
	   accu_deps
       | Env.TypeInformation.SF_let (name, _, body) ->
	   (begin
	   let deps =
	     in_species_dependencies_for_one_name
	       ~current_species (name, body) fields in
	   VnameSet.union deps accu_deps
	   end)
       | Env.TypeInformation.SF_let_rec l ->
	   (begin
	   List.fold_left
	     (fun accu_deps' (name, _, body) ->
	       let deps =
		 in_species_dependencies_for_one_name
		   ~current_species (name, body) fields in
	       VnameSet.union deps accu_deps')
	     accu_deps
	     l
	   end))
    VnameSet.empty
    fields
;;

(*
let left_triangle x1 x2 fields =
  let y1s = clockwise_arrow x1 fields in
  let yns = clockwise_arrow x2 in
  List.exists
    (fun y1 ->
    )
    y1s
;;
*)


(** Mostly debug. Must disapear. *)
let debug_clockwise_arrow_equiv_class field_name fields =
  let phis =  where field_name fields in
  List.fold_left
    (fun accu field ->
      match field with
       | Env.TypeInformation.SF_sig (vname, _)
       | Env.TypeInformation.SF_let (vname, _, _) ->
	   if vname = field_name then
	     if List.mem vname accu then accu
	     else vname :: accu else accu
       | Env.TypeInformation.SF_let_rec l ->
	   List.fold_left
	     (fun accu' (vname,  _, _) ->
	       if List.mem vname accu' then accu' else vname :: accu')
             accu l)
    []
    phis
;;


let debug_where fields =
  List.iter
    (function
       | Env.TypeInformation.SF_sig (vname, _) 
       | Env.TypeInformation.SF_let (vname, _, _) ->
	   let w = where vname fields in
	   Format.eprintf "Where (%a) : { " Sourcify.pp_vname vname ;
	   List.iter
	     (fun f ->
	       let s = names_set_of_field f in
	       let s' = VnameSet.elements s in
	       Format.eprintf "[%a] " (Sourcify.pp_vnames ",") s')
	     w ;
	   Format.eprintf "}@."
       | Env.TypeInformation.SF_let_rec l ->
	   (begin
	   List.iter
	     (fun (vname, _, _) ->
	       let w = where vname fields in
	       Format.eprintf "Where (%a) : { " Sourcify.pp_vname vname ;
	       List.iter
		 (fun f ->
		   let s = names_set_of_field f in
		   let s' = VnameSet.elements s in
		   Format.eprintf "[%a] " (Sourcify.pp_vnames ",") s')
		 w ;
	       Format.eprintf "}@.")
	     l
	   end))
    fields
;;


let debug_in_species_dependencies ~current_species fields =
  Format.eprintf "Dependences de l'espèce '%s' : " current_species ;
  let s = in_species_dependencies ~current_species fields in
  let s' = VnameSet.elements s in
  Format.eprintf "{%a}" (Sourcify.pp_vnames ",") s' ;
  Format.eprintf "@."
;;
