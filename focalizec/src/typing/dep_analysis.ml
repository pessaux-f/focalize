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


(* $Id: dep_analysis.ml,v 1.3 2007-08-31 13:45:52 pessaux Exp $ *)

(* *********************************************************************** *)
(** {b Descr} : This module performs the well-formation analysis described
              in Virgile Prevosto's Phd, section 3.5.                      *)
(* *********************************************************************** *)



(* ********************************************************** *)
(** {b Descr} : Raised if a species appears to be ill-formed.

    {b Rem} : Exported outside this module.                   *)
(* ********************************************************** *)
exception Ill_formed_species of Types.species_name ;;



(* *************************************************************** *)
(** {b Descr} : Module stuff to create sets of [Parsetree.vanme]s.
 
    {b Rem} : Not exported outside this module.                    *)
(* *************************************************************** *)
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
	  if vname = field_name then Handy.list_cons_uniq_eq vname accu
	  else accu
       | Env.TypeInformation.SF_let_rec l ->
	   (* Check if the searched field name is among those in this     *)
           (* recursive let definition. If so, then the relation includes *)
           (* all the bound names of this recursive let definition.       *)
	   if List.exists (fun (vname, _, _) -> vname = field_name) l then
	     List.fold_right
	       (fun (n, _, _) accu' -> Handy.list_cons_uniq_eq n accu')
	       l accu
	   else accu)
    fields
    []
;;



(* ******************************************************************** *)
(* Parsetree.vname -> Env.TypeInformation.species_field list ->         *)
(*   Env.TypeInformation.species_field list                             *)
(** {b Descr} : Computes the set Where as defined in Virgile Prevosto's
              Phd, section 3.5, page 32, definition 15.

    {b Rem} : Not exported outside this module.                         *)
(* ******************************************************************** *)
let where field_name fields =
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



(* ******************************************************************* *)
(* Env.TypeInformation.species_field -> VnameSet.t                     *)
(** {b Descr} : Just an helper returning the set of all names bound in
              a species fields.

    {b Rem} : Not exported outside this module.                        *)
(* ******************************************************************* *)
let names_set_of_field = function
  | Env.TypeInformation.SF_sig (vname, _)
  | Env.TypeInformation.SF_let (vname, _, _) -> VnameSet.singleton vname
  | Env.TypeInformation.SF_let_rec l ->
      List.fold_left
	(fun accu_names (n, _, _) -> VnameSet.add n accu_names)
	VnameSet.empty
	l
;;



(* ************************************************************************ *)
(** {b Descr} : Just helper returning the list of all names bound in a list
              of fields. The resulting list preserves the order the names
              appear in the list of fields and in the list of names in
              case of recursive let-def field.
              Example: [let s ; sig y ; let rec z ... and t] will give the
              list [s; y; z; t].

    {b Rem} : Not exported outside this module.                             *)
(* ************************************************************************ *)
let ordered_names_list_of_fields fields =
  List.fold_right
    (fun field accu ->
      match field with
       | Env.TypeInformation.SF_sig (n, _)
       | Env.TypeInformation.SF_let (n, _, _) -> n :: accu
       | Env.TypeInformation.SF_let_rec l ->
	   List.fold_right (fun (n, _, _) accu' -> n :: accu') l accu)
    fields
    []
;;
	       


(* ****************************************************************** *)
(* Parsetree.vname -> Env.TypeInformation.species_field list ->       *)
(*   Env.TypeInformation.species_field                                *)
(** {b Descr} : Looks for the most recently defined field that binds 
              [y_name] among [fields] and return it.
              This function relies on the fact that the field list is
              ordered with oldest inherited fields are in head of the
              list and the most recent are in tail.

    {b Rem} : Not exported outside this module.                       *)
(* ****************************************************************** *)
let find_most_recent_rec_field_binding y_name fields =
  (* The search is a simple walk in the list, starting by the head and    *)
  (* going on with the tail. Because fields list is assumed to be ordered *)
  (* with the oldest inherited fields in head, one will have to reverse   *)
  (* the initial list of field before applying the present function.      *)
  let rec rec_search = function
    | [] -> raise Not_found
    | h :: q ->
	(begin
	match h with
	 | Env.TypeInformation.SF_sig (_, _)
	 | Env.TypeInformation.SF_let (_, _, _) ->
	     rec_search q
       | Env.TypeInformation.SF_let_rec l ->
	   if List.exists (fun (n, _, _) -> n = y_name ) l then
	     h
	   else rec_search q
	end) in
  (* Reverse the list so that most recent names are in head. *)
  rec_search (List.rev fields)
;;



(* *********************************************************************** *)
(* current_species: Types.collection_name -> Parsetree.vname ->            *)
(*   Env.TypeInformation.species_field list -> VnameSet.t                  *)
(** {b Descr} : Implements the second case of the definition 16 in Virgile
              Prevosto's Phd, section 3.5, page 32.

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let union_y_clock_x_etc ~current_species x_name fields =
  let all_ys = clockwise_arrow x_name fields in
  List.fold_left
    (fun accu_deps y_name ->
      (* First compute the great union. *)
      (* We look for the most recently defined field that binds [y_name]. *)
      (* This way, we really "compute" \Cal B_s (y) because \Cal B is     *)
      (* defined to return the body of the most recent fiedl in the       *)
      (* inheritance.                                                     *)
      let field_y = find_most_recent_rec_field_binding y_name fields in
      let u =
	VnameSet.union
	  (field_dependencies ~current_species field_y) accu_deps in
      (*  Then remove the recursive bound names. *)
      let rec_bound_names = names_set_of_field field_y in
      VnameSet.diff u rec_bound_names)
    VnameSet.empty
    all_ys
;;



(* ********************************************************************* *)
(* current_species: Types.collection_name ->                             *)
(*   (Parsetree.vname * Parsetree.expr) ->                               *)
(*     Env.TypeInformation.species_field list -> VnameSet.t              *)
(** {b Descr} : Compute the dependencies of a name in a species. Namely
              this is the \lbag x \rbag_s in Virgile Prevosto's Pdh,
              section 3.5, page 32, definition 16.

    {b Rem} : Not exported outside this module.                          *)
(* ********************************************************************* *)
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



(* ************************************************************************ *)
(** {b Descr} : Strutrure of a node in a dependency graph representing the
            fact that some names' bodies contain call to non-let-rec-bound
            othernames (relation \lbag n \rbag in Virgile Prevosto's Phd,
            section 3.5, definition 16, page 32.

    {b Rem} : Not exported outside this module.                             *)
(* ************************************************************************ *)
type name_node = {
 (** Name of the node, i.e. one name of a species fields. *)
  nn_name : Parsetree.vname ;
 (** Means that the current names depends of the children nodes. I.e. the
     current name's body contains calls to the children names. *)
  mutable nn_children : name_node list
} ;;



(* ******************************************************************** *)
(* name_node list ref -> Parsetree.vname -> name_node                   *)
(** {b Descr} : Looks for a node labeled [name] in the list of nodes
              [tree_nodes]. If a node with this name is found, then we
              return it. Otherwise, a fresh node is created with [name]
              as name and no child, and this fresh node is returned.
              This is mostly a helper for the function
              [build_dependencies_graph_for_fields].

    {b Rem} : Not exported outside this module.                         *)
(* ******************************************************************** *)
let find_or_create tree_nodes name =
  try List.find (fun node -> node.nn_name = name) !tree_nodes
  with Not_found ->
    let new_node = { nn_name = name ; nn_children = [] } in
    tree_nodes := new_node :: !tree_nodes ;
    new_node
;;



(* ********************************************************************* *)
(* current_species: Types.collection_name ->                             *)
(*   Env.TypeInformation.species_field list -> name_node list            *)
(** {b Descr} : Build the dependencies graph of the names present in the
              fields list [fields] of the species [~current_species].
              In such a graph, if an arrow exists from n1 to n2, then
              it means that in the body of n1, call(s) to n2 is (are)
              performed.

    {b Rem} : Not exported outside this module.                          *)
(* ********************************************************************* *)
let build_dependencies_graph_for_fields ~current_species fields =
  let tree_nodes = ref ([] : name_node list) in
  (* Just make a local function dealing with one let binding. We *)
  (* when use it once for a Let and iter it for a Let_Rec.       *)
  let local_build_for_one_let n b =
    (* Find the dependencies node for the current name. *)
    let n_node = find_or_create tree_nodes n in
    (* Find the names dependencies for the current name. *)
    let n_deps_names =
      in_species_dependencies_for_one_name ~current_species (n, b) fields in
    (* Now, find the dependencies nodes for these names. *)
    let n_deps_nodes =
      VnameSet.fold
	(fun n accu ->
	  let node = find_or_create tree_nodes n in
	  node :: accu)
	n_deps_names
	[] in
    (* Now add an edge from the current name's node to each of the *)
    (* dependencies names' nodes.                                  *)
    n_node.nn_children <-
      Handy.list_concat_uniqq n_deps_nodes n_node.nn_children in
  (* Now do the job. *)
  List.iter
    (function
      | Env.TypeInformation.SF_sig (n, _) ->
	  if not (List.exists (fun node -> node.nn_name = n) !tree_nodes) then
	    tree_nodes := { nn_name = n ; nn_children = [] } :: !tree_nodes
      | Env.TypeInformation.SF_let (n, _, b) -> local_build_for_one_let n b
      | Env.TypeInformation.SF_let_rec l ->
	  List.iter (fun (n, _, b) -> local_build_for_one_let n b) l)
    fields ;
  (* Return the list of nodes of the graph. *)
  !tree_nodes
;;



(* ************************************************************************ *)
(* dirname: string -> current_species: string -> name_node list -> unit     *)
(** {b Descr} : Prints the dependencies graph of a species in dotty format.

    {b Rem} : Not exported outside this module.                             *)
(* ************************************************************************ *)
let dependencies_graph_to_dotty ~dirname ~current_species tree_nodes =
  (* For each species, a file named with "deps_", the species name *)
  (* and the suffix ".dot" will be generated in the directory.     *)
  let out_filename =
    Filename.concat dirname ("deps_" ^ current_species ^ ".dot") in
  let out_hd = open_out_bin out_filename in
  (* First, outputs the header of the dotty file. *)
  Printf.fprintf out_hd "digraph G {\n" ;
  (* Outputs all the nodes og the graph. *)
  List.iter
    (fun { nn_name = n } -> 
      Printf.fprintf out_hd "\"%s\" [shape=box,fontsize=10] ;\n"
	(Parsetree_utils.name_of_vname n))
    tree_nodes ;
  (* Outputs all the edges between the nodes. *)
  List.iter
    (fun { nn_name = n ; nn_children = children } -> 
      List.iter
	(fun { nn_name = child_name } ->
	  Printf.fprintf out_hd
	    "\"%s\" -> \"%s\" [style=dotted,color=blue,fontsize=10] ;"
	    (Parsetree_utils.name_of_vname n)
	    (Parsetree_utils.name_of_vname child_name))
	children)
    tree_nodes ;
  (* Finally, outputs the trailer of the dotty file. *)
  Printf.fprintf out_hd " \n}\n" ;
  close_out out_hd
;;



(* ************************************************************************ *)
(* name_node -> name_node -> bool                                           *)
(** {b Descr} : Checks for a non trivial path from [start_node] to
              [start_node]. "Non-trivial" means that the path must at least
              be of length 1.

    {b Rem} : Not exported outside this module.                             *)
(* ************************************************************************ *)
let is_reachable start_node end_node =
  (* List of already seen nodes. Will be extended during the search. *)
  let seen = ref ([] : name_node list) in
  let rec rec_search current_node =
    (* If the current node was already seen, this means that ... we already  *)
    (* saw it, then we already checked if the [end_node] was in its children *)
    (* and the anwser was NOT. Hence there is no reason to start again the   *)
    (* search, we will get the same answer forever (and loop forever of      *)
    (* course by the way).                                                   *)
    if List.memq current_node !seen then false
    else
      (begin
      (* We check if the current node's children contain en [end_node]. This *)
      (* way, for each node, we are sure that the possibly found path is not *)
      (* the trivial path (because we explicitely look inside the children   *)
      (* and if a node is acceptable in the children, then the path length   *)
      (* is mandatorily non-null).                                           *)
      if List.memq end_node current_node.nn_children then true
      else
	(begin
	seen := current_node :: !seen ;
        (* The [end_node] was not found in the children, *)
        (* then search in the children.                  *)
	List.exists rec_search current_node.nn_children
	end)
      end) in
  (* Start the search. *)
  rec_search start_node
;;



(* *********************************************************************** *)
(* current_species: Types.collection_name -> Parsetree.vname ->            *)
(*   Parsetree.vname -> Env.TypeInformation.species_field list -> bool     *)
(** {b Descr} : Implements the relation "left-oriented triangle" of
              Virgile Prevosto's Phd, section 3.5, page 32, definition 17.
              Also output the dependencies as a dotty file if asked in the
              command-line options.

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let left_triangle ~current_species x1 x2 fields =
  (* Guess the fields where x1 is recursively bound. *)
  let x1_arrow = clockwise_arrow x1 fields in
  (* Guess the fields where x2 is recursively bound. *)
  let x2_arrow = clockwise_arrow x2 fields in
  (* Now, let's build the global dependencies graph for all the names. *)
  let dep_graph_nodes =
    build_dependencies_graph_for_fields ~current_species fields in
  (* If asked, generate the dotty output of the dependencies. *)
  (match Configuration.get_dotty_dependencies () with
   | None -> ()
   | Some dirname ->
       dependencies_graph_to_dotty ~dirname ~current_species dep_graph_nodes) ;
  (* Now we will apply the "well-formness" predicate on the cartesian  *)
  (* product of the names bound by "clockwise-arrow" of [x1] and those *)
  (* bound by "clockwise-arrow" of [x2]. Intuitively, we will apply    *)
  (* this predicate on all possible combinaisons of [y_1] and [y_n].   *)
  List.exists
    (fun y1 ->
      List.exists
	(fun yn ->
	  (* Search a path in the graph from yn to y2. The lookup for *)
          (* nodes y1 and yn should never fail because the graph was  *)
	  (* created before.                                          *)
	  let y1_node =
	    List.find (fun node -> node.nn_name = y1) dep_graph_nodes in
	  let yn_node =
	    List.find (fun node -> node.nn_name = yn) dep_graph_nodes in
          (* Because our graph edges link from yn to y1, we must invert *)
          (* the start/end nodes.                                       *)
	  is_reachable yn_node y1_node)
	x2_arrow)
    x1_arrow
;;



(* ************************************************************************ *)
(* current_species: Types.collection_name ->                                *)
(*   Env.TypeInformation.species_field list -> unit                         *)
(** {b Descr} : Checks if a species is well-formed, applying the definition
              17 in Virgile Prevosto's Phd, section 3.5, page 32

    {b Rem} : Exported outside this module.                                 *)
(* ************************************************************************ *)
let ensure_species_well_formed ~current_species fields =
  let names = ordered_names_list_of_fields fields in
  List.iter
    (fun x_name ->
      let ill_f = left_triangle ~current_species x_name x_name fields in
      if ill_f then raise (Ill_formed_species current_species))
    names
;;



(*
(* Was debug. To disapear. *)
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
*)

