(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            François Pessaux                                                *)
(*            Pierre Weis                                                     *)
(*            Damien Doligez                                                  *)
(*                                                                            *)
(*               LIP6  --  INRIA Rocquencourt -- ENSTA ParisTech              *)
(*                                                                            *)
(*  Copyright 2007 - ...  LIP6 and INRIA                                      *)
(*            2012 - ... ENSTA ParisTech                                      *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)


(* ******************************************************************** *)
(** {b Descr} : Describes how a method arrives into a visible universe.
    Either by a decl-dependency and NO transitive def-dependency. Or by
    at least a transitive def-dependency (in this case, no matter if it
    also arrives thanks to a decl-dependency).

    {b Exported} : Yes.                                                 *)
(* ******************************************************************** *)
type in_the_universe_because =
  | IU_decl_comput   (** The method arrives in the visible universe by the
                         presence of only a decl-dependency in a computational
                         related method and NO transitive def-dependency. *)
  | IU_decl_logic
  | IU_trans_def    (** The method arrives in the visible universe by the
                        presence at least of a transitive def-dependency
                        (no matter whether the also is a decl-dependency in
                        this case). *)
;;



(* ********************************************************************* *)
(** {b Descr} : Structure of a visible universe. It's a [Map] linking
    a method name with the reason how this method arrives in the visible
    universe.
    Building the visible universe with this information ease the
    computation of the \inter\smallinter operation of Virgile Prevosto's
    Phd, page 116, definition 58, section 6.4.4. In effect, with the
    reason of why the method is in the universe reminded, the choice of
    rule 3 or 4 is immediate instead of having to look again if the
    method was a introduced my a decl ou a transitive-def dependency.

    {b Exported} : Yes.                                                  *)
(* ********************************************************************* *)
module UniverseElem = struct
  type t = Parsetree.vname
  let compare = compare
end ;;
module Universe = Map.Make(UniverseElem) ;;



(* ************************************************************************* *)
(* (Dep_analysis.name_node * 'a) list ->                                     *)
(*   (Dep_analysis.name_node * 'b) list ->                                   *)
(*     in_the_universe_because Universe.t                                    *)
(** {b Descr} : Computes the visible universe of a species field whose decl,
    def-dependencies and fields are given as argument.
    This function works according to the definition 57 page 116 section
    6.4.4 in Virgile Prevosto's Phd.
    The representation is processed like other methods, not special stuff.
    Its name is simply "rep".

    {b Exported} : Yes.                                                      *)
(* ************************************************************************* *)
let visible_universe dep_graph x_decl_dependencies x_def_dependencies =
  (* First, apply rule 1. Because decl-dependencies are already computed
     when computing the visible universe, just take them as parameter instead
     of computing them again. We add each method with the tag telling that it
     comes here thanks to a decl-dependency. If it appears later to also come
     thank to a transitive def-dependency, then it will be removed and
     changed to that def tag in the universe. *)
  let universe = ref Universe.empty in
  List.iter
    (fun (n, dep_kind) ->
      match dep_kind with
      | DepGraphData.DK_def _ ->
          assert false (* Should always be a decl-dependency ! *)
      | DepGraphData.DK_decl DepGraphData.DcDK_from_type_logic
      | DepGraphData.DK_decl DepGraphData.DcDK_from_body_logic ->
          universe :=
            Universe.add n.DepGraphData.nn_name IU_decl_logic !universe
      | DepGraphData.DK_decl DepGraphData.DcDK_from_type_comput
      | DepGraphData.DK_decl DepGraphData.DcDK_from_body_comput ->
          universe :=
            Universe.add n.DepGraphData.nn_name IU_decl_comput !universe)
    x_decl_dependencies ;
  (* Next, apply rule 2 and 3. Add the def-dependencies. Like
     decl-dependencies, they are already available, so take them as a
     parameter instead of computing them again. For each of them we follow
     the transitive links to add the transitive def-dependencies.
     Rule 3 is implemented by adding for each transitive def-dependency
     node, its decl-dependencies. *)
  (* First, create the set of already visited nodes. *)
  let seen = ref Parsetree_utils.VnameSet.empty in
  (* ************************************************************ *)
  (* transitive_addition : name_node -> unit                      *)
  (** {b Descr} : The local recursive function that will walk the
      dependencies graph to hunt transitive def-dependencies.
      It also add the decl-dependencies for each def-dependency
      found. This way, one unique walk is needed.                 *)
  (* ************************************************************ *)
  let rec transitive_addition n =
    if not (Parsetree_utils.VnameSet.mem n.DepGraphData.nn_name !seen) then (
      (* Mark it as seen. *)
      seen := Parsetree_utils.VnameSet.add n.DepGraphData.nn_name !seen ;
      (* Add the node that has def-dependency to the universe. If the
         method already appeared with only the decl tag, then it gets
         cleared and replaced with the tag meaning that this method comes
         here thanks to a transitive def-dependency. *)
      universe := Universe.add n.DepGraphData.nn_name IU_trans_def !universe ;
      (* Add the decl-dependencies of this node to the universe. Since they
         are introduced by a def-dependency and this latter can only
         arise through a logical method, added methods will be added
         as IU_decl_logic except if already present as IU_decl_comput
         or IU_trans_def. *)
      List.iter
        (function
          | (child_node, (DepGraphData.DK_decl _)) -> (
              (* If the method already appeared with the tag meaning that is
                 comes here thanks to a transitive def-dep, let it unchanged.
                 Otherwise if it already appeared with the tag meaning that is
                 comes here thanks to a decl-computational, leave it
                 unchanged. Otherwise add it as coming through a logical.
                 In fact the process is simpler: if the method already
                 appears in the universe: either it's with a transitive
                 def-dep, or a decl-comput and then no change to do.
                 Or it's already with a decl-logic, hence no need to add
                 it again. *)
              if not
                  (Universe.mem
                     child_node.DepGraphData.nn_name !universe) then
                universe :=
                  Universe.add
                    child_node.DepGraphData.nn_name IU_decl_logic !universe)
          | (_, (DepGraphData.DK_def _)) -> ())
        n.DepGraphData.nn_children ;
      (* Recurse on each def-dependency child of the current node. *)
      List.iter
        (function
          | (child_node, (DepGraphData.DK_def _)) ->
              (* Now recurse to walk deeper in the graph on def-dependency
                 children only. *)
              transitive_addition child_node
          | (_, (DepGraphData.DK_decl _)) -> ())
        n.DepGraphData.nn_children
     ) in
  (* Now, start the transitive hunt for each initial def-dependencies nodes. *)
  List.iter
    (fun (def_node, _) -> transitive_addition def_node)
    x_def_dependencies ;
  (* Now, compute the fixpoint for rule 4. This rule only takes into account
     decl-dependencies of the "type" of a field. This is equivalent to only
     use the "decl"-dependencies tagged [DDK_from_type]. *)
  let continue = ref true in
  while !continue do
    (* Reset the fixpoint end's flag. *)
    continue := false ;
    (* For each member of the universe... *)
    Universe.iter
      (fun z_name z_reason ->
        (* [z_name] is "z" in definition 57 of the Phd. *)
        (* ... Find its node in the graph (must never fail because the graph
           is already built and is built from the species fields)... *)
        let z_node =
          (try
            List.find (fun node -> node.DepGraphData.nn_name = z_name) dep_graph
          with Not_found -> assert false) in
        (* ... For each dependency of the node... *)
        List.iter
          (function
            | (child_node,
               (DepGraphData.DK_decl
                  ((DepGraphData.DcDK_from_type_comput |
                   DepGraphData.DcDK_from_type_logic)))) -> (
                 (* ... Only consider those tagged as coming from the "type"
                    of the field having this mode. *)
                 try
                   (* If it is already in the universe ... *)
                   (match Universe.find
                       child_node.DepGraphData.nn_name !universe with
                   | IU_decl_logic ->
                       (* If the dependency must be added because z was
                          brought by computational reason, then it is
                          stronger than the reason decl-logic we just found
                          here, then override. Otherwise, do nothing, it is
                          already in the universe with the good reason. *)
                       if z_reason = IU_decl_comput then (
                         universe :=
                           Universe.add
                             child_node.DepGraphData.nn_name IU_decl_comput
                             !universe ;
                         (* Mark that we must continue the fixpoint. *)
                         continue := true
                        )
                   | IU_trans_def | IU_decl_comput ->
                       (* Nothing to do since already in the universe by
                          stronger reason: because of def-dep or because
                          decl-comput which is stronger than decl-logic. *)
                       ())
                 with Not_found ->
                   (* If the child is not already in universe, then add it
                      with the tag depending on the one of [z_reason].
                      The new dependency must be added as decl-logic if they
                      are due to an initial z's def-dep (which is forcibly
                      logical related) or an initial z's logic-decl-dep.
                      Otherwise, it is related to decl-comput. *)
                   let add_with_reason =
                     (match z_reason with
                     | IU_decl_comput -> IU_decl_comput
                     | IU_decl_logic | IU_trans_def -> IU_decl_logic) in
                   universe :=
                     Universe.add
                       child_node.DepGraphData.nn_name
                       add_with_reason !universe ;
                   (* Mark that we must continue the fixpoint. *)
                   continue := true
                )
            | (_, _) -> ())
          z_node.DepGraphData.nn_children)
      !universe
  done ;
  (* Finally, return the visible universe. *)
  !universe
;;
