(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                                                                     *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007, 2008 LIP6 and INRIA                                *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: visUniverse.ml,v 1.9 2008-12-09 12:51:58 pessaux Exp $ *)

(* ******************************************************************** *)
(** {b Descr} : Describes how a method arrives into a visible universe.
    Either by a decl-dependency and NO transitive def-dependency. Or by
    at least a transitive def-dependency (in this case, no matter if it
    also arrives thanks a decl-dependency.

    {b Rem} : Not exported outside this module.                         *)
(* ******************************************************************** *)
type in_the_universe_because =
  | IU_only_decl   (** The method arrives in the visible universe by the
                       presence of only a decl-dependency and NO transitive
                       def-dependency. *)
  | IU_trans_def   (** The method arrives in the visible universe by the
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

    {b Rem} : Not exported outside this module.                          *)
(* ********************************************************************* *)
module UniverseElem = struct
  type t = Parsetree.vname
  let compare = compare
end ;;
module Universe = Map.Make(UniverseElem) ;;



(* ************************************************************************* *)
(* with_def_deps_n_term_pr : bool (Dep_analysis.name_node * 'a) list ->      *)
(*   (Dep_analysis.name_node * 'b) list ->                                   *)
(*     in_the_universe_because Universe.t                                    *)
(** {b Descr} : Computes the visible universe of a species field whose decl,
    def-dependencies and fields are given as argument.
    This function works according to the definition 57 page 116 section
    6.4.4 in Virgile Prevosto's Phg.
    To be usable for OCaml generation, the [with_def_deps_n_term_pr] flag
    enables to forget the def-dependencies and their implied transitive
    decl-dependencies and the dependencies induced from recursive functions
    termination proofs. In effect, in OCaml, only decl-dependencies are
    relevant and since there is no termination proof, dependencies induced
    by them must be forgotten.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let visible_universe ~with_def_deps_n_term_pr dep_graph x_decl_dependencies
    x_def_dependencies =
  (* First, apply rule 1. Because decl-dependencies are already computed
     when computing the visible universe, just take them as parameter instead
     of computing them again. We add each method with the tag telling that it
     comes here thanks to a decl-dependency. If it appears later to also come
     thank to a transitive def-dependency, then it will be removed and
     changed to that def tag in the universe. *)
  let universe = ref Universe.empty in
  (* If we are in coq case, then we also take into account the dependencies
     induced by recursive functions termination proofs. *)
  if with_def_deps_n_term_pr then
    (begin
    List.iter
      (fun (n, _) ->
        universe := Universe.add n.DepGraphData.nn_name IU_only_decl !universe)
      x_decl_dependencies
    end)
  else
    (begin
    (* Otherwise, we are only interested in decl-dependencies that are not
       induced by recursive functions termination proofs. *)
    List.iter
      (fun (n, dep_kind) ->
        match dep_kind with
         | DepGraphData.DK_def _ ->
             assert false (* Should always be a decl-dependency ! *)
         | DepGraphData.DK_decl DepGraphData.DcDK_from_term_proof ->
	     (* For termination proofs we need to take these dependencies
		into account. *)
	     if with_def_deps_n_term_pr then
	       universe :=
		 Universe.add n.DepGraphData.nn_name IU_only_decl !universe
         | DepGraphData.DK_decl _ ->
             universe :=
               Universe.add n.DepGraphData.nn_name IU_only_decl !universe)
      x_decl_dependencies
    end) ;
  (* We take def-dependencies and their transitive implied decl-dependencies
     only if requested. And in this case, we also take def-dependencies coming
     from the recursive functions termination proofs. *)
  if with_def_deps_n_term_pr then
    (begin
    (* Next, apply rule 2 and 3. Add the def-dependencies. Like
       decl-dependencies,  they are already available, so take them as a
       parameter instead of computing them again. For each of them we
       follow the transitive links to add the transitive def-dependencies.
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
      if not (Parsetree_utils.VnameSet.mem n.DepGraphData.nn_name !seen) then
        (begin
        (* Mark it as seen. *)
        seen := Parsetree_utils.VnameSet.add n.DepGraphData.nn_name !seen ;
        (* Add the node that has def-dependency to the universe. If the
           method already appeared with only the decl tag, then it gets
           cleared and replaced with the tag meaning that this method comes
           here thanks to a transitive def-dependency. *)
        universe := Universe.add n.DepGraphData.nn_name IU_trans_def !universe ;
        (* Add the decl-dependencies of this node to the universe. *)
        List.iter
          (function
            | (child_node, (DepGraphData.DK_decl _)) ->
                (begin
                (* If the method already appeared with the tag meaning that is
                   comes here thanks to a transitive def-dep, let it unchanged,
                   otherwise add it with the tag meaning that it come here
                   thanks to a decl-dep.
                   In fact the process is simpler: if the method already
                   appears in the universe: either it's with a transitive
                   def-dep, and then no change to do.
                   Or it's with de decl-dep tag, and in this case, it's
                   useless to add it again with this tag: then also no change
                   to do. *)
                if not
                    (Universe.mem
                       child_node.DepGraphData.nn_name !universe) then
                  universe :=
                    Universe.add
                      child_node.DepGraphData.nn_name IU_only_decl !universe
                end)
            | (_, (DepGraphData.DK_def _)) -> ())
          n.DepGraphData.nn_children ;
        (* Recurse on each def-pedendency child of the current node. *)
        List.iter
          (function
            | (child_node, (DepGraphData.DK_def _)) ->
                (* Now recurse to walk deeper in the graph on def-dependency
                   children only. *)
                transitive_addition child_node
            | (_, (DepGraphData.DK_decl _)) -> ())
          n.DepGraphData.nn_children
        end) in
    (* Now, start the transitive hunt for each initial def-dependencies
       nodes. *)
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
        (fun z_name _ ->    (* [z_name] is "z" in definition 57 of the Phd. *)
          (* ... Find its node in the graph (must never fail because the graph
             is already built and is built from the species fields)... *)
          let z_node =
            (try
              List.find
                (fun node -> node.DepGraphData.nn_name = z_name) dep_graph
            with Not_found -> assert false) in
          (* ... For each dependency of the node... *)
          List.iter
            (function
              | (child_node,
                 (DepGraphData.DK_decl DepGraphData.DcDK_from_type)) ->
                  (* ... Only consider those tagged as coming from the "type"
                     of the field having this mode.
                     If the child is not already in universe, then add it
                     with a [IU_only_decl] tag. If it is already in the
                     universe, then do nothing. In effect, either it is inside
                     with a [IU_only_decl] tag and then it is useless to add
                     it again.
                     Or it is inside with a [IU_trans_def] and this flag has a
                     higher priority and must not be changed. *)
                  if not
                      (Universe.mem child_node.DepGraphData.nn_name !universe)
                  then
                    (begin
                    universe :=
                      Universe.add
                        child_node.DepGraphData.nn_name IU_only_decl !universe ;
                    (* Mark that we must continue the fixpoint. *)
                    continue := true ;
                    end)
              | (_, _) -> ())
            z_node.DepGraphData.nn_children)
        !universe
    done
    end) ;
  (* Finally, return the visible universe. *)
  !universe
;;
