(***********************************************************************)
(*                                                                     *)
(*                        FoCaLiZe compiler                            *)
(*                                                                     *)
(*            François Pessaux                                         *)
(*                                                                     *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: infoOutput.ml,v 1.1 2012-02-21 17:27:08 pessaux Exp $ *)


(* ************************************************************************** *)
(** {b Descr}: Generate a plain text history explaining for each method of the
    species where does it come step by step by inheritance. One text file is
    generated per species of the compilation unit.

    {b Args}:
     - [dirname]: The directory name where to store the generated text files.
     - [current_species]: The [qualified_species] name of the currently
       processed species.
     - [methods]: The list of information on the methods contained in the
       species (assumed to be in normal form of course).

   {b Return}: None                                                           *)
(* ************************************************************************** *)
let methods_history_to_text ~dirname ~current_species methods =
  (* For each species, a file named with "history_", the species name and the
     suffix ".txt" will be generated in the directory. *)
  let (current_species_module, current_species_vname) = current_species in
  let out_filename =
    Filename.concat
      dirname
      ("history_" ^ current_species_module ^ "_" ^
       (Parsetree_utils.name_of_vname current_species_vname) ^ ".txt") in
  let out_hd = open_out_bin out_filename in
  let out_ppf = Format.formatter_of_out_channel out_hd in
  (* Just a local function to process printing of one method. Will be handy to
     iterate on methods of a [SF_let_rec] field. *)
  let process_on_method from n =
    Format.fprintf out_ppf "** Method '%a':@\n" Sourcify.pp_vname n;
    Format.fprintf out_ppf "\tInitially appearing in species '%a'@\n"
      Sourcify.pp_qualified_species from.Env.fh_initial_apparition;
    List.iter
      (fun (inherited_from, by_expression, _) ->
        Format.fprintf out_ppf "\tAppearing by inheritance in species '%a'@\n"
          Sourcify.pp_qualified_species inherited_from;
        Format.fprintf out_ppf "\t\tVia expression '%a'@\n"
          Sourcify.pp_simple_species_expr by_expression)
      from.Env.fh_inherited_along in
  (* Now, dump information for each field of the species, starting by
     information about ourselves, then closest parent first. This means that
     reading the text top-down, we "go back in the past". *)
  List.iter
    (function
      | Env.TypeInformation.SF_sig (from, n, _)
      | Env.TypeInformation.SF_let (from, n, _, _, _, _, _, _)
      | Env.TypeInformation.SF_theorem (from, n, _, _, _, _)
      | Env.TypeInformation.SF_property (from, n, _, _, _) ->
          process_on_method from n
      | Env.TypeInformation.SF_let_rec (_, l) ->
          List.iter
            (fun (from, n, _, _, _, _, _, _) -> process_on_method from n)
            l)
    methods;
  close_out out_hd
;;



let seen_methods_per_species = ref Parsetree_utils.Qualified_speciesMap.empty ;;



(* ************************************************************************** *)
(** {b Descr}: Generate a plain text in Graphwiz format (AKA dotty) showing
    for each method of the species where does it come step by inheritance.
    Species are represented a a big box containing its methods and an arrow from
    such a method toward the method of the same name but in another species
    indicates that the first one is inherited from the second one.
    One text file is generated per species of the compilation unit.

    {b Args}:
     - [dirname]: The directory name where to store the generated text files.
     - [current_species]: The [qualified_species] name of the currently
       processed species.
     - [methods]: The list of information on the methods contained in the
       species (assumed to be in normal form of course).

   {b Return}: None                                                           *)
(* ************************************************************************** *)
let methods_history_to_dot ~dirname ~current_species methods =
  (* For each species, a file named with "history_", the species name and the
     suffix ".dot" will be generated in the directory. *)
  let (current_species_module, current_species_vname) = current_species in
  let out_filename =
    Filename.concat
      dirname
      ("history_" ^ current_species_module ^ "_" ^
       (Parsetree_utils.name_of_vname current_species_vname) ^ ".dot") in
  let out_hd = open_out_bin out_filename in
  let out_ppf = Format.formatter_of_out_channel out_hd in


  (* Just a local function to add a method to a species bucket or create it
     in the memo table. *)
  let add_seen_method_for_species species_name method_name =
    (* Search for the hosting species bucket in the memo table. If no method
       was recorded for this species, i.e. if there is no bucket for this
       species, an empty set of vnames. *)
    let needed_methods =
      (try
        Parsetree_utils.Qualified_speciesMap.find
          species_name !seen_methods_per_species
      with Not_found -> Parsetree_utils.VnameSet.empty) in
    let needed_methods' =
      Parsetree_utils.VnameSet.add method_name needed_methods in
    (* Put back this new set of seen methods for the species. *)
    seen_methods_per_species :=
      Parsetree_utils.Qualified_speciesMap.add
        species_name needed_methods' !seen_methods_per_species in

  (* Just a local function to process recording the all the hosting species of
    one method. Will be handy to iterate on methods of a [SF_let_rec] field. *)
  let process_on_method from n =
    (* Process the initial apparition. *)
    add_seen_method_for_species from.Env.fh_initial_apparition n ;
    List.iter
      (fun (inherited_from, _, _) ->
        add_seen_method_for_species inherited_from n)
      from.Env.fh_inherited_along in

  (* Just a local function to process drawing the inheritance arrows between
     corresponding dot nodes representing from which species a method comes. 
     Will be handy to iterate on methods of a [SF_let_rec] field. *)
  let process_on_method2 from n =
    let printable_meth_name =
      Parsetree_utils.vname_as_string_with_operators_expanded n in
    (* Reming where the next arrow arrow to draw must end. At the first time,
       it is the species hosting the initial apparition. *)
    let arrow_end = ref from.Env.fh_initial_apparition in
    List.iter
      (fun (inherited_from, _, _) ->
        (* Source of the arrow. *)
        let (src_mod_name, src_meth_name) = inherited_from in
        (* End of the arrow.*)
        let (dest_mod_name, dest_meth_name) = !arrow_end in
        Format.fprintf out_ppf "  dot_%s__hash__%s:%s -> dot_%s__hash__%s:%s\n"
          src_mod_name
          (Parsetree_utils.vname_as_string_with_operators_expanded
             src_meth_name)
          printable_meth_name
          dest_mod_name
          (Parsetree_utils.vname_as_string_with_operators_expanded
             dest_meth_name)
          printable_meth_name ;
        (* Remind the new end of the future arrow. *)
        arrow_end := inherited_from)
      (* Don't forget to reverse the list, otherwise, we will process the
         inheritance in wrong order ! We want to start processing the oldest
         species first ! *)
      (List.rev from.Env.fh_inherited_along) in

  (* Really do the job. First clean the memo table. *)
  seen_methods_per_species := Parsetree_utils.Qualified_speciesMap.empty ;
  (* Dump the graphwiz header. *)
  Format.fprintf out_ppf "digraph G {\n  node [shape=record];\nrankdir=LR\n" ;
  (* Now, register information for each field of the species, starting by
     information about ourselves, then closest parent first. At the end, the
     memo table contains as many species as methods come from, and for each
     species, the effectively used methods. *)
  List.iter
    (function
      | Env.TypeInformation.SF_sig (from, n, _)
      | Env.TypeInformation.SF_let (from, n, _, _, _, _, _, _)
      | Env.TypeInformation.SF_theorem (from, n, _, _, _, _)
      | Env.TypeInformation.SF_property (from, n, _, _, _) ->
          process_on_method from n
      | Env.TypeInformation.SF_let_rec (_, l) ->
          List.iter
            (fun (from, n, _, _, _, _, _, _) -> process_on_method from n)
            l)
    methods ;
  (* This way, we can now define the shape of the graphwiz nodes, using
     "records". So, start by generating all the nodes, i.e. tables representing
     species with their interesting methods as entries. *)
  Parsetree_utils.Qualified_speciesMap.iter
    (fun species_qual_name methods ->
      let (species_modname, species_vname) = species_qual_name in
      (* Name the node using the full qualified name of the species using
         "__hash__" in place of the "#" character. First slot of the record is
         the name of the species. *)
      Format.fprintf out_ppf "  dot_%s__hash__%s [label=\"%a"
        species_modname
        (Parsetree_utils.vname_as_string_with_operators_expanded
           species_vname)
        Sourcify.pp_qualified_species species_qual_name ;
      (* Now create the entries of the species, i.e. its relevant methods. *)
      Parsetree_utils.VnameSet.iter
        (fun meth_vname ->
          let meth_name =
            Parsetree_utils.vname_as_string_with_operators_expanded
              meth_vname in
          (* The ident of the label escapes operators present in the name of
             the method although the really displayed text doesn't. *)
          Format.fprintf out_ppf "|<%s> %a"
            meth_name Sourcify.pp_vname meth_vname)
      methods ;
      Format.fprintf out_ppf "\"];\n")
    !seen_methods_per_species ;
  (* Now, it's time to generate the arrows representing inheritance. By
     convention we decide that the arrow points toward the parent. *)
  List.iter
    (function
      | Env.TypeInformation.SF_sig (from, n, _)
      | Env.TypeInformation.SF_let (from, n, _, _, _, _, _, _)
      | Env.TypeInformation.SF_theorem (from, n, _, _, _, _)
      | Env.TypeInformation.SF_property (from, n, _, _, _) ->
          process_on_method2 from n
      | Env.TypeInformation.SF_let_rec (_, l) ->
          List.iter
            (fun (from, n, _, _, _, _, _, _) -> process_on_method2 from n)
            l)
    methods ;
  (* Close the graphwiz file. *)
  Format.fprintf out_ppf "}\n" ;
  close_out out_hd
;;
