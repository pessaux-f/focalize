(***********************************************************************)
(*                                                                     *)
(*                        FoCaLiZe compiler                            *)
(*                                                                     *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                                                                     *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: utils_docgen.ml,v 1.5 2012-10-12 13:17:04 pessaux Exp $ *)



(* *********************************************************************** *)
(** {b Descr}: Translates a string into a XML compliant string by escaping
     characters according to XML lexical conventions.
     The input string is NOT modified in place. We alwaus return a fresh
     string.

    {b Rem} : Exported outside this module.                                *)
(* *********************************************************************** *)
let xmlify_string s =
  let s_len = String.length s in
  let result = ref "" in
  let i = ref 0 in
  while !i < s_len do
    (match s.[!i] with
     | '<' -> result := !result ^ "&lt;"
     | '>' -> result := !result ^ "&gt;"
     | '&' -> result := !result ^ "&amp;"
     | _ -> result := !result ^ (String.make 1 s.[!i]));
    incr i
  done;
  !result
;;



let pp_xml_vname ppf = function
  | Parsetree.Vlident s | Parsetree.Vuident s | Parsetree.Vpident s
  | Parsetree.Viident s | Parsetree.Vqident s ->
      Format.fprintf ppf "%s" (xmlify_string s)
;;



let get_in_file_and_name_from_ident ~current_unit ident =
  match ident.Parsetree.ast_desc with
   | Parsetree.I_local vname
   | Parsetree.I_global (Parsetree.Vname vname) ->
       ("", vname)
   | Parsetree.I_global (Parsetree.Qualified (mod_name, vname)) ->
       if mod_name = current_unit then ("", vname)
       else (mod_name, vname)
;;



(* ************************************************************************ *)
(** {b Descr}: Find amongst the fields of the species definition (i.e. the
    structure representing the species in the AST) the one wearing the name
    [meth_name] and get its documentation.
    This is used to generate the documentation of a method since in the
    [please_compile_me]s (on which we iterate the documentation generation
    process) we do not have the comments. So we need to recover them for
    each [please_compile_me] via the original AST structure of the species
    fields.

    {b Rem}: Exported outside this module.                                  *)
(* ************************************************************************ *)
let find_annotation_of_method meth_name species_def_fields =
  (* Local function to perform search among the [binding]s of a [SF_let] in
     order to find if a binding has the searched name. *)
  let rec find_in_bindings = function
    | [] -> false
    | h :: q ->
        if h.Parsetree.ast_desc.Parsetree.b_name = meth_name then true
        else find_in_bindings q in
  (* Local function to perform search among the whole list of fields. *)
  let rec rec_find = function
    | [] ->
        (* If the method is not agmonst the species definition fields that's
           because the method is inherited (either we the method is inherited
           in a regular way by the "inherits" clause or we are called on a
           collection, hence the inheritance is shadow under the "implements"
           clause. So that's normal not to find it.
           We then return an empty documentation, i.e/ an empty list. *)
        []
    | h :: q ->
        (begin
        match h.Parsetree.ast_desc with
         | Parsetree.SF_rep rep_type_def ->
             if meth_name = (Parsetree.Vlident "rep") then
               rep_type_def.Parsetree.ast_annot
             else rec_find q
         | Parsetree.SF_sig sig_def ->
             if sig_def.Parsetree.ast_desc.Parsetree.sig_name = meth_name then
               sig_def.Parsetree.ast_annot
             else rec_find q
         | Parsetree.SF_let let_def ->
             (begin
             (* It appears that the parsed comment of a "let" definition is
                hoocked on the [let_def] node. *)
             (* TODO: This means that the principle of focdoc is wrong in case
                of multiple functions binding since we will have only 1 comment
                for all the defined functions. There is something to think
                again... *)
             if find_in_bindings
                 let_def.Parsetree.ast_desc.Parsetree.ld_bindings then
               let_def.Parsetree.ast_annot
             else rec_find q
             end)
         | Parsetree.SF_property property_def ->
             if property_def.Parsetree.ast_desc.Parsetree.prd_name = meth_name
             then
               property_def.Parsetree.ast_annot
             else rec_find q
         | Parsetree.SF_theorem theorem_def ->
             if theorem_def.Parsetree.ast_desc.Parsetree.th_name = meth_name
             then
               theorem_def.Parsetree.ast_annot
             else rec_find q
         | Parsetree.SF_proof proof_def ->
             if proof_def.Parsetree.ast_desc.Parsetree.pd_name = meth_name then
               proof_def.Parsetree.ast_annot
             else rec_find q
         | Parsetree.SF_termination_proof term_proof_def ->
             let found_among_profiles =
               Handy.list_mem_custom_eq
                 (fun profile n ->
                   profile.Parsetree.ast_desc.Parsetree.tpp_name = n)
                 meth_name
                 term_proof_def.Parsetree.ast_desc.Parsetree.tpd_profiles in
             if found_among_profiles then h.Parsetree.ast_annot
             else rec_find q
        end) in
  (* ********************** *)
  (* Now, let's do the job. *)
  rec_find species_def_fields
;;



(* ************************************************************************** *)
(* Parsetree.annot_elem list ->                                                 *)
(*   ((string option * string option * string option *                        *)
(*    (string option * string option * string option))                        *)
(** {b Descr}: Searches the string related to all documentation tags known in
    FoCaL.
    If several occurences of a same tag are found, their related texts are
    concatened together in their apparition order and separated by a newline.

    {b Rem}: Exported outside this module.                                    *)
(* ************************************************************************** *)
let extract_tagged_info_from_annotation annot_elems =
  let found_title = ref None in
  let found_author = ref None in
  let found_description = ref None in
  let found_mathml = ref None in
  let found_latex = ref None in
  let found_untagged = ref None in
  (* A local function to search the tags in an AST node. *)
  List.iter
    (function
     | { Parsetree.ae_tag = ("" | "FoCaLize"); Parsetree.ae_desc = s ; _ } ->
       let lexbuf = Lexing.from_string s in
       let continue = ref true in
       (* We lex ther string until we reach its end, i.e. until we get a
          non-tagged string being empty (i.e. ""). *)
       while !continue do
         match Doc_lexer.start lexbuf with
          | Doc_lexer.DT_Author s ->
              (match !found_author with
               | None -> found_author := Some s
               | Some old -> found_author := Some (old ^ "\n" ^ s))
          | Doc_lexer.DT_Title s ->
              (match !found_title with
               | None -> found_title := Some s
               | Some old -> found_title := Some (old ^ "\n" ^ s))
          | Doc_lexer.DT_Description s ->
              (match !found_description with
               | None -> found_description := Some s
               | Some old -> found_description := Some (old ^ "\n" ^ s))
          | Doc_lexer.DT_MathMl s ->
              (match !found_mathml with
               | None -> found_mathml := Some s
               | Some old -> found_mathml := Some (old ^ "\n" ^ s))
          | Doc_lexer.DT_LaTeX s ->
              (match !found_latex with
               | None -> found_latex := Some s
               | Some old -> found_latex := Some (old ^ "\n" ^ s))
          | Doc_lexer.DT_None "" -> continue := false
          | Doc_lexer.DT_None s ->
              (match !found_untagged with
               | None -> found_untagged := Some s
               | Some old -> found_untagged := Some (old ^ "\n" ^ s))
       done
     (* Not our stuff. *)
     | _ -> ())
    annot_elems;
  (!found_title, !found_author, !found_description, !found_mathml,
   !found_latex, !found_untagged)
;;



(* ************************************************************************** *)
(* Parsetree.file_desc Parsetree.ast ->                                       *)
(*   (string option * string option * string option)                          *)
(** {b Descr}: Searches the string related to the 3 documentation tags
    "@title ", "@author ", "@description ". These 3 tags are used to generate
    the header of the XML file.
    Since these 3 tags may appear in a documentation that is not attached to
    the toplevel node of the AST (for instance if the source file starts with
    a comment looking like a documentation because it is a sequence of stars,
    exactly like we did in the comment of this function), we also search in
    the documentation attached to the first definition of the source file.

    {b Rem}: Exported outside this module.                                    *)
(* ************************************************************************** *)
let find_title_author_and_description ast_root =
  (* Do the job on the top node of the AST. We lex the documentation and
     look at the informations we found inside. *)
  let (found_title, found_author, found_description, _, _, _) =
    extract_tagged_info_from_annotation ast_root.Parsetree.ast_annot in
  (* Then do the same thing on the AST node of the first definition of the
     source file. *)
  match ast_root.Parsetree.ast_desc with
   | Parsetree.File [] ->
       (* So, the source file is empty (with no definition). Nothing new and
          be happy with what we may have found before on the toplevel AST
          node. *)
       (found_title, found_author, found_description)
   | Parsetree.File (first :: _) ->
       let (found_title', found_author', found_description', _, _, _) =
         extract_tagged_info_from_annotation first.Parsetree.ast_annot in
       (* Now, just return what we found, always prefering the items found
          in the first AST node. *)
       let final_title =
         (match found_title with
          | None -> found_title' | Some _ -> found_title) in
       let final_author =
         (match found_author with
          | None -> found_author' | Some _ -> found_author) in
       let final_description =
         (match found_description with
          | None -> found_description' | Some _ -> found_description) in
       (final_title, final_author, final_description)
;;
