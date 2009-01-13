(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*            François Pessaux                                         *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: main_docgen.ml,v 1.24 2009-01-13 12:33:12 pessaux Exp $ *)



(* *********************************************************************** *)
(** {b Descr}: Translates a string into a XML compliant string by escaping
     characters according to XML lexical conventions.
     The input string is NOT modified in place. We alwaus return a fresh
     string.

    {b Rem} : Not exported outside this module.                            *)
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
     | _ -> result := !result ^ (String.make 1 s.[!i])) ;
    incr i
  done ;
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

    {b Rem}: Not exported outside this module.                              *)
(* ************************************************************************ *)
let find_documentation_of_method meth_name species_def_fields =
  (* Local function to perform search among the [binding]s of a [SF_let]. *)
  let rec find_in_bindings = function
    | [] -> None
    | h :: q ->
        if h.Parsetree.ast_desc.Parsetree.b_name = meth_name then
          Some h.Parsetree.ast_doc
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
               rep_type_def.Parsetree.ast_doc
             else rec_find q
         | Parsetree.SF_sig sig_def ->
             if sig_def.Parsetree.ast_desc.Parsetree.sig_name = meth_name then
               sig_def.Parsetree.ast_doc
             else rec_find q
         | Parsetree.SF_let let_def ->
             (begin
             match
                find_in_bindings
                  let_def.Parsetree.ast_desc.Parsetree.ld_bindings with
              | Some doc -> doc
              | None -> rec_find q
             end)
         | Parsetree.SF_property property_def ->
             if property_def.Parsetree.ast_desc.Parsetree.prd_name = meth_name
             then
               property_def.Parsetree.ast_doc
             else rec_find q
         | Parsetree.SF_theorem theorem_def ->
             if theorem_def.Parsetree.ast_desc.Parsetree.th_name = meth_name
             then
               theorem_def.Parsetree.ast_doc
             else rec_find q
         | Parsetree.SF_proof proof_def ->
             if proof_def.Parsetree.ast_desc.Parsetree.pd_name = meth_name then
               proof_def.Parsetree.ast_doc
             else rec_find q
         | Parsetree.SF_termination_proof term_proof_def ->
             let found_among_profiles =
               Handy.list_mem_custom_eq
                 (fun profile n ->
                   profile.Parsetree.ast_desc.Parsetree.tpp_name = n)
                 meth_name
                 term_proof_def.Parsetree.ast_desc.Parsetree.tpd_profiles in
             if found_among_profiles then h.Parsetree.ast_doc
             else rec_find q
        end) in
  (* ********************** *)
  (* Now, let's do the job. *)
  rec_find species_def_fields
;;



(* ************************************************************************** *)
(* Parsetree.doc_elem list ->                                                 *)
(*   ((string option * string option * string option *                        *)
(*    (string option * string option * string option))                        *)
(** {b Descr}: Searches the string related to all documentation tags known in
    FoCaL.
    If several occurences of a same tag are found, their related texts are
    concatened together in their apparition order and separated by a newline.
    {b Rem}: Not exported outside this module.                                *)
(* ************************************************************************** *)
let extract_tagged_info_from_documentation doc_elems =
  let found_title = ref None in
  let found_author = ref None in
  let found_description = ref None in
  let found_mathml = ref None in
  let found_latex = ref None in
  let found_untagged = ref None in
  (* A local function to search the tags in an AST node. *)
  List.iter
    (fun { Parsetree.de_desc = s } ->
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
      done)
    doc_elems ;
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

    {b Rem}: Not exported outside this module.                                *)
(* ************************************************************************** *)
let find_title_author_and_description ast_root =
  (* Do the job on the top node of the AST. We lex the documentation and
     look at the informations we found inside. *)
  let (found_title, found_author, found_description, _, _, _) =
    extract_tagged_info_from_documentation ast_root.Parsetree.ast_doc in
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
         extract_tagged_info_from_documentation first.Parsetree.ast_doc in
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



(* ********************************************************************** *)
(* Format.formatter -> string option -> string option -> string option -> *)
(*   Parsetree.doc_elem list -> unit                                      *)
(* {b Descr}: Generates the "<foc:information>" section from the optional
    values for each element of "<foc:information>" in the DTD.

   {b Rem}: Not exported outside this module.                             *)
(* ********************************************************************** *)
let gen_doc_foc_informations out_fmt name_opt math_opt latex_opt comments =
  Format.fprintf out_fmt "@[<h 2><foc:informations>@\n" ;
  (match name_opt with
   | None -> ()
   | Some s -> Format.fprintf out_fmt "<foc:name>%s</foc:name>@\n" s) ;
  (match math_opt with
   | None -> ()
   | Some s -> Format.fprintf out_fmt "<foc:math>%s</foc:math>@\n" s) ;
  (match latex_opt with
   | None -> ()
   | Some s -> Format.fprintf out_fmt "<foc:latex>%s</foc:latex>@\n" s) ;
  (match comments with
   | None -> ()
   | Some s ->
       let s = xmlify_string s in
       Format.fprintf out_fmt "<foc:comments>%s</foc:comments>@\n" s) ;
  Format.fprintf out_fmt "@]</foc:informations>@\n" ;
;;



let gen_doc_species_expr out_fmt ~current_unit species_expr =
  (* ***************************************************************** *)
  (* Just a local recursive function to go inside the paren expression
     when generating the XML for species parameters expressions.       *)
  (* ***************************************************************** *)
  let rec rec_gen_species_param_expr e =
    match e.Parsetree.ast_desc with
     | Parsetree.E_self ->
         Format.fprintf out_fmt "<foc:param>Self</foc:param>@\n"
     | Parsetree.E_constr (cstr_expr, []) ->
         (begin
         let Parsetree.CI qvname = cstr_expr.Parsetree.ast_desc in
         match qvname with
          | Parsetree.Vname vn ->
              Format.fprintf out_fmt "<foc:param>%a</foc:param>@\n"
                pp_xml_vname vn
          | Parsetree.Qualified (mod_name, vn) ->
              Format.fprintf out_fmt
                "<foc:param infile=\"%s\">%a</foc:param>@\n"
                mod_name pp_xml_vname vn
         end)
     | Parsetree.E_paren e' -> rec_gen_species_param_expr e'
     | _ -> assert false in (* ????????product_structures.fcl??????????? *)
  (* **************** *)
  (* Now, do the job. *)
  let species_expr_desc = species_expr.Parsetree.ast_desc in
  let (infile, ident_vname) =
    get_in_file_and_name_from_ident
      ~current_unit species_expr_desc.Parsetree.se_name in
  match species_expr_desc.Parsetree.se_params with
   | [] ->
       (begin
       (* order = "high" because the atom represents a species. *)
       Format.fprintf out_fmt "<foc:atom order=\"high\"" ;
       if infile <> "" then
         Format.fprintf out_fmt " infile=\"%s\"" infile ;
       Format.fprintf out_fmt ">%a</foc:atom>@\n"
         pp_xml_vname ident_vname
       end)
   | params ->
       (begin
       Format.fprintf out_fmt "@[<h 2><foc:app>@\n" ;
       Format.fprintf out_fmt "<foc:foc-name" ;
       if infile <> "" then
         Format.fprintf out_fmt " infile=\"%s\"" infile ;
       Format.fprintf out_fmt ">%a</foc:foc-name>@\n"
         pp_xml_vname ident_vname ;
       List.iter
         (fun species_param ->
           let Parsetree.SP expr = species_param.Parsetree.ast_desc in
           rec_gen_species_param_expr expr)
         params ;
       Format.fprintf out_fmt "@]</foc:app>@\n"
       end)
;;



let gen_doc_inherits out_fmt ~current_unit species_def =
  let species_def_descr = species_def.Parsetree.ast_desc in
  if species_def_descr.Parsetree.sd_inherits.Parsetree.ast_desc <> [] then
    (begin
    (* ************************************ *)
    (* Now generate the "inherits" clauses. *)
    List.iter
      (fun spe_expr ->
        Format.fprintf out_fmt "@[<h 2><foc:inherits>@\n" ;
        gen_doc_species_expr out_fmt ~current_unit spe_expr ;
        Format.fprintf out_fmt "@]</foc:inherits>@\n")
      species_def_descr.Parsetree.sd_inherits.Parsetree.ast_desc
    end)
;;



(* ********************************************************************** *)
(** {b Descr}: Emits the XML code for species parameters declaration in a
    species definition.

    {b Rem} : Not exported outside this module.                           *)
(* ********************************************************************** *)
let gen_doc_parameters out_fmt ~current_unit params =
  List.iter
    (fun (p_vname, p_kind) ->
      Format.fprintf out_fmt "@[<h 2><foc:parameter kind=\"" ;
      (match p_kind.Parsetree.ast_desc with
       | Parsetree.SPT_in in_ident ->
           let (infile, ident_vname) =
             get_in_file_and_name_from_ident ~current_unit in_ident in
           Format.fprintf out_fmt "entity\">@\n" ;
           Format.fprintf out_fmt "<foc:foc-name>%a</foc:foc-name>@\n"
             pp_xml_vname p_vname ;
           Format.fprintf out_fmt "@[<h 2><foc:type>@\n" ;
           (* order = "high" because the atom represents a species. *)
           Format.fprintf out_fmt
             "<foc:atom order=\"high\" infile=\"%s\">%a</foc:atom>@\n"
             infile pp_xml_vname ident_vname ;
           Format.fprintf out_fmt "@]</foc:type>@\n"
       | Parsetree.SPT_is species_expr ->
           Format.fprintf out_fmt "collection\">@\n" ;
           Format.fprintf out_fmt "<foc:foc-name>%a</foc:foc-name>@\n"
             pp_xml_vname p_vname ;
           Format.fprintf out_fmt "@[<h 2><foc:type>@\n" ;
           gen_doc_species_expr out_fmt ~current_unit species_expr ;
           Format.fprintf out_fmt "@]</foc:type>@\n") ;
      (* <foc:informations>. The comments and other informative stuff. *)
      let (_, _, i_descrip, i_mathml, i_latex, i_other) =
        extract_tagged_info_from_documentation p_kind.Parsetree.ast_doc in
      gen_doc_foc_informations out_fmt i_descrip i_mathml i_latex i_other ;
      Format.fprintf out_fmt "@]</foc:parameter>@\n")
    params
;;



(* *********************************************** *)
(** {b Descr}: Emits XML code for a [simple_type].

    {b Rem}: Not exported outside this module.     *)
(* *********************************************** *)
let gen_doc_type ~reuse_mapping out_fmt ty =
  Format.fprintf out_fmt "@[<h 2><foc:type>@\n" ;
  Types.pp_type_simple_to_xml ~reuse_mapping out_fmt ty ;
  Format.fprintf out_fmt "@]</foc:type>@\n"
;;



(** Used for [qualified_vname]s that are not in a <identifier></identifier>
    markup. Especially, those appearing in an [expr_ident] being a
    [EI_method] must **not** be printed with this function since their hosting
    information leads tp a "<foc:of-species></foc:of-species>" markup instead
    of a "infile" attribute.
*)
let gen_doc_qualified_vname_not_EI_method out_fmt qvname =
  match qvname with
   | Parsetree.Vname vname ->
       Format.fprintf out_fmt "<foc:foc-name>%a</foc:foc-name>@\n"
         pp_xml_vname vname
   | Parsetree.Qualified (mod_name, vname) ->
       Format.fprintf out_fmt
         "<foc:foc-name infile=\"%s\">%a</foc:foc-name>@\n"
         mod_name pp_xml_vname vname
;;



let gen_doc_expr_ident out_fmt id =
  match id.Parsetree.ast_desc with
   | Parsetree.EI_local vname ->
       Format.fprintf out_fmt "<foc:foc-name>%a</foc:foc-name>@\n"
         pp_xml_vname vname
   | Parsetree.EI_global qvname ->
       gen_doc_qualified_vname_not_EI_method out_fmt qvname
   | Parsetree.EI_method (qcoll_name_opt, vname) ->
       (begin
       Format.fprintf out_fmt "<foc:foc-name>%a</foc:foc-name>@\n"
         pp_xml_vname vname ;
       match qcoll_name_opt with
        | None -> ()
        | Some qvname ->
            (begin
            match qvname with
             | Parsetree.Vname coll_vname ->
                 Format.fprintf out_fmt
                   "<foc:of-species><foc:foc-name>%a</foc:foc-name>\
                   </foc:of-species>@\n"
                   pp_xml_vname coll_vname
             | Parsetree.Qualified (mod_name, coll_vname) ->
                 Format.fprintf out_fmt
                   "<foc:of-species><foc:foc-name infile=\"%s\">%a\
                   </foc:foc-name></foc:of-species>@\n"
                   mod_name pp_xml_vname coll_vname
            end)
       end)
;;



(* **************************************************** *)
(** {b Descr}: Emits XML code for a [Env.from_history].

    {b Rem}: Not exported outside this module.          *)
(* **************************************************** *)
let gen_doc_history out_fmt from_hist =
  (* foc:initial-apparition. The species where the field was declared or
     defined for the first time along the inheritance tree without being
     re-defined. *)
  Format.fprintf out_fmt "@[<h 2><foc:history>@\n" ;
  let (mod_name, spe_name) = from_hist.Env.fh_initial_apparition in
  Format.fprintf out_fmt
    "<foc:initial-apparition infile=\"%s\">%a</foc:initial-apparition>@\n"
    mod_name pp_xml_vname spe_name ;
  (* foc:comes-from. The latest species from where we get the field by
     inheritance along the inheritance tree. I.e. the closest parent providing
     us the field. *)
  let (come_from_mod_name, come_from_spe_name)  =
    (match from_hist.Env.fh_inherited_along with
     | [] -> from_hist.Env.fh_initial_apparition
     | (host, _) :: _ -> host) in
  Format.fprintf out_fmt "<foc:comes-from infile=\"%s\">%a</foc:comes-from>@\n"
    come_from_mod_name pp_xml_vname come_from_spe_name ;
  Format.fprintf out_fmt "@]</foc:history>@\n"
;;



(* ********************************************** *)
(* Format.formatter -> Parsetree.constant -> unit *)
(** {b Descr}: Emits the XML code for constants.

    {b Rem}: Not exported outside this modole.    *)
(* ********************************************** *)
let gen_doc_constant out_fmt cst =
  match cst.Parsetree.ast_desc with
   | Parsetree.C_int s ->
       Format.fprintf out_fmt "<foc:int>\"%s\"</foc:int>@\n" s
   | Parsetree.C_float s ->
       Format.fprintf out_fmt "<foc:float>\"%s\"</foc:float>@\n" s
   | Parsetree.C_bool s ->
       Format.fprintf out_fmt "<foc:bool>\"%s\"</foc:bool>@\n" s
   | Parsetree.C_string s ->
       Format.fprintf out_fmt "<foc:string>\"%s\"</foc:string>@\n"
         (xmlify_string s)
   | Parsetree.C_char c ->
       Format.fprintf out_fmt "<foc:char>\"%s\"</foc:char>@\n"
         (xmlify_string (Char.escaped c))
;;



(* ************************************************************************* *)
(* Format.formatter -> string -> Parsetree.vname list ->                     *)
(*   Parsetree.type_expr -> Parsetree.logical_expr -> unit                   *)
(** {b Descr}: Emits the XML for a "forall" / "exists" logical expression.
    Since the DTD's structure only allows to have one variable at once for a
    "foc:all" / "foc:ex", we nest each bound variable from the list
    [vnames].
    Since the process is the same for "forall" and "exists", the function
    takes the binder's name to use (as a string). The legal binders are
    currently "ex" and "all". This name will be prefixed by "foc:" to create
    the complete markup.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let rec gen_doc_forall_exists out_fmt binder_name vnames ty_expr lexpr =
  let ty =
    (match ty_expr.Parsetree.ast_type with
     | Parsetree.ANTI_type t -> t
     | Parsetree.ANTI_none | Parsetree.ANTI_non_relevant -> assert false
     | Parsetree.ANTI_scheme sch -> Types.specialize sch) in
  let rec rec_gen = function
    | [] -> gen_doc_proposition out_fmt lexpr
    | h :: q ->
        (* Binder is "all" or "ex". *)
        Format.fprintf out_fmt "@[<h 2><foc:%s>@\n" binder_name ;
        (* foc:var foc:foc-name. *)
        Format.fprintf out_fmt
          "<foc:var><foc:foc-name>%a</foc:foc-name></foc:var>@\n"
          pp_xml_vname h ;
        (* foc:type. *)
        gen_doc_type ~reuse_mapping: true out_fmt ty ;
        (* %foc:proposition. *)
        rec_gen q ;
        Format.fprintf out_fmt "@]</foc:%s>@\n" binder_name in
  rec_gen vnames



and gen_doc_proposition out_fmt initial_prop =
  let rec rec_gen proposition =
    match proposition.Parsetree.ast_desc with
     | Parsetree.Pr_forall (vnames, ty_expr, lexpr) ->
         (* Nested foc:all. *)
         gen_doc_forall_exists out_fmt "all" vnames ty_expr lexpr
     | Parsetree.Pr_exists (vnames, ty_expr, lexpr) ->
         (* foc:ex. *)
         gen_doc_forall_exists out_fmt "ex" vnames ty_expr lexpr
     | Parsetree.Pr_imply (lexpr1 , lexpr2) ->
         (* foc:implies. *)
         Format.fprintf out_fmt "@[<h 2><foc:implies>@\n" ;
         rec_gen lexpr1 ;
         rec_gen lexpr2 ;
         Format.fprintf out_fmt "@]</foc:implies>@\n"
     | Parsetree.Pr_or (lexpr1 , lexpr2) ->
         (* foc:or. *)
         Format.fprintf out_fmt "@[<h 2><foc:or>@\n" ;
         rec_gen lexpr1 ;
         rec_gen lexpr2 ;
         Format.fprintf out_fmt "@]</foc:or>@\n"
     | Parsetree.Pr_and (lexpr1 , lexpr2) ->
         (* foc:and. *)
         Format.fprintf out_fmt "@[<h 2><foc:and>@\n" ;
         rec_gen lexpr1 ;
         rec_gen lexpr2 ;
         Format.fprintf out_fmt "@]</foc:and>@\n"
     | Parsetree.Pr_equiv (lexpr1 , lexpr2) ->
         (* foc:equiv. *)
         Format.fprintf out_fmt "@[<h 2><foc:equiv>@\n" ;
         rec_gen lexpr1 ;
         rec_gen lexpr2 ;
         Format.fprintf out_fmt "@]</foc:equiv>@\n"
     | Parsetree.Pr_not lexpr ->
         (* foc:not. *)
         Format.fprintf out_fmt "@[<h 2><foc:not>@\n" ;
         rec_gen lexpr ;
         Format.fprintf out_fmt "@]</foc:not>@\n"
     | Parsetree.Pr_expr expr -> gen_doc_expression out_fmt expr
     | Parsetree.Pr_paren lexpr ->
         (* foc:paren-logical-expr. *)
         Format.fprintf out_fmt "@[<h 2><foc:paren-logical-expr>@\n" ;
         rec_gen lexpr ;
         Format.fprintf out_fmt "@]</foc:paren-logical-expr>@\n" in
  rec_gen initial_prop



and gen_doc_expression out_fmt initial_expression =
  let rec rec_gen expression =
    match expression.Parsetree.ast_desc with
     | Parsetree.E_self ->
         Format.fprintf out_fmt "@[<h 2><foc:identifier>@\n" ;
         Format.fprintf out_fmt "<foc:foc-name>Self</foc:foc-name>" ;
         Format.fprintf out_fmt "@]</foc:identifier>@\n"
     | Parsetree.E_const cst -> gen_doc_constant out_fmt cst
     | Parsetree.E_fun (vnames, expr) ->
         Format.fprintf out_fmt "@[<h 2>foc:fun>\n" ;
         List.iter
           (fun vname ->
             Format.fprintf out_fmt "<foc:name>%a</foc:name>@\n"
               pp_xml_vname vname)
           vnames ;
         rec_gen expr ;
         Format.fprintf out_fmt "@]</foc:fun>@\n"
     | Parsetree.E_var id ->
         Format.fprintf out_fmt "@[<h 2><foc:identifier>@\n" ;
         gen_doc_expr_ident out_fmt id ;
         Format.fprintf out_fmt "@]</foc:identifier>@\n"
     | Parsetree.E_app (expr, exprs) ->
         Format.fprintf out_fmt "@[<h 2><foc:application>@\n" ;
         rec_gen expr ;
         List.iter rec_gen exprs ;
         Format.fprintf out_fmt "@]</foc:application>@\n"
(*
     | Parsetree.E_constr (cstr_expr, exprs) ->
     | Parsetree.E_match (expr, pat_exprs) ->
*)
     | Parsetree.E_if (expr1, expr2, expr3) ->
         Format.fprintf out_fmt "@[<h 2><foc:if-expr>@\n" ;
         rec_gen expr1 ;
         rec_gen expr2 ;
         rec_gen expr3 ;
         Format.fprintf out_fmt "@]</foc:if-expr>@\n"
(*
     | Parsetree.E_let (let_def, expr) ->
*)
     | Parsetree.E_record label_exprs ->
         Format.fprintf out_fmt "@[<h 2><foc:record-expr>@\n" ;
         List.iter
           (fun (label_ident, expr) ->
             let (Parsetree.LI label) = label_ident.Parsetree.ast_desc in
             gen_doc_qualified_vname_not_EI_method out_fmt label ;
             rec_gen expr)
           label_exprs ;
         Format.fprintf out_fmt "@]</foc:record-expr>@\n"
     | Parsetree.E_record_access (expr, label_ident) ->
         Format.fprintf out_fmt "@[<h 2><foc:record-access-expr>@\n" ;
         rec_gen expr ;
         let (Parsetree.LI label) = label_ident.Parsetree.ast_desc in
         gen_doc_qualified_vname_not_EI_method out_fmt label ;
         Format.fprintf out_fmt "@]</foc:record-access-expr>@\n"
     | Parsetree.E_record_with (expr, label_exprs) ->
         Format.fprintf out_fmt "@[<h 2><foc:record-with-expr>@\n" ;
         rec_gen expr ;
         List.iter
           (fun (label_ident, expr) ->
             let (Parsetree.LI label) = label_ident.Parsetree.ast_desc in
             gen_doc_qualified_vname_not_EI_method out_fmt label ;
             rec_gen expr)
           label_exprs ;
         Format.fprintf out_fmt "@]</foc:record-with-expr>@\n"
     | Parsetree.E_tuple exprs ->
         Format.fprintf out_fmt "@[<h 2><foc:tuple-expr>@\n" ;
         List.iter rec_gen exprs ;
         Format.fprintf out_fmt "@]</foc:tuple-expr>@\n"
(*
     | Parsetree.E_external external_expr ->
*)
     | Parsetree.E_paren expr ->
         Format.fprintf out_fmt "@[<h 2><foc:paren-expr>@\n" ;
         rec_gen expr ;
         Format.fprintf out_fmt "@]</foc:paren-expr>@\n"
     | _ -> (* TODO. *) ()
 in
  rec_gen initial_expression
;;



let gen_doc_logical_let out_fmt from_opt name pnames sch body_as_prop doc =
  (* foc:letprop. *)
  Format.fprintf out_fmt "@[<h 2><foc:letprop>@\n" ;
  (* foc:foc-name. *)
  Format.fprintf out_fmt "<foc:foc-name>%a</foc:foc-name>@\n"
    pp_xml_vname name ;
  (* foc:history?. *)
  (match from_opt with
   | Some from -> gen_doc_history out_fmt from
   | None ->()) ;
  (* foc:informations?. *)
  let (_, _, i_descrip, i_mathml, i_latex, i_other) =
    extract_tagged_info_from_documentation doc in
  gen_doc_foc_informations out_fmt i_descrip i_mathml i_latex i_other ;
  (* foc:param-prop*. *)
  let (params_with_type, _, _) =
    MiscHelpers.bind_parameters_to_types_from_type_scheme
      ~self_manifest: None (Some sch) pnames in
  List.iter
    (fun (p_name, p_ty_opt) ->
      (* foc:param-prop. *)
      Format.fprintf out_fmt "@[<h 2><foc:param-prop>@\n" ;
      let ty = (match p_ty_opt with None -> assert false | Some t -> t) in
      (* foc:foc-name. *)
      Format.fprintf out_fmt "<foc:foc-name>%a</foc:foc-name>@\n"
        pp_xml_vname p_name ;
      (* foc:type. *)
      gen_doc_type ~reuse_mapping: true out_fmt ty ;
      Format.fprintf out_fmt "@]</foc:param-prop>@\n")
    params_with_type ;
  (* foc:proposition. *)
  gen_doc_proposition out_fmt body_as_prop ;
  Format.fprintf out_fmt "@]</foc:letprop>@\n"
;;



let gen_doc_computational_let out_fmt from name sch rec_flag doc =
  let attr_rec_string =
    (match rec_flag with
     | Parsetree.RF_rec -> " recursive=\"yes\""
     | Parsetree.RF_no_rec -> "") in
  Format.fprintf out_fmt "@[<h 2><foc:definition%s>@\n" attr_rec_string ;
  (* foc:foc-name. *)
  Format.fprintf out_fmt "<foc:foc-name>%a</foc:foc-name>@\n"
    pp_xml_vname name ;
  (* foc:history. *)
  gen_doc_history out_fmt from ;
  (* foc:informations. *)
  let (_, _, i_descrip, i_mathml, i_latex, i_other) =
    extract_tagged_info_from_documentation doc in
  gen_doc_foc_informations out_fmt i_descrip i_mathml i_latex i_other ;
  (* foc:ho?. *) (* TODO *)
  (* foc:type. *)
  gen_doc_type ~reuse_mapping: false out_fmt (Types.specialize sch) ;
  Format.fprintf out_fmt "@]</foc:definition>@\n"
;;



let gen_doc_computational_toplevel_let out_fmt name sch rec_flag =
  let attr_rec_string =
    (match rec_flag with
     | Parsetree.RF_rec -> " recursive=\"yes\""
     | Parsetree.RF_no_rec -> "") in
  Format.fprintf out_fmt "@[<h 2><foc:global-fun%s>@\n" attr_rec_string ;
  (* foc:foc-name. *)
  Format.fprintf out_fmt "<foc:foc-name>%a</foc:foc-name>@\n"
    pp_xml_vname name ;
  (* foc:type. *)
  gen_doc_type ~reuse_mapping: false out_fmt (Types.specialize sch) ;
  Format.fprintf out_fmt "@]</foc:global-fun>@\n"
;;



let gen_doc_theorem out_fmt from name lexpr doc =
  Format.fprintf out_fmt "@[<h 2><foc:theorem>@\n" ;
  (* foc:foc-name. *)
  Format.fprintf out_fmt "<foc:foc-name>%a</foc:foc-name>@\n"
    pp_xml_vname name ;
  (* foc:history. *)
  gen_doc_history out_fmt from ;
  (* foc:informations. *)
  let (_, _, i_descrip, i_mathml, i_latex, i_other) =
    extract_tagged_info_from_documentation doc in
  gen_doc_foc_informations out_fmt i_descrip i_mathml i_latex i_other ;
  (* foc:proposition. *)
  Format.fprintf out_fmt "@[<h 2><foc:proposition>@\n" ;
  gen_doc_proposition out_fmt lexpr ;
  Format.fprintf out_fmt "@]</foc:proposition>@\n" ;
  (* foc:proof. *)
  Format.fprintf out_fmt "@[<h 2><foc:proof>@\n" ;
  (* TODO. *)
  Format.fprintf out_fmt "@]</foc:proof>@\n" ;
  Format.fprintf out_fmt "@]</foc:theorem>@\n"
;;



(* *************************************************************** *)
(** {b Descr}: Emits the XML code for a method of kind "property".

    {b Rem}: Not exported outside this module.                     *)
(* *************************************************************** *)
let gen_doc_property out_fmt from name lexpr doc =
  Format.fprintf out_fmt "@[<h 2><foc:property>@\n" ;
  (* foc:foc-name. *)
  Format.fprintf out_fmt "<foc:foc-name>%a</foc:foc-name>@\n"
    pp_xml_vname name ;
  (* foc:history. *)
  gen_doc_history out_fmt from ;
  (* foc:informations. *)
  let (_, _, i_descrip, i_mathml, i_latex, i_other) =
    extract_tagged_info_from_documentation doc in
  gen_doc_foc_informations out_fmt i_descrip i_mathml i_latex i_other ;
  (* foc:proposition. *)
  Format.fprintf out_fmt "@[<h 2><foc:proposition>@\n" ;
  gen_doc_proposition out_fmt lexpr ;
  Format.fprintf out_fmt "@]</foc:proposition>@\n" ;
  Format.fprintf out_fmt "@]</foc:property>@\n"
;;



(* ************************************************************************ *)
(** {b Descr}: Emits the XML code for a method of a species.
    We also take in parameter the list of fields of the species definition.
    We need it only to recover the documentation attached to each method.
    In effect, in the [please_compile_me], we record the list of fields
    under the form of [Env.TypeInformation.species_field]s, and in such
    structure we do not have the documentation of the fields.

    {b Rem}: Not exported outside this module.                              *)
(* ************************************************************************ *)
let gen_doc_method out_fmt species_def_fields = function
  | Env.TypeInformation.SF_sig (from, n, sch) ->
      (begin
      (* Factorise the code: directly get the documentation for the 2 cases ! *)
      let doc = find_documentation_of_method n species_def_fields in
      let (_, _, i_descrip, i_mathml, i_latex, i_other) =
        extract_tagged_info_from_documentation doc in
      (* foc:signature, foc:carrier. *)
      if n = (Parsetree.Vlident "representation") then
        (begin
        Format.fprintf out_fmt "@[<h 2><foc:carrier>@\n" ;
        (* foc:history. *)
        gen_doc_history out_fmt from ;
        (* foc:informations. *)
        gen_doc_foc_informations out_fmt i_descrip i_mathml i_latex i_other ;
        (* foc:ho?. *) (* TODO. *)
        (* foc:type. *)
        gen_doc_type ~reuse_mapping: false out_fmt (Types.specialize sch) ;
        Format.fprintf out_fmt "@]</foc:carrier>@\n"
        end)
      else
        (begin
        Format.fprintf out_fmt "@[<h 2><foc:signature>@\n" ;
        let n_as_xml = xmlify_string (Parsetree_utils.name_of_vname n) in
        (* foc:foc-name. *)
        Format.fprintf out_fmt "<foc:foc-name>%s</foc:foc-name>@\n" n_as_xml ;
        (* foc:history. *)
        gen_doc_history out_fmt from ;
        (* foc:informations. *)
        gen_doc_foc_informations out_fmt i_descrip i_mathml i_latex i_other ;
        (* foc:ho?. *) (* TODO. *)
        (* foc:type. *)
        gen_doc_type ~reuse_mapping: false out_fmt (Types.specialize sch) ;
        Format.fprintf out_fmt "@]</foc:signature>@\n"
        end)
      end)
   | Env.TypeInformation.SF_let (from, n, pnames, sch, body, _, _, lflags) ->
         (begin
         (* foc:definition, foc:letprop. *)
         let doc = find_documentation_of_method n species_def_fields in
         match lflags.Env.TypeInformation.ldf_logical with
          | Parsetree.LF_logical ->
              (begin
              match body with
               | Parsetree.BB_logical lexpr ->
                   gen_doc_logical_let
                     out_fmt (Some from) n pnames sch lexpr doc
               | Parsetree.BB_computational
                   { Parsetree.ast_desc = Parsetree.E_external _ } ->
                     (* The only admitted case is a "logical let" defined as an
                        "external". In this case, since external stuff is an
                        expression, we find an expression as body for a 
                        "logical let". Otherwise, in any other case that's an
                        error. *)
                     ()  (* TODO. Not handled by the DTD and XSLs. *)
               | Parsetree.BB_computational _ -> assert false
              end)
          | Parsetree.LF_no_logical ->
              gen_doc_computational_let
                out_fmt from n sch lflags.Env.TypeInformation.ldf_recursive doc
         end)
   | Env.TypeInformation.SF_let_rec _l ->
       (* foc:definition, foc:letprop. *)  (* TODO *)
       ()
   | Env.TypeInformation.SF_theorem (from, n, _, body, _proof, _) ->
       (* foc:theorem. *)
       let doc = find_documentation_of_method n species_def_fields in
       gen_doc_theorem out_fmt from n body doc
   | Env.TypeInformation.SF_property (from, n, _, body, _) ->
       (* foc:property. *)
       let doc = find_documentation_of_method n species_def_fields in
       gen_doc_property out_fmt from n body doc
;;



(* ****************************************************************** *)
(* {b Descr}: Emits the XML code for a species, including its fields.

   {b Rem}: Not exported outside this module.                         *)
(* ****************************************************************** *)
let gen_doc_species out_fmt ~current_unit species_def species_descr =
  (* foc:species. *)
  Format.fprintf out_fmt "@[<h 2><foc:species>@\n" ;
  Format.fprintf out_fmt "<foc:foc-name>%a</foc:foc-name>@\n"
    pp_xml_vname species_def.Parsetree.ast_desc.Parsetree.sd_name ;
  (* Information: foc:informations. *)
  let (_, _, i_descrip, i_mathml, i_latex, i_other) =
    extract_tagged_info_from_documentation species_def.Parsetree.ast_doc in
  gen_doc_foc_informations out_fmt i_descrip i_mathml i_latex i_other ;
  (* Parameters: foc:parameter*. *)
  gen_doc_parameters
    out_fmt ~current_unit species_def.Parsetree.ast_desc.Parsetree.sd_params ;
  (* Inherits: foc:inherits*. *)
  gen_doc_inherits out_fmt ~current_unit species_def ;
  (* Methods: (%foc:component;)*. *)
  List.iter
    (gen_doc_method out_fmt species_def.Parsetree.ast_desc.Parsetree.sd_fields)
    species_descr.Env.TypeInformation.spe_sig_methods ;
  Format.fprintf out_fmt "@]</foc:species>@\n@\n" ;
;;



(* ************************************************************** *)
(* Format.formatter -> current_unit: Parsetree.module_name ->     *)
(*   Parsetree.collection_def_desc Parsetree.ast ->               *)
(*     Env.TypeInformation.species_description -> unit            *)
(** {b Descr} : Emits the XML code for a collection definition.

    {b Rem}: Not exported outside this module.                    *)
(* ************************************************************** *)
let gen_doc_collection out_fmt ~current_unit coll_def coll_descr =
  (* foc:collection. *)
  Format.fprintf out_fmt "@[<h 2><foc:collection>@\n" ;
  Format.fprintf out_fmt "<foc:foc-name>%a</foc:foc-name>@\n"
    pp_xml_vname coll_def.Parsetree.ast_desc.Parsetree.cd_name ;
  (* Information: foc:informations. *)
  let (_, _, i_descrip, i_mathml, i_latex, i_other) =
    extract_tagged_info_from_documentation coll_def.Parsetree.ast_doc in
  gen_doc_foc_informations out_fmt i_descrip i_mathml i_latex i_other ;
  (* foc:implements. *)
  gen_doc_species_expr
    out_fmt ~current_unit coll_def.Parsetree.ast_desc.Parsetree.cd_body ;
  (* (%foc:component;)*. *)
  List.iter
    (gen_doc_method out_fmt
       (* No documentation will be found since in a collection there is no
          definition. All are inherited via the "implements" clause. So we
          pass the empty list of methods descriptions and in effect no doc
          will be found. *)
       [])
    coll_descr.Env.TypeInformation.spe_sig_methods ;
  Format.fprintf out_fmt "@]</foc:collection>@\n@\n" ;
;;




let gen_doc_concrete_type out_fmt ~current_unit ty_vname ty_descrip =
  (* During specialization, we remind the instanciated variables of the type
     identity scheme to force later their usage for the specialization of
     each sum constructor or record fields. Hence, the sharing of these
     varibles between all these types will be preserved. *)
  let (ty_identity, ty_param_vars) =
    Types.specialize_n_show_instanciated_generalized_vars
      ty_descrip.Env.TypeInformation.type_identity in
  (* foc:concrete-type. *)
  Format.fprintf out_fmt "@[<h 2><foc:concrete-type>@\n" ;
  (* foc:foc-name. *)
  Format.fprintf out_fmt "<foc:foc-name infile=\"%s\">%a</foc:foc-name>@\n"
    current_unit pp_xml_vname ty_vname ;
  (* foc:param*. *)
  List.iter
    (fun ty ->
      Format.fprintf out_fmt "<foc:param infile=\"%s\">" current_unit ;
      gen_doc_type out_fmt ~reuse_mapping: true ty ;
      Format.fprintf out_fmt "</foc:param>@\n")
    ty_param_vars ;
  (* (foc:alias|foc:constr* ). *)
  (match ty_descrip.Env.TypeInformation.type_kind with
   | Env.TypeInformation.TK_abstract ->
       (* TODO. We don't make any difference between an abstract type and
          an abbrev. Both are generated in XML as aliases. A bit weak... *)
       Format.fprintf out_fmt "<foc:alias>" ;
       gen_doc_type ~reuse_mapping: true out_fmt ty_identity ;
       Format.fprintf out_fmt "</foc:alias>@\n"
   | Env.TypeInformation.TK_external _ ->
       Format.fprintf out_fmt "<foc:external-type></foc:external-type>@\n"
   | Env.TypeInformation.TK_variant constructors ->
       List.iter
         (fun (cstr_name, _, sch) ->
           (* foc:constr. *)
           Format.fprintf out_fmt"@[<h 2><foc:constr>@\n" ;
           (* foc:foc-name. *)
           Format.fprintf out_fmt
             "<foc:foc-name infile=\"%s\">%a</foc:foc-name>@\n"
             current_unit pp_xml_vname cstr_name ;
           (* foc:type. *)
           let ty = Types.specialize_with_args sch ty_param_vars in
           gen_doc_type ~reuse_mapping: true out_fmt ty ;
           Format.fprintf out_fmt "@]</foc:constr>@\n")
         constructors
   | Env.TypeInformation.TK_record fields ->
       (* foc:record-type. *)
       Format.fprintf out_fmt"@[<h 2><foc:record-type)@\n" ;
       List.iter
         (fun (field_name, _, sch) ->
           Format.fprintf out_fmt"@[<h 2><foc:record-label-and-type)@\n" ;
           (* foc:name. *)
           Format.fprintf out_fmt "<foc:name>%a</foc:name>@\n"
             pp_xml_vname field_name ;
           let ty = Types.specialize_with_args sch ty_param_vars in
           (* foc:type. *)
           gen_doc_type ~reuse_mapping: true out_fmt ty ;
           Format.fprintf out_fmt "@]</foc:record-label-and-type>@\n")
         fields ;
       Format.fprintf out_fmt "@]</foc:record-type>@\n") ;
  Format.fprintf out_fmt "@]</foc:concrete-type>@\n"
;;



let gen_doc_pcm out_fmt ~current_unit = function
  | Infer.PCM_use (_, comp_unit) ->
      Format.fprintf out_fmt "<foc:load>%s</foc:load>@\n" comp_unit
  | Infer.PCM_open (_, comp_unit) ->
      Format.fprintf out_fmt "<foc:open>%s</foc:open>@\n" comp_unit
  | Infer.PCM_coq_require comp_unit ->
      Format.fprintf out_fmt
        "<foc:coq-require>%s</foc:coq-require>@\n" comp_unit
  | Infer.PCM_type (ty_vname, ty_descrip) ->
      gen_doc_concrete_type out_fmt ~current_unit ty_vname ty_descrip
  | Infer.PCM_let_def (let_def, schemes) ->
      (begin
      (* foc:global-fun foc:letprop *)
      match let_def.Parsetree.ast_desc.Parsetree.ld_logical with
       | Parsetree.LF_logical ->
           List.iter2
             (fun binding scheme ->
               match binding.Parsetree.ast_desc.Parsetree.b_body with
                | Parsetree.BB_logical lexpr ->
                    (* Extract the parameters names. *)
                    let pnames =
                      List.map
                        fst binding.Parsetree.ast_desc.Parsetree.b_params in
                    (* Since this is a toplevel definition and not a method, we
                       don't have any history information. So, pass [None]. *)
                    gen_doc_logical_let
                      out_fmt None binding.Parsetree.ast_desc.Parsetree.b_name
                      pnames scheme lexpr binding.Parsetree.ast_doc
                | Parsetree.BB_computational
                    { Parsetree.ast_desc = Parsetree.E_external _ } ->
                      (* The only admitted case is a "logical let" defined as an
                         "external". In this case, since external stuff is an
                         expression, we find an expression as body for a 
                         "logical let". Otherwise, in any other case that's an
                         error. *)
                      ()  (* TODO. Not handled by the DTD and XSLs. *)
                | Parsetree.BB_computational _ -> assert false)
             let_def.Parsetree.ast_desc.Parsetree.ld_bindings schemes
       | Parsetree.LF_no_logical ->
           List.iter2
             (fun binding sch ->
               gen_doc_computational_toplevel_let
                 out_fmt
                 binding.Parsetree.ast_desc.Parsetree.b_name
                 sch let_def.Parsetree.ast_desc.Parsetree.ld_rec)
             let_def.Parsetree.ast_desc.Parsetree.ld_bindings schemes
      end)
  | Infer.PCM_theorem (_, _) ->
      (* foc:theorem *) () (* TODO. *)
  | Infer.PCM_expr _ -> () (* TODO. *)
  | Infer.PCM_species (species_def, species_descr, _) ->
      gen_doc_species out_fmt ~current_unit species_def species_descr
  | Infer.PCM_collection (coll_def, col_descr, _) ->
      gen_doc_collection out_fmt ~current_unit coll_def col_descr
;;



(* ************************************************************** *)
(* string -> Parsetree.file_desc Parsetree.ast ->                 *)
(*   Infer.please_compile_me list -> unit                         *)
(** {b Descr} : Entry point for the documentation generation on a
    complete FoCaL source code.

    {b Rem}: Exported outside this module.                        *)
(* ************************************************************** *)
let gen_doc_please_compile_me input_file_name ast_root pcms =
  let input_name_no_extension = Filename.chop_extension input_file_name in
  let current_unit = Filename.basename input_name_no_extension in
  let out_filename = input_name_no_extension ^ ".fcd" in
  let out_channel = open_out_bin out_filename in
  let out_fmt = Format.formatter_of_out_channel out_channel in
  let (title_opt, author_opt, description_opt) =
    find_title_author_and_description ast_root in
  Format.fprintf out_fmt
    "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>@\n\
    <!-- document automatically generated from a FoCaL program. Note that \
    focdoc has also both a DTD 'focdoc.dtd' and a RELAX NG schema \
    'focdoc.rnc'.  To validate a focdoc file with focdoc.dtd, please \
    product the focdoc file with the '-focalize-doc' option of focalizecc. \
    But whereas an XML document can associate itself with a DTD using a \
    DOCTYPE declaration, RELAX NG does not define a way for an XML document \
    to associate itself with a RELAX NG pattern. -->@\n@\n" ;
  Format.fprintf out_fmt
    "@[<v 2>\
     <foc:focdoc xsi:schemaLocation=\"focal focdoc.xsd mathml2 \
     http://www.w3.org/Math/XMLSchema/mathml2/mathml2.xsd\" \
     xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" \
     xmlns:mml=\"mathml2\" xmlns:foc=\"http://focal.inria.fr/site/index\">@\n" ;
  Format.fprintf out_fmt "<foc:foc-name>%s</foc:foc-name>@\n" current_unit ;
  Format.fprintf out_fmt "@[<v 2><foc:general-informations>@\n" ;
  (match title_opt with
    | Some title ->
        let title = xmlify_string title in
        Format.fprintf out_fmt "<foc:title>%s</foc:title>@\n" title
    | None -> ()) ;
  (match author_opt with
    | Some author ->
        let author = xmlify_string author in
        Format.fprintf out_fmt "<foc:author>%s</foc:author>@\n" author
    | None -> ()) ;
  (match description_opt with
    | Some description ->
        let description = xmlify_string description in
        Format.fprintf out_fmt "<foc:comments>%s</foc:comments>@\n" description
    | None -> ()) ;
  Format.fprintf out_fmt "@]</foc:general-informations>@\n@\n" ;
  List.iter (gen_doc_pcm out_fmt ~current_unit) pcms ;
  Format.fprintf out_fmt "@]</foc:focdoc>@\n" ;
  close_out out_channel
;;
