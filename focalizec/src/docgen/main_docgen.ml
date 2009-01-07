(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            Yvan Noyer                                               *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*            François Pessaux                                         *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: main_docgen.ml,v 1.15 2009-01-07 16:18:01 pessaux Exp $ *)



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



let get_in_file_and_name_from_ident ~current_unit ident =
  match ident.Parsetree.ast_desc with
   | Parsetree.I_local vname
   | Parsetree.I_global (Parsetree.Vname vname) ->
       ("", vname)
   | Parsetree.I_global (Parsetree.Qualified (mod_name, vname)) ->
       if mod_name = current_unit then ("", vname)
       else (mod_name, vname)
;;




let gendoc_foc_informations out_fmt name_opt math_opt latex_opt comments =
  Format.fprintf out_fmt "@[<h 2><foc:informations>@\n" ;
  (match name_opt with
   | None -> ()
   | Some _ -> failwith "To do Focdoc 1") ;
  (match math_opt with
   | None -> ()
   | Some _ -> failwith "To do Focdoc 2") ;
  (match latex_opt with
   | None -> ()
   | Some _ -> failwith "To do Focdoc 3") ;
  List.iter
    (fun { Parsetree.de_desc = s } ->
      let s = xmlify_string s in
      Format.fprintf out_fmt "<foc:comments>%s</foc:comments>@\n" s)
    comments ;
  Format.fprintf out_fmt "@]</foc:informations>@\n" ;
;;



let gendoc_species_expr out_fmt ~current_unit species_expr =
  (* ***************************************************************** *)
  (* Just a local recursive function to go inside the paren expression
     when generating the XML for species parameters expressions.       *)
  (* ***************************************************************** *)
  let rec rec_gen_species_param_expr e =
    match e.Parsetree.ast_desc with
     | Parsetree.E_self ->
         Format.fprintf out_fmt "<foc:param>Self</foc:param>@\n"
     | Parsetree.E_constr (cstr_expr, []) ->
         let Parsetree.CI qvname = cstr_expr.Parsetree.ast_desc in
         (* [Fixme] DTD doesn't take care of parameters not hosted in
            the current compilation unit. There is no "infile" attribute. *)
         let vname =
           match qvname with
              Parsetree.Vname vn | Parsetree.Qualified (_, vn) -> vn in
         Format.fprintf out_fmt "<foc:param>%a</foc:param>@\n"
           Sourcify.pp_vname vname
     | Parsetree.E_paren e' -> rec_gen_species_param_expr e'
     | _ -> assert false in
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
         Sourcify.pp_vname ident_vname
       end)
   | params ->
       (begin
       Format.fprintf out_fmt "@[<h 2><foc:app>@\n" ;
       Format.fprintf out_fmt "<foc:foc-name " ;
       if infile <> "" then
         Format.fprintf out_fmt " infile=\"%s\"" infile ;
       Format.fprintf out_fmt "\">%a</foc:foc-name>@\n"
         Sourcify.pp_vname ident_vname ;
       List.iter
         (fun species_param ->
           let Parsetree.SP expr = species_param.Parsetree.ast_desc in
           rec_gen_species_param_expr expr)
         params ;
       Format.fprintf out_fmt "@]</foc:app>@\n"
       end)
;;



let gendoc_inherits out_fmt ~current_unit species_def =
  let species_def_descr = species_def.Parsetree.ast_desc in
  if species_def_descr.Parsetree.sd_inherits.Parsetree.ast_desc <> [] then
    (begin
    (* ************************************ *)
    (* Now generate the "inherits" clauses. *)
    List.iter
      (fun spe_expr ->
        Format.fprintf out_fmt "@[<h 2><foc:inherits>@\n" ;
        gendoc_species_expr out_fmt ~current_unit spe_expr ;
        Format.fprintf out_fmt "@]</foc:inherits>@\n")
      species_def_descr.Parsetree.sd_inherits.Parsetree.ast_desc
    end)
;;



let gendoc_parameters out_fmt ~current_unit params =
  List.iter
    (fun (p_vname, p_kind) ->
      Format.fprintf out_fmt "@[<h 2><foc:parameter kind=\"" ;
      (match p_kind.Parsetree.ast_desc with
       | Parsetree.SPT_in in_ident ->
           let (infile, ident_vname) =
             get_in_file_and_name_from_ident ~current_unit in_ident in
           Format.fprintf out_fmt "entity\">@\n" ;
           Format.fprintf out_fmt "<foc:foc-name>%a</foc:foc-name>@\n"
             Sourcify.pp_vname p_vname ;
           Format.fprintf out_fmt "@[<h 2><foc:type>@\n" ;
           (* order = "high" because the atom represents a species. *)
           Format.fprintf out_fmt 
             "<foc:atom order=\"high\" infile=\"%s\">%a</foc:atom>@\n"
             infile Sourcify.pp_vname ident_vname ;
           Format.fprintf out_fmt "@]</foc:type>@\n"
       | Parsetree.SPT_is species_expr ->
           Format.fprintf out_fmt "collection\">@\n" ;
           Format.fprintf out_fmt "<foc:foc-name>%a</foc:foc-name>@\n"
             Sourcify.pp_vname p_vname ;
           Format.fprintf out_fmt "@[<h 2><foc:type>@\n" ;
           gendoc_species_expr out_fmt ~current_unit species_expr ;
           Format.fprintf out_fmt "@]</foc:type>@\n") ;
      (* The comments and other informative stuff. *)
      gendoc_foc_informations out_fmt None None None  p_kind.Parsetree.ast_doc ;
      Format.fprintf out_fmt "@]</foc:parameter>@\n")
    params
;;



(* *********************************************** *)
(** {b Descr}: Emits XML code for a [simple_type].

    {b Rem}: Not exported outside this module.     *)
(* *********************************************** *)
let gendoc_type out_fmt ty =
  Format.fprintf out_fmt "@[<h 2><foc:type>@\n" ;
  Types.pp_type_simple_to_xml out_fmt ty ;
  Format.fprintf out_fmt "@]</foc:type>@\n"
;;




(* **************************************************** *)
(** {b Descr}: Emits XML code for a [Env.from_history].

    {b Rem}: Not exported outside this module.          *)
(* **************************************************** *)
let gendoc_history out_fmt from_hist =
  (* foc:initial-apparition. The species where the field was declared or
     defined for the first time along the inheritance tree without being
     re-defined. *)
  Format.fprintf out_fmt "@[<h 2><foc:history>@\n" ;
  let (mod_name, spe_name) = from_hist.Env.fh_initial_apparition in
  Format.fprintf out_fmt
    "<foc:initial-apparition infile=\"%s\">%a</foc:initial-apparition>@\n"
    mod_name Sourcify.pp_vname spe_name ;
  (* foc:comes-from. The latest species from where we get the field by
     inheritance along the inheritance tree. I.e. the closest parent providing
     us the field. *)
  let (come_from_mod_name, come_from_spe_name)  =
    (match from_hist.Env.fh_inherited_along with
     | [] -> from_hist.Env.fh_initial_apparition
     | (host, _) :: _ -> host) in
  Format.fprintf out_fmt "<foc:comes-from infile=\"%s\">%a</foc:comes-from>@\n"
    come_from_mod_name Sourcify.pp_vname come_from_spe_name ;
  Format.fprintf out_fmt "@]</foc:history>@\n"
;;



let gen_doc_logical_let out_fmt _ =
  Format.fprintf out_fmt "@[<h 2><foc:letprop>@\n" ;
  (* TODO. *)
  Format.fprintf out_fmt "@]</foc:letprop>@\n"
;;



let gen_doc_computational_let out_fmt from name sch rec_flag _body =
  let attr_rec_string =
    (match rec_flag with
     | Parsetree.RF_rec -> " recursive=\"yes\""
     | Parsetree.RF_no_rec -> "") in
  Format.fprintf out_fmt "@[<h 2><foc:definition%s>@\n" attr_rec_string ;
  (* foc:foc-name. *)
  Format.fprintf out_fmt "<foc:foc-name>%a</foc:foc-name>@\n"
    Sourcify.pp_vname name ;
  (* foc:history. *)
  gendoc_history out_fmt from ;
  (* foc:informations,foc:ho?. *) (* TODO *)
  (* foc:type. *)
  gendoc_type out_fmt (Types.specialize sch) ;
  Format.fprintf out_fmt "@]</foc:definition>@\n"
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
          Sourcify.pp_vname h ;
        (* foc:type. *)
        gendoc_type out_fmt ty ;
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
     | Parsetree.Pr_expr _expr ->
         (* foc:expression. *) (* TODO *)
         ()
     | Parsetree.Pr_paren lexpr ->
         (* foc:paren-logical-expr. *)
         Format.fprintf out_fmt "@[<h 2><foc:paren-logical-expr>@\n" ;
         rec_gen lexpr ;
         Format.fprintf out_fmt "@]</foc:paren-logical-expr>@\n" in
  rec_gen initial_prop
;;



let gen_doc_theorem out_fmt from name lexpr =
  Format.fprintf out_fmt "@[<h 2><foc:theorem>@\n" ;
  (* foc:foc-name. *)
  Format.fprintf out_fmt "<foc:foc-name>%a</foc:foc-name>@\n"
    Sourcify.pp_vname name ;
  (* foc:history. *)
  gendoc_history out_fmt from ;
  (* foc:informations. *)             (* TODO *)
  (* foc:proposition. *)
  Format.fprintf out_fmt "@[<h 2><foc:proposition>@\n" ;
  gen_doc_proposition out_fmt lexpr ;
  Format.fprintf out_fmt "@]</foc:proposition>@\n" ;
  (* foc:proof. *)              (* TODO *)
  Format.fprintf out_fmt "@]</foc:theorem>@\n"
;;



let gen_doc_property out_fmt from name lexpr =
  Format.fprintf out_fmt "@[<h 2><foc:property>@\n" ;
  (* foc:foc-name. *)
  Format.fprintf out_fmt "<foc:foc-name>%a</foc:foc-name>@\n"
    Sourcify.pp_vname name ;
  (* foc:history. *)
  gendoc_history out_fmt from ;
  (* foc:informations. *)         (* TODO *)
  (* foc:proposition. *)
  Format.fprintf out_fmt "@[<h 2><foc:proposition>@\n" ;
  gen_doc_proposition out_fmt lexpr ;
  Format.fprintf out_fmt "@]</foc:proposition>@\n" ;
  Format.fprintf out_fmt "@]</foc:property>@\n"
;;



let gendoc_method out_fmt = function
  | Env.TypeInformation.SF_sig (from, n, sch) ->
      (begin
      (* foc:signature, foc:carrier. *)
      if n = (Parsetree.Vlident "representation") then
        (begin
        Format.fprintf out_fmt "@[<h 2><foc:carrier>@\n" ;
        (* foc:history. *)
        gendoc_history out_fmt from ;
        (* foc:informations, foc:ho?. *) (* TODO. *)
        (* foc:type. *)
        gendoc_type out_fmt (Types.specialize sch) ;
        Format.fprintf out_fmt "@]</foc:carrier>@\n"
        end)
      else
        (begin
        Format.fprintf out_fmt "@[<h 2><foc:signature>@\n" ;
        let n_as_xml = xmlify_string (Parsetree_utils.name_of_vname n) in
        (* foc:foc-name. *)
        Format.fprintf out_fmt "<foc:foc-name>%s</foc:foc-name>@\n" n_as_xml ;
        (* foc:history. *)
        gendoc_history out_fmt from ;
        (* foc:informations, foc:ho?. *) (* TODO. *)
        (* foc:type. *)
        gendoc_type out_fmt (Types.specialize sch) ;
        Format.fprintf out_fmt "@]</foc:signature>@\n"
        end)
      end)
  | Env.TypeInformation.SF_let
         (from, n, _parms, sch, body, _otp, _rep_deps, lflags) ->
      (begin
      (* foc:definition, foc:letprop. *)
      match lflags.Env.TypeInformation.ldf_logical with
       | Parsetree.LF_logical ->
           gen_doc_logical_let out_fmt body
       | Parsetree.LF_no_logical ->
           gen_doc_computational_let
             out_fmt from n sch lflags.Env.TypeInformation.ldf_recursive body
      end)
  | Env.TypeInformation.SF_let_rec _l ->
      (* foc:definition, foc:letprop. *)
      ()
  | Env.TypeInformation.SF_theorem (from, n, _, body, _proof, _rep_deps) ->
      (* foc:theorem. *)
      gen_doc_theorem out_fmt from n body
  | Env.TypeInformation.SF_property (from, n, _, body, _rep_deps) ->
      (* foc:property. *)
      gen_doc_theorem out_fmt from n body
;;



let gendoc_species out_fmt ~current_unit species_def species_descr =
  Format.fprintf out_fmt "@[<h 2><foc:species>@\n" ;
  Format.fprintf out_fmt "<foc:foc-name>%a</foc:foc-name>@\n"
    Sourcify.pp_vname species_def.Parsetree.ast_desc.Parsetree.sd_name ;
  (* Information: foc:informations. *)
  gendoc_foc_informations out_fmt None None None species_def.Parsetree.ast_doc ;
  (* Parameters: foc:parameter*. *)
  gendoc_parameters
    out_fmt ~current_unit species_def.Parsetree.ast_desc.Parsetree.sd_params ;
  (* Inherits: foc:inherits*. *)
  gendoc_inherits out_fmt ~current_unit species_def ;
  (* Methods: (%foc:component;)*. *)
  List.iter
    (gendoc_method out_fmt)
    species_descr.Env.TypeInformation.spe_sig_methods ;
  Format.fprintf out_fmt "@]</foc:species>@\n@\n" ;
;;



let gen_doc_pcm out_fmt ~current_unit = function
  | Infer.PCM_use (_, comp_unit) ->
      Format.fprintf out_fmt "<foc:load>%s</foc:load>@\n" comp_unit
  | Infer.PCM_open (_, comp_unit) ->
      Format.fprintf out_fmt "<foc:open>%s</foc:open>@\n" comp_unit
  | Infer.PCM_coq_require _ ->
      (* [Fixme] DTD doesn't take care of the coq_require directive. *)
      () (* TODO. *)
  | Infer.PCM_type (_, _) ->
      (* foc:concrete-type *) () (* TODO. *)
  | Infer.PCM_let_def (_, _) ->
      (* foc:global-fun foc:letprop *) () (* TODO. *)
  | Infer.PCM_theorem (_, _) ->
      (* foc:theorem *) () (* TODO. *)
  | Infer.PCM_expr _ -> () (* TODO. *)
  | Infer.PCM_species (species_def, species_descr, _) ->
      gendoc_species out_fmt ~current_unit species_def species_descr
  | Infer.PCM_collection (_col_def, _col_description, _) ->
      (* foc:collection *) () (* TODO. *)
;;



(** "@title ", "@author ", "@description ". *)
let find_title_author_and_description ast_root =
  let found_title = ref None in
  let found_author = ref None in
  let found_description = ref None in
  (* A local function to search the tags in an AST node. It will be used on
     the toplevel AST node and on the first AST definition's node in case the
     documentation was not attached to the toplevel AST node. In case there
     is several times the same tag, it always keep the first one found. *)
  let hunt ast_node =
    List.iter
      (fun { Parsetree.de_desc = s } ->
        let lexbuf = Lexing.from_string s in
	let continue = ref true in
	(* We lex ther string until we reach its end, i.e. until we get a
	   non-tagged string being empty (i.e. ""). *)
	while !continue do
          match Doc_lexer.start lexbuf with
           | Doc_lexer.DT_Author s ->
               if !found_author = None then found_author := Some s
           | Doc_lexer.DT_Title s ->
               if !found_title = None then found_title := Some s
           | Doc_lexer.DT_Description s ->
               if !found_description = None then found_description := Some s
	   | Doc_lexer.DT_None "" -> continue := false
           | _ -> ()   (* Ignore other tags. *)
	done)
      ast_node.Parsetree.ast_doc in
  (* Do the job on the top node of the AST. We lex the documentation and
     look at the informations we found inside. *)
  hunt ast_root ;
  (* Then do the same thing on the AST node of the first definition of the
     source file. *)
  (match ast_root.Parsetree.ast_desc with
   | Parsetree.File [] ->
       (* So, the source file is empty (with no definition). Do nothing and be
          happy with what we may have found before on the toplevel AST node. *)
       ()
   | Parsetree.File (first :: _) -> hunt first) ;
  (* Now, just return what we found. *)
  (!found_title, !found_author, !found_description)
;;



let gendoc_please_compile_me input_file_name ast_root pcms =
  let input_name_no_extension = Filename.chop_extension input_file_name in
  let current_unit = Filename.basename input_name_no_extension in
  let out_filename = input_name_no_extension ^ ".xml" in
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
    to associate itself with a RELAX NG pattern.-->@\n@\n" ;
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
