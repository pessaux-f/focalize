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

(* $Id: main_docgen.ml,v 1.8 2008-12-22 15:04:50 pessaux Exp $ *)

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
       (* [Unsure] "order ?????" *)
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
    Format.fprintf out_fmt "@[<h 2><foc:inherits>@\n" ;
    List.iter (gendoc_species_expr out_fmt ~current_unit)      
      species_def_descr.Parsetree.sd_inherits.Parsetree.ast_desc ;
    Format.fprintf out_fmt "@]</foc:inherits>@\n" ;
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
           (* [Unsure] "order ?????" *)
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



let gendoc_type out_fmt ty =
  Format.fprintf out_fmt "@[<h 2><foc:type>@\n" ;
  Types.pp_type_simple_to_xml out_fmt ty ;
  Format.fprintf out_fmt "@]</foc:type>@\n"
;;



let gendoc_method out_fmt = function
  | Env.TypeInformation.SF_sig (_from, n, sch) ->
      (begin
      (* foc:signature, foc:carrier. *)
      if n = (Parsetree.Vlident "representation") then
        (begin
        Format.fprintf out_fmt "@[<h 2><foc:carrier>@\n" ;
        (* foc:dependence, foc:informations, foc:ho?, foc:type. *) (* TODO. *)
        gendoc_type out_fmt (Types.specialize sch) ;
        Format.fprintf out_fmt "@]</foc:carrier>@\n"
        end)
      else
        (begin
        Format.fprintf out_fmt "@[<h 2><foc:signature>@\n" ;
        let n_as_xml = xmlify_string (Parsetree_utils.name_of_vname n) in
        Format.fprintf out_fmt "<foc:foc-name>%s</foc:foc-name>@\n" n_as_xml ;
        (* foc:foc-name, foc:dependence, foc:informations, foc:ho?,
           foc:type. *) (* TODO. *)
        gendoc_type out_fmt (Types.specialize sch) ;
        Format.fprintf out_fmt "@]</foc:signature>@\n"
        end)
      end)
  | Env.TypeInformation.SF_let
         (_from, _n, _parms, _sch, _body, _otp, _rep_deps, _lflag) ->
      (* foc:definition, foc:letprop. *)  (* TODO. *) 
      ()
  | Env.TypeInformation.SF_let_rec _l ->
      (* foc:definition, foc:letprop. *) (* TODO. *)
      ()
  | Env.TypeInformation.SF_theorem (_from, _n, _sch, _body, _proof, _rep_deps) ->
      (* foc:theorem. *) (* TODO. *)
      ()
  | Env.TypeInformation.SF_property (_from, _n, _sch, _body, _rep_deps) ->
      (* foc:property. *) (* TODO. *)
      ()
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





(* *********************************************************************** *)
(* look_for:string -> in_str:string -> (int * int) option                  *)
(** {Descr}: Search from left to right, the indices where the string
    [look_for] was found in the string [in_str]. If the searched string is
    the empty string, then we consider that the search always fails.
    This method is pure brute force.
    If the string is found, then we return the start and stop positions in
    the string [in_str] where [look_for] was found.                        *)
(* *********************************************************************** *)
let str_search ~look_for ~in_str =
  if look_for = "" then None
  else
    (begin
    let m = String.length look_for in
    let n = String.length in_str in
    let found_index = ref None in
    let j = ref 0 in
    while !j <= n - m && !found_index = None do
      let i = ref 0 in
      while !i < m && look_for.[!i] = in_str.[!i + !j] do incr i done ;
      if !i >= m then found_index := Some (!j, (!j + m)) ;
      incr j
    done ;
    !found_index
    end)
;;



(** {b Descr}: If the string [look_for] was found in the string [in_str],
    returns the characters of [in_str] remaining after the position where
    [look_for] was found. *)
let get_text_after_matched_string ~look_for ~in_str =
  match str_search ~look_for ~in_str with
   | None -> None
   | Some (_, match_end) ->
       let rem_len = (String.length in_str) - match_end in
       let sub_str = String.sub in_str match_end rem_len in
       Some sub_str
;;


let find_title_author_and_description ast_root =
  let rec search searched_tag_string = function
    | [] -> None
    | { Parsetree.de_desc = s } :: q ->
        let found =
          get_text_after_matched_string
            ~look_for: searched_tag_string ~in_str: s in
        match found with
         | None ->
             (* If not found in the current doc element, go on searching in
                the next ones. *)
             search searched_tag_string q
         | Some txt ->
             (* Only keep text until the end of line if we find some otherwise,
                keep the whole text. *)
             let txt_len = String.length txt in
             let cut_at = try String.index txt '\n' with Not_found -> txt_len in
             let txt' =
               if cut_at = txt_len then txt else String.sub txt 0 cut_at in
             Some txt' in
  (* ************************************** *)
  (* Do the job on the top node of the AST. *)
  let title_opt = search "@title " ast_root.Parsetree.ast_doc in
  let author_opt = search "@author " ast_root.Parsetree.ast_doc in
  let description_opt = search "@description " ast_root.Parsetree.ast_doc in
  if title_opt <> None && author_opt <> None && description_opt <> None then
    (title_opt, author_opt, description_opt)
  else
    (begin
    (* We didn't find the 3 searched tags in the top node of the AST. So,
       search for them (or at least the missing one if only one was found) in
       the first definition of the source file. *)
    match ast_root.Parsetree.ast_desc with
     | Parsetree.File [] ->
         (* So, the source file is empty (with no definition). Return what we
            got even if there is some tag(s) missing. *)
         (title_opt, author_opt, description_opt)
     | Parsetree.File (first :: _) ->
         (begin
         let title_opt' =
           match title_opt with
            | Some _ -> title_opt
            | None -> search "@title " first.Parsetree.ast_doc in
         let author_opt' =
           match author_opt with
            | Some _ -> author_opt |
              None -> search "@author " first.Parsetree.ast_doc in
         let description_opt' =
           match description_opt with
            | Some _ -> description_opt
            | None -> search "@description " first.Parsetree.ast_doc in
         (title_opt', author_opt', description_opt')
         end)
    end)
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
    <!-- document automatically generated from a FoC program. Note that \
    focdoc has also both a DTD 'focdoc.dtd' and a RELAX NG schema \
    'focdoc.rnc'.  To validate a focdoc file with focdoc.dtd, please \
    product the focdoc file with the '-focdoc' option of focc. But whereas \
    an XML document can associate itself with a DTD using a DOCTYPE \
    declaration, RELAX NG does not define a way for an XML document to \
    associate itself with a RELAX NG pattern.-->@\n@\n" ;
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
