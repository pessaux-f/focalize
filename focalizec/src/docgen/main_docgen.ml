(***********************************************************************)
(*                                                                     *)
(*                        FoCaLiZe compiler                            *)
(*                                                                     *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                                                                     *)
(*               LIP6  --  INRIA Rocquencourt -- ENSTA ParisTech       *)
(*                                                                     *)
(*  Copyright 2007 - ... LIP6 and INRIA                                *)
(*            2012 - ... ENSTA ParisTech                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)


(* ********************************************************************** *)
(* Format.formatter -> string option -> string option -> string option -> *)
(*   Parsetree.doc_elem list -> unit                                      *)
(* {b Descr}: Generates the "<foc:information>" section from the optional
    values for each element of "<foc:information>" in the DTD.

   {b Rem}: Not exported outside this module.                             *)
(* ********************************************************************** *)
let gen_doc_foc_informations out_fmt name_opt math_opt latex_opt comments =
  Format.fprintf out_fmt "@[<h 2><foc:informations>@\n";
  (match name_opt with
   | None -> ()
   | Some s -> Format.fprintf out_fmt "<foc:name>%s</foc:name>@\n" s);
  (match math_opt with
   | None -> ()
   | Some s -> Format.fprintf out_fmt "<foc:math>%s</foc:math>@\n" s);
  (match latex_opt with
   | None -> ()
   | Some s -> Format.fprintf out_fmt "<foc:latex>%s</foc:latex>@\n" s);
  (match comments with
   | None -> ()
   | Some s ->
       let s = Utils_docgen.xmlify_string s in
       Format.fprintf out_fmt "<foc:comments>%s</foc:comments>@\n" s);
  Format.fprintf out_fmt "@]</foc:informations>@\n";
;;



(* ************************************************************************** *)
(** {b Descr}: Emits XML code for a [simple_type].

    {b Rem}: Not exported outside this module.
 **************************************************************************** *)
let gen_doc_type out_fmt ty = Types.pp_type_simple_to_xml out_fmt ty ;;



(* ************************************************************************** *)
(** {b Descr}: Emits XML code for a [type_variable].

    {b Rem}: Not exported outside this module.
 **************************************************************************** *)
let gen_doc_type_variable out_fmt ty_var =
  Format.fprintf out_fmt "@[<h 2><foc:type-expr>@\n" ;
  Types.pp_type_variable_to_xml out_fmt ty_var ;
  Format.fprintf out_fmt "@]</foc:type-expr>@\n"
;;



(* ********************************************** *)
(* Format.formatter -> Parsetree.constant -> unit *)
(** {b Descr}: Emits the XML code for constants.

    {b Rem}: Not exported outside this modole.    *)
(* ********************************************** *)
let gen_doc_constant out_fmt cst =
  match cst.Parsetree.ast_desc with
   | Parsetree.C_int s ->
       Format.fprintf out_fmt "<foc:cst-int val=\"%s\"></foc:cst-int>@\n" s
   | Parsetree.C_float s ->
       Format.fprintf out_fmt "<foc:cst-float val=\"%s\"></foc:cst-float>@\n" s
   | Parsetree.C_bool s ->
       Format.fprintf out_fmt "<foc:cst-bool val=\"%s\"></foc:cst-bool>@\n" s
   | Parsetree.C_string s ->
       Format.fprintf out_fmt "<foc:cst-string val=\"%s\"></foc:cst-string>@\n"
         (Utils_docgen.xmlify_string s)
   | Parsetree.C_char c ->
       Format.fprintf out_fmt "<foc:cst-char val=\"%s\"></foc:cst-char>@\n"
         (Utils_docgen.xmlify_string (Char.escaped c))
;;



let gen_doc_math_if_any out_fmt env vname =
  try (
    let symbol = Env_docgen.find_method vname env in
    (* If the search didn't fail then we are sure we have at least 1 symbol
       definition. *)
    (match symbol.Env_docgen.sd_mathml with
      | Some s -> Format.fprintf out_fmt "<foc:math>%s</foc:math>@\n" s
      | None -> ()) ;
    (match symbol.Env_docgen.sd_latex with
      | Some s -> Format.fprintf out_fmt "<foc:latex>%s</foc:latex>@\n" s
      | None -> ())
   )
  with Not_found -> () ;;



(** Used for [qualified_vname]s that are not in a <identifier></identifier>
    markup. Especially, those appearing in an [expr_ident] being a
    [EI_method] must **not** be printed with this function since their hosting
    information leads tp a "<foc:of-species></foc:of-species>" markup instead
    of a "infile" attribute.
*)
let gen_doc_qualified_vname_not_EI_method out_fmt env qvname =
  match qvname with
   | Parsetree.Vname vname ->
       (* File location is empty to tell "the current file". *)
       Format.fprintf out_fmt "@[<h 2><foc:identifier infile=\"\">@\n";
       Format.fprintf out_fmt "<foc:fcl-name>%a</foc:fcl-name>@\n"
         Utils_docgen.pp_xml_vname vname ;
       gen_doc_math_if_any out_fmt env vname ;
       Format.fprintf out_fmt "@]</foc:identifier>@\n";
   | Parsetree.Qualified (mod_name, vname) ->
       Format.fprintf out_fmt "@[<h 2><foc:identifier infile=\"%s\">@\n"
         mod_name ;
       Format.fprintf out_fmt "<foc:fcl-name>%a</foc:fcl-name>@\n"
         Utils_docgen.pp_xml_vname vname ;
       gen_doc_math_if_any out_fmt env vname ;
       Format.fprintf out_fmt "@]</foc:identifier>@\n"
;;



let gen_doc_ident out_fmt env id =
  match id.Parsetree.ast_desc with
  | Parsetree.I_local vname ->
      (* File location is empty to tell "the current file". *)
      Format.fprintf out_fmt "@[<h 2><foc:identifier infile=\"\">@\n" ;
      Format.fprintf out_fmt "<foc:fcl-name>%a</foc:fcl-name>@\n"
        Utils_docgen.pp_xml_vname vname ;
      gen_doc_math_if_any out_fmt env vname ;
      Format.fprintf out_fmt "@]</foc:identifier>@\n"
  | Parsetree.I_global qvname ->
      gen_doc_qualified_vname_not_EI_method out_fmt env qvname
;;



let gen_doc_expr_ident out_fmt env id =
  match id.Parsetree.ast_desc with
  | Parsetree.EI_local vname ->
      (* File location is empty to tell "the current file". *)
      Format.fprintf out_fmt "@[<h 2><foc:identifier infile=\"\">@\n" ;
      Format.fprintf out_fmt "<foc:fcl-name>%a</foc:fcl-name>@\n"
        Utils_docgen.pp_xml_vname vname ;
      gen_doc_math_if_any out_fmt env vname ;
      Format.fprintf out_fmt "@]</foc:identifier>@\n"
  | Parsetree.EI_global qvname ->
      gen_doc_qualified_vname_not_EI_method out_fmt env qvname
  | Parsetree.EI_method (qcoll_name_opt, vname) -> (
       (match qcoll_name_opt with
       | None ->
           (* File location is empty to tell "the current file". *)
           Format.fprintf out_fmt "@[<h 2><foc:identifier infile=\"\">@\n"
        | Some qvname -> (
            match qvname with
            | Parsetree.Vname coll_vname ->
                (* File location is empty to tell "the current file". *)
                Format.fprintf out_fmt
                  "@[<h 2><foc:identifier infile=\"\">@\n" ;
                 Format.fprintf out_fmt
                   "<foc:of-species><foc:fcl-name>%a</foc:fcl-name>\
                   </foc:of-species>@\n"
                   Utils_docgen.pp_xml_vname coll_vname
             | Parsetree.Qualified (mod_name, coll_vname) ->
                Format.fprintf out_fmt
                  "@[<h 2><foc:identifier infile=\"%s\">@\n" mod_name ;
                 Format.fprintf out_fmt
                   "<foc:of-species><foc:fcl-name>%a\
                   </foc:fcl-name></foc:of-species>@\n"
                    Utils_docgen.pp_xml_vname coll_vname
           ));
      Format.fprintf out_fmt "<foc:fcl-name>%a</foc:fcl-name>@\n"
        Utils_docgen.pp_xml_vname vname ;
      gen_doc_math_if_any out_fmt env vname ;
      Format.fprintf out_fmt "@]</foc:identifier>@\n"
     )
;;



(* **************************************************** *)
(** {b Descr}: Emits XML code for a [Env.from_history].

    {b Rem}: Not exported outside this module.          *)
(* **************************************************** *)
let gen_doc_history out_fmt from_hist =
  (* foc:initial-apparition. The species where the field was declared or
     defined for the first time along the inheritance tree without being
     re-defined. *)
  Format.fprintf out_fmt "@[<h 2><foc:history>@\n";
  let (mod_name, spe_name) = from_hist.Env.fh_initial_apparition in
  Format.fprintf out_fmt
    "<foc:initial-apparition infile=\"%s\">%a</foc:initial-apparition>@\n"
    mod_name Utils_docgen.pp_xml_vname spe_name;
  (* foc:comes-from. The latest species from where we get the field by
     inheritance along the inheritance tree. I.e. the closest parent providing
     us the field. *)
  let (come_from_mod_name, come_from_spe_name)  =
    (match from_hist.Env.fh_inherited_along with
     | [] -> from_hist.Env.fh_initial_apparition
     | (host, _, _) :: _ -> host) in
  Format.fprintf out_fmt "<foc:comes-from infile=\"%s\">%a</foc:comes-from>@\n"
    come_from_mod_name Utils_docgen.pp_xml_vname come_from_spe_name;
  Format.fprintf out_fmt "@]</foc:history>@\n"
;;



let rec gen_doc_logical_expr out_fmt env initial_prop =
  (* ************************************************************************ *)
  (* Format.formatter -> string -> Parsetree.vname list ->                    *)
  (*   Parsetree.type_expr -> Parsetree.logical_expr -> unit                  *)
  (** {b Descr}: Emits the XML for a "forall" / "exists" logical expression.
      Since the DTD's structure only allows to have one variable at once for a
      "foc:all" / "foc:ex", we nest each bound variable from the list
      [vnames].
      Since the process is the same for "forall" and "exists", the function
      takes the binder's name to use (as a string). The legal binders are
      currently "ex" and "all". This name will be prefixed by "foc:" to create
      the complete markup.

      {b Rem} : Not exported outside this module.                             *)
  (* ************************************************************************ *)
  let rec gen_doc_forall_exists binder_name vnames ty_expr lexpr =
    let ty =
      (match ty_expr.Parsetree.ast_type with
      | Parsetree.ANTI_type t -> t
      | Parsetree.ANTI_none | Parsetree.ANTI_irrelevant -> assert false
      | Parsetree.ANTI_scheme sch -> Types.specialize sch) in
    let rec rec_gen_fo_ex = function
      | [] -> rec_gen_lexpr lexpr
      | h :: q ->
          (* Binder is "all" or "ex". *)
          Format.fprintf out_fmt "@[<h 2><foc:logexpr-%s>@\n" binder_name;
          (* foc:var foc:fcl-name. *)
          Format.fprintf out_fmt
            "<foc:var><foc:fcl-name>%a</foc:fcl-name></foc:var>@\n"
            Utils_docgen.pp_xml_vname h;
          (* foc:type. *)
          gen_doc_type out_fmt ty ;
          (* %foc:logexpr. *)
          rec_gen_fo_ex q ;
          Format.fprintf out_fmt "@]</foc:logexpr-%s>@\n" binder_name in
    rec_gen_fo_ex vnames
  (* ************************************************************************ *)
  (** {b Descr}: Local function to recurse generating documentation on logical
     expressions.
      {b Rem} : Not exported outside this module.                            *)
  (* ************************************************************************ *)
  and rec_gen_lexpr proposition =
    Format.fprintf out_fmt "@[<h 2><foc:logexpr>@\n" ;
    (match proposition.Parsetree.ast_desc with
    | Parsetree.Pr_forall (vnames, ty_expr, lexpr) ->
        (* Nested foc:logexpr-all. *)
        gen_doc_forall_exists "all" vnames ty_expr lexpr
    | Parsetree.Pr_exists (vnames, ty_expr, lexpr) ->
        (* foc:logexpr-ex. *)
        gen_doc_forall_exists "ex" vnames ty_expr lexpr
    | Parsetree.Pr_imply (lexpr1 , lexpr2) ->
        (* foc:logexpr-implies. *)
        Format.fprintf out_fmt "@[<h 2><foc:logexpr-implies>@\n";
        rec_gen_lexpr lexpr1;
        rec_gen_lexpr lexpr2;
        Format.fprintf out_fmt "@]</foc:logexpr-implies>@\n"
    | Parsetree.Pr_or (lexpr1 , lexpr2) ->
        (* foc:logexpr-or. *)
        Format.fprintf out_fmt "@[<h 2><foc:logexpr-or>@\n";
        rec_gen_lexpr lexpr1;
        rec_gen_lexpr lexpr2;
        Format.fprintf out_fmt "@]</foc:logexpr-or>@\n"
    | Parsetree.Pr_and (lexpr1 , lexpr2) ->
        (* foc:logexpr-and. *)
        Format.fprintf out_fmt "@[<h 2><foc:logexpr-and>@\n";
        rec_gen_lexpr lexpr1;
        rec_gen_lexpr lexpr2;
        Format.fprintf out_fmt "@]</foc:logexpr-and>@\n"
    | Parsetree.Pr_equiv (lexpr1 , lexpr2) ->
        (* foc:logexpr-equiv. *)
        Format.fprintf out_fmt "@[<h 2><foc:logexpr-equiv>@\n";
        rec_gen_lexpr lexpr1;
        rec_gen_lexpr lexpr2;
        Format.fprintf out_fmt "@]</foc:logexpr-equiv>@\n"
    | Parsetree.Pr_not lexpr ->
        (* foc:logexpr-not. *)
        Format.fprintf out_fmt "@[<h 2><foc:logexpr-not>@\n";
        rec_gen_lexpr lexpr;
        Format.fprintf out_fmt "@]</foc:logexpr-not>@\n"
    | Parsetree.Pr_expr expr ->
        (* foc:logexpr-expr *)
        Format.fprintf out_fmt "@[<h 2><foc:logexpr-expr>@\n" ;
        gen_doc_expression out_fmt env expr ;
        Format.fprintf out_fmt "@]</foc:logexpr-expr>@\n"
    | Parsetree.Pr_paren lexpr ->
        (* foc:paren-logical-expr. *)
        Format.fprintf out_fmt "@[<h 2><foc:logexpr-paren>@\n";
        rec_gen_lexpr lexpr;
        Format.fprintf out_fmt "@]</foc:logexpr-paren>@\n") ;
    Format.fprintf out_fmt "@]</foc:logexpr>@\n" in
  (* Now do the job. *)
  rec_gen_lexpr initial_prop




and gen_doc_expression out_fmt env expression =
  Format.fprintf out_fmt "@[<h 2><foc:expr>@\n" ;
  (match expression.Parsetree.ast_desc with
  | Parsetree.E_self ->
      Format.fprintf out_fmt "@[<h 2><foc:expr-self>@\n" ;
      Format.fprintf out_fmt "<foc:self>" ;
      Format.fprintf out_fmt "@]</foc:expr-self>@\n"
  | Parsetree.E_const cst ->
      Format.fprintf out_fmt "@[<h 2><foc:expr-cst>@\n" ;
      gen_doc_constant out_fmt cst ;
      Format.fprintf out_fmt "@]</foc:expr-cst>@\n"
  | Parsetree.E_fun (vnames, expr) ->
      Format.fprintf out_fmt "@[<h 2>foc:expr-fun>\n" ;
      List.iter
        (fun vname ->
          Format.fprintf out_fmt "<foc:name>%a</foc:name>@\n"
            Utils_docgen.pp_xml_vname vname)
        vnames ;
      gen_doc_expression out_fmt env expr ;
      Format.fprintf out_fmt "@]</foc:expr-fun>@\n"
  | Parsetree.E_var id -> gen_doc_expr_ident out_fmt env id ;
  | Parsetree.E_app (expr, exprs) ->
      Format.fprintf out_fmt "@[<h 2><foc:expr-app>@\n" ;
      gen_doc_expression out_fmt env expr ;
      List.iter (gen_doc_expression out_fmt env) exprs ;
      Format.fprintf out_fmt "@]</foc:expr-app>@\n"
  | Parsetree.E_constr (cstr_expr, exprs) ->
      let Parsetree.CI glob_ident = cstr_expr.Parsetree.ast_desc in
      Format.fprintf out_fmt "@[<h 2><foc:expr-constructor>@\n" ;
      gen_doc_ident out_fmt env glob_ident ;
      List.iter (gen_doc_expression out_fmt env) exprs ;
      Format.fprintf out_fmt "@]</foc:expr-constructor>@\n"
  | Parsetree.E_match (expr, pat_exprs) ->
      (* TODO *)
      Format.fprintf out_fmt "@[<h 2><foc:expr-match>@\n" ;
      Format.fprintf out_fmt "@]</foc:expr-match>@\n"
  | Parsetree.E_if (expr1, expr2, expr3) ->
      Format.fprintf out_fmt "@[<h 2><foc:expr-if>@\n" ;
      gen_doc_expression out_fmt env expr1 ;
      gen_doc_expression out_fmt env expr2 ;
      gen_doc_expression out_fmt env expr3 ;
      Format.fprintf out_fmt "@]</foc:expr-if>@\n"
  | Parsetree.E_let (let_def, expr) ->
      let env' =
        gen_doc_let_bindings out_fmt env let_def None None "expr-let-in" in
      (* Now, generate the "in" expression. *)
      gen_doc_expression out_fmt env' expr
  | Parsetree.E_record label_exprs ->
      Format.fprintf out_fmt "@[<h 2><foc:expr-record>@\n";
      List.iter
        (fun (label_ident, expr) ->
          let (Parsetree.LI label) = label_ident.Parsetree.ast_desc in
          (* Since record labels are not methods, we don't want any symbol
             at this point. Hence pass the empty environment. *)
          gen_doc_ident out_fmt Env_docgen.empty label ;
          gen_doc_expression out_fmt env expr)
        label_exprs ;
      Format.fprintf out_fmt "@]</foc:expr-record>@\n"
  | Parsetree.E_record_access (expr, label_ident) ->
      Format.fprintf out_fmt "@[<h 2><foc:expr-record-access>@\n" ;
      gen_doc_expression out_fmt env expr ;
      let (Parsetree.LI label) = label_ident.Parsetree.ast_desc in
      (* Since record labels are not methods, we don't want any symbol
         at this point. Hence pass the empty environment. *)
      gen_doc_ident out_fmt Env_docgen.empty label ;
      Format.fprintf out_fmt "@]</foc:expr-record-access>@\n"
  | Parsetree.E_record_with (expr, label_exprs) ->
      Format.fprintf out_fmt "@[<h 2><foc:expr-record-with>@\n" ;
      gen_doc_expression out_fmt env expr ;
      List.iter
        (fun (label_ident, expr) ->
          let (Parsetree.LI label) = label_ident.Parsetree.ast_desc in
          (* Since record labels are not methods, we don't want any symbol
             at this point. Hence pass the empty environment. *)
          gen_doc_ident out_fmt Env_docgen.empty label;
          gen_doc_expression out_fmt env expr)
        label_exprs;
      Format.fprintf out_fmt "@]</foc:expr-record-with>@\n"
  | Parsetree.E_tuple exprs ->
      Format.fprintf out_fmt "@[<h 2><foc:expr-tuple>@\n" ;
      List.iter (gen_doc_expression out_fmt env) exprs ;
      Format.fprintf out_fmt "@]</foc:expr-tuple>@\n"
  | Parsetree.E_external _ext_expr ->
      (* TODO *)
      Format.fprintf out_fmt "<foc:expr-external/>@\n"
  | Parsetree.E_paren expr ->
      Format.fprintf out_fmt "@[<h 2><foc:expr-paren>@\n" ;
      gen_doc_expression out_fmt env expr ;
      Format.fprintf out_fmt "@]</foc:expr-paren>@\n"
  | Parsetree.E_sequence exprs ->
      Format.fprintf out_fmt "@[<h 2><foc:expr-sequence>@\n" ;
      List.iter (gen_doc_expression out_fmt env) exprs ;
      Format.fprintf out_fmt "@]</foc:expr-sequence>@\n") ;
  Format.fprintf out_fmt "@]</foc:expr>@\n"



(** Generate the documentation for a let-def and all its bindings.
    [let_markup] may be "expr-let-in", "global-fun", "meth-let". *)
and gen_doc_let_bindings out_fmt env let_def opt_doc opt_history let_markup =
  let let_def_descr = let_def.Parsetree.ast_desc in
  let attr_rec_string =
    (match let_def_descr.Parsetree.ld_rec with
     | Parsetree.RF_rec -> " recursive=\"yes\""
     | Parsetree.RF_no_rec -> "") in
  Format.fprintf out_fmt "@[<h 2><foc:%s%s>@\n" let_markup attr_rec_string ;
  let extended_env =
    List.fold_left
      (fun env_accu bnd ->
        Format.fprintf out_fmt "<foc:fcl-name>%a</foc:fcl-name>@\n"
          Utils_docgen.pp_xml_vname bnd.Parsetree.ast_desc.Parsetree.b_name ;
        (* foc:history?. *)
        (match opt_history with
        | Some from -> gen_doc_history out_fmt from
        | None -> ()) ;
        (* foc:informations?. *)
        let env_accu' =
          (match opt_doc with
          | Some doc ->
              let (_, _, i_descrip, i_mathml, i_latex, i_other) =
                Utils_docgen.extract_tagged_info_from_annotation doc in
              gen_doc_foc_informations
                out_fmt i_descrip i_mathml i_latex i_other ;
              Env_docgen.add_method
                bnd.Parsetree.ast_desc.Parsetree.b_name
                i_mathml i_latex env_accu
          | None -> env_accu) in
        (* binding-param. *)
        Format.fprintf out_fmt "<foc:binding-param>@\n" ;
        let bnd_scheme =
          (match bnd.Parsetree.ast_type with
          | Parsetree.ANTI_none | Parsetree.ANTI_irrelevant
          | Parsetree.ANTI_type _ -> assert false
          | Parsetree.ANTI_scheme s -> s) in
        let params_names =
          List.map fst bnd.Parsetree.ast_desc.Parsetree.b_params in
        let (params_with_type, _, _) =
          MiscHelpers.bind_parameters_to_types_from_type_scheme
            ~self_manifest: None (Some bnd_scheme) params_names in
        List.iter
          (fun (vname, opt_ty) ->
            Format.fprintf out_fmt "<foc:fcl-name>%a</foc:fcl-name>@\n"
              Utils_docgen.pp_xml_vname vname ;
            let ty = (match opt_ty with None -> assert false | Some t -> t) in
            (* foc:type-expr. *)
            gen_doc_type out_fmt ty)
          params_with_type ;
        Format.fprintf out_fmt "</foc:binding-param>@\n" ;
        (* TODO: type. *)
        (match let_def_descr.Parsetree.ld_logical with
        | Parsetree.LF_logical -> (
            match bnd.Parsetree.ast_desc.Parsetree.b_body with
            | Parsetree.BB_logical lexpr ->
                (* foc:logexpr. *)
                gen_doc_logical_expr out_fmt env_accu' lexpr
            | Parsetree.BB_computational
                { Parsetree.ast_desc = Parsetree.E_external _ } ->
                  (* The only admitted case is a "logical let" defined as an
                     "external". In this case, since external stuff is an
                     expression, we find an expression as body for a
                     "logical let". Otherwise, in any other case that's an
                     error. *)
                  (* TODO *)
                  ()
            | Parsetree.BB_computational _ -> assert false
           )
        | Parsetree.LF_no_logical -> (
            match bnd.Parsetree.ast_desc.Parsetree.b_body with
            | Parsetree.BB_logical _ -> assert false
            | Parsetree.BB_computational expr ->
                gen_doc_expression out_fmt env_accu' expr)) ;
        env_accu')
      env
      let_def_descr.Parsetree.ld_bindings in
  Format.fprintf out_fmt "@]</foc:%s>@\n" let_markup ;
  extended_env
;;



let gen_doc_species_expr out_fmt env ~current_unit species_expr =
  (* ***************************************************************** *)
  (* Just a local recursive function to go inside the paren expression
     when generating the XML for species parameters expressions.       *)
  (* ***************************************************************** *)
  let rec rec_gen_species_param_expr e =
    match e.Parsetree.ast_desc with
    | Parsetree.E_self ->
        (* Let "infile" empty since Self is always in the current file. *)
        Format.fprintf out_fmt
          "<foc:spe-expr-atom infile=\"\"><foc:self/></foc:spe-expr-atom>@\n"
    | Parsetree.E_constr (cstr_expr, []) -> (
        let Parsetree.CI glob_ident = cstr_expr.Parsetree.ast_desc in
        match glob_ident.Parsetree.ast_desc with
        | Parsetree.I_local vn | Parsetree.I_global (Parsetree.Vname vn) ->
            Format.fprintf out_fmt
              "<foc:spe-expr-atom infile=\"\"><foc:fcl-name>%a</foc:fcl-name>\
               </foc:spe-expr-atom>@\n"
              Utils_docgen.pp_xml_vname vn
        | Parsetree.I_global (Parsetree.Qualified (mod_name, vn)) ->
            Format.fprintf out_fmt
              "<foc:spe-expr-atom infile=\"%s\"><foc:fcl-name>%a</foc:fcl-name>\
               </foc:spe-expr-atom>@\n"
              mod_name Utils_docgen.pp_xml_vname vn
       )
    | Parsetree.E_var ident -> (
        (* To handle the case of "IN" parameters names. They must be a simple
           identifier, i.e. parsed as a EI_local. *)
        match ident.Parsetree.ast_desc with
        | Parsetree.EI_local vname ->
            Format.fprintf out_fmt
              "<foc:spe-expr-atom infile=\"\"> <foc:fcl-name>%a</foc:fcl-name>\
               </foc:spe-expr-atom>@\n"
              Utils_docgen.pp_xml_vname vname
        | _ -> assert false
       )
    | Parsetree.E_paren e' -> rec_gen_species_param_expr e'
    | _ ->
        (* Other cases represent expressions used to instantiate an entity
           parameter. *)
        gen_doc_expression out_fmt env e in
  (* **************** *)
  (* Now, do the job. *)
  let species_expr_desc = species_expr.Parsetree.ast_desc in
  let (infile, ident_vname) =
    Utils_docgen.get_in_file_and_name_from_ident
      ~current_unit species_expr_desc.Parsetree.se_name in
  Format.fprintf out_fmt "@[<h 2><foc:spe-expr>@\n" ;
  (match species_expr_desc.Parsetree.se_params with
  | [] -> (
      Format.fprintf out_fmt "@[<h 2><foc:spe-expr-atom infile=\"%s\">@\n"
        infile ;
      Format.fprintf out_fmt "<foc:fcl-name>%a</foc:fcl-name>@\n"
        Utils_docgen.pp_xml_vname ident_vname ;
      Format.fprintf out_fmt "@]</foc:spe-expr-atom>@\n"
     )
   | params -> (
       Format.fprintf out_fmt "@[<h 2><foc:spe-expr-paramd infile=\"%s\">@\n"
         infile ;
       Format.fprintf out_fmt "<foc:fcl-name>%a</foc:fcl-name>@\n"
         Utils_docgen.pp_xml_vname ident_vname ;
       List.iter
         (fun species_param ->
           let Parsetree.SP expr = species_param.Parsetree.ast_desc in
           rec_gen_species_param_expr expr)
         params ;
       Format.fprintf out_fmt "@]</foc:spe-expr-paramd>@\n"
      )) ;
  Format.fprintf out_fmt "@]</foc:spe-expr>@\n"
;;



let gen_doc_inherits out_fmt env ~current_unit species_def =
  let species_def_descr = species_def.Parsetree.ast_desc in
  if species_def_descr.Parsetree.sd_inherits.Parsetree.ast_desc <> [] then (
    (* ************************************ *)
    (* Now generate the "inherits" clauses. *)
    List.iter
      (fun spe_expr ->
        Format.fprintf out_fmt "@[<h 2><foc:spe-inherits>@\n" ;
        gen_doc_species_expr out_fmt env ~current_unit spe_expr ;
        Format.fprintf out_fmt "@]</foc:spe-inherits>@\n")
      species_def_descr.Parsetree.sd_inherits.Parsetree.ast_desc
   )
;;



(* ********************************************************************** *)
(** {b Descr}: Emits the XML code for species parameters declaration in a
    species definition.

    {b Rem} : Not exported outside this module.                           *)
(* ********************************************************************** *)
let gen_doc_parameters out_fmt env ~current_unit params =
  List.iter
    (fun (p_vname, p_kind) ->
      Format.fprintf out_fmt "@[<h 2><foc:spe-param>@\n" ;
      (match p_kind.Parsetree.ast_desc with
       | Parsetree.SPT_in in_ident ->
           let (infile, ident_vname) =
             Utils_docgen.get_in_file_and_name_from_ident
               ~current_unit in_ident in
           Format.fprintf out_fmt "<foc:fcl-name>%a</foc:fcl-name>@\n"
             Utils_docgen.pp_xml_vname p_vname ;
           Format.fprintf out_fmt
             "<foc:spe-param-kind>@\n\
              <foc:spe-param-kind-entity infile=\"%s\">@\n\
              <foc:fcl-name>%a</foc:fcl-name></foc:spe-param-kind-entity>@\n\
              </foc:spe-param-kind>@\n"
             infile Utils_docgen.pp_xml_vname ident_vname
       | Parsetree.SPT_is species_expr ->
           Format.fprintf out_fmt "<foc:fcl-name>%a</foc:fcl-name>@\n"
             Utils_docgen.pp_xml_vname p_vname ;
           Format.fprintf out_fmt
             "<foc:spe-param-kind>@\n\
              <foc:spe-param-kind-collection>@\n" ;
           gen_doc_species_expr out_fmt env ~current_unit species_expr ;
           Format.fprintf out_fmt
             "</foc:spe-param-kind-collection>@\n\
              </foc:spe-param-kind>@\n") ;
      (* <foc:informations>. The comments and other informative stuff. *)
      let (_, _, i_descrip, i_mathml, i_latex, i_other) =
        Utils_docgen.extract_tagged_info_from_annotation
          p_kind.Parsetree.ast_annot in
      gen_doc_foc_informations out_fmt i_descrip i_mathml i_latex i_other ;
      Format.fprintf out_fmt "@]</foc:spe-param>@\n")
    params
;;



(* If [from_opt] is None, then we are called for a toplevel logical let. *)
let gen_doc_theorem out_fmt env opt_from name lexpr doc =
  if opt_from = None then
    Format.fprintf out_fmt "@[<h 2><foc:global-theorem>@\n"
  else Format.fprintf out_fmt "@[<h 2><foc:meth-theorem>@\n" ;
  (* foc:fcl-name. *)
  Format.fprintf out_fmt "<foc:fcl-name>%a</foc:fcl-name>@\n"
    Utils_docgen.pp_xml_vname name ;
  (* foc:history?. *)
  (match opt_from with Some from -> gen_doc_history out_fmt from |None -> ()) ;
  (* foc:informations. *)
  let (_, _, i_descrip, i_mathml, i_latex, i_other) =
    Utils_docgen.extract_tagged_info_from_annotation doc in
  gen_doc_foc_informations out_fmt i_descrip i_mathml i_latex i_other ;
  let env' = Env_docgen.add_method name i_mathml i_latex env in
  (* foc:logexpr. *)
  gen_doc_logical_expr out_fmt env' lexpr;
  (* foc:proof. *)
  Format.fprintf out_fmt "@[<h 2><foc:proof>@\n" ;
  (* TODO. *)
  Format.fprintf out_fmt "@]</foc:proof>@\n" ;
  if opt_from = None then Format.fprintf out_fmt "@]</foc:global-theorem>@\n"
  else Format.fprintf out_fmt "@]</foc:meth-theorem>@\n" ;
  env'
;;



(* *************************************************************** *)
(** {b Descr}: Emits the XML code for a method of kind "property".

    {b Rem}: Not exported outside this module.                     *)
(* *************************************************************** *)
let gen_doc_property out_fmt env from name lexpr doc =
  Format.fprintf out_fmt "@[<h 2><foc:meth-property>@\n" ;
  (* foc:fcl-name. *)
  Format.fprintf out_fmt "<foc:fcl-name>%a</foc:fcl-name>@\n"
    Utils_docgen.pp_xml_vname name ;
  (* foc:history. *)
  gen_doc_history out_fmt from ;
  (* foc:informations. *)
  let (_, _, i_descrip, i_mathml, i_latex, i_other) =
    Utils_docgen.extract_tagged_info_from_annotation doc in
  gen_doc_foc_informations out_fmt i_descrip i_mathml i_latex i_other ;
  let env' = Env_docgen.add_method name i_mathml i_latex env in
  (* foc:logexpr. *)
  gen_doc_logical_expr out_fmt env' lexpr ;
  Format.fprintf out_fmt "@]</foc:meth-property>@\n" ;
  env'
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
let gen_doc_method out_fmt env species_def_fields = function
  | Env.TypeInformation.SF_sig (from, n, sch) -> (
      (* Factorise the code: directly get the documentation for the 2 cases ! *)
      let doc =
        Utils_docgen.find_annotation_of_method n species_def_fields in
      let (_, _, i_descrip, i_mathml, i_latex, i_other) =
        Utils_docgen.extract_tagged_info_from_annotation doc in
      let env' = Env_docgen.add_method n i_mathml i_latex env in
      (* foc:signature, foc:meth-repr. *)
      if n = (Parsetree.Vlident "representation") then (
        Format.fprintf out_fmt "@[<h 2><foc:meth-repr>@\n" ;
        (* foc:history. *)
        gen_doc_history out_fmt from ;
        (* foc:informations. *)
        gen_doc_foc_informations out_fmt i_descrip i_mathml i_latex i_other;
        (* foc:type-expr. *)
        gen_doc_type out_fmt (Types.specialize sch) ;
        Format.fprintf out_fmt "@]</foc:meth-repr>@\n"
       )
      else (
        Format.fprintf out_fmt "@[<h 2><foc:meth-signature>@\n";
        let n_as_xml =
          Utils_docgen.xmlify_string (Parsetree_utils.name_of_vname n) in
        (* foc:fcl-name. *)
        Format.fprintf out_fmt "<foc:fcl-name>%s</foc:fcl-name>@\n" n_as_xml ;
        (* foc:history. *)
        gen_doc_history out_fmt from ;
        (* foc:informations. *)
        gen_doc_foc_informations out_fmt i_descrip i_mathml i_latex i_other;
        (* foc:type-expr. *)
        gen_doc_type out_fmt (Types.specialize sch) ;
        Format.fprintf out_fmt "@]</foc:meth-signature>@\n"
       ) ;
      env'
      )
  | Env.TypeInformation.SF_let
      (from, n, pnames, sch, body, opt_term_proof, _, lflags) ->
      (* foc:meth-let. *)
      let doc =
        Utils_docgen.find_annotation_of_method n species_def_fields in
      (* Let's build a fake let_def AST node to directly feed
         [gen_doc_let_bindings]. *)
      let fake_binding_desc = {
        Parsetree.b_name = n ;
        Parsetree.b_params = List.map (fun n -> (n, None)) pnames ;
        Parsetree.b_type = None ;
        Parsetree.b_body = body } in
      let fake_binding = {
        Parsetree.ast_loc = Location.none ;
        Parsetree.ast_desc = fake_binding_desc ;
        Parsetree.ast_annot = [] ;
        Parsetree.ast_type = Parsetree.ANTI_scheme sch } in
      let fake_let_def_desc = {
        Parsetree.ld_rec = lflags.Env.TypeInformation.ldf_recursive ;
        Parsetree.ld_logical = lflags.Env.TypeInformation.ldf_logical ;
        Parsetree.ld_final = lflags.Env.TypeInformation.ldf_final ;
        Parsetree.ld_local = Parsetree.LF_no_local ; (* Not implemented. *)
        Parsetree.ld_bindings = [fake_binding] ;
        Parsetree.ld_termination_proof = opt_term_proof } in
      let fake_let_def = {
        Parsetree.ast_loc = Location.none ;
        Parsetree.ast_desc = fake_let_def_desc ;
        Parsetree.ast_annot = [] ;
        Parsetree.ast_type = Parsetree.ANTI_scheme sch } in
      gen_doc_let_bindings
        out_fmt env fake_let_def (Some doc) (Some from) "meth-let"
   | Env.TypeInformation.SF_let_rec _l ->
       (* foc:meth-let, foc:meth-letprop. *)  (* TODO *)
       env
   | Env.TypeInformation.SF_theorem (from, n, _, body, _, _) ->
       (* foc:meth-theorem. *)
       let doc =
         Utils_docgen.find_annotation_of_method n species_def_fields in
       gen_doc_theorem out_fmt env (Some from) n body doc
   | Env.TypeInformation.SF_property (from, n, _, body, _) ->
       (* foc:meth-property. *)
       let doc =
         Utils_docgen.find_annotation_of_method n species_def_fields in
       gen_doc_property out_fmt env from n body doc
;;



(* ****************************************************************** *)
(* {b Descr}: Emits the XML code for a species, including its fields.

   {b Rem}: Not exported outside this module.                         *)
(* ****************************************************************** *)
let gen_doc_species out_fmt env ~current_unit species_def species_descr =
  (* foc:species. *)
  Format.fprintf out_fmt "@[<h 2><foc:species>@\n";
  Format.fprintf out_fmt "<foc:fcl-name>%a</foc:fcl-name>@\n"
    Utils_docgen.pp_xml_vname species_def.Parsetree.ast_desc.Parsetree.sd_name;
  (* Information: foc:informations. *)
  let (_, _, i_descrip, i_mathml, i_latex, i_other) =
    Utils_docgen.extract_tagged_info_from_annotation
      species_def.Parsetree.ast_annot in
  gen_doc_foc_informations out_fmt i_descrip i_mathml i_latex i_other ;
  gen_doc_parameters
    out_fmt env ~current_unit
    species_def.Parsetree.ast_desc.Parsetree.sd_params ;
  gen_doc_inherits out_fmt env ~current_unit species_def ;
  (* Methods. *)
  (* Do not [fold_right] otherwise methods will be processed in revere order
     and resulting environment will also be reversed. *)
  let env' =
    List.fold_left
      (fun accu_env meth ->
        Format.fprintf out_fmt "@[<h 2><foc:method>@\n" ;
        let accu' =
          gen_doc_method
            out_fmt accu_env species_def.Parsetree.ast_desc.Parsetree.sd_fields
            meth in
        Format.fprintf out_fmt "@]</foc:method>@\n" ;
        accu')
      env
      species_descr.Env.TypeInformation.spe_sig_methods in
  Format.fprintf out_fmt "@]</foc:species>@\n@\n" ;
  env'
;;



(* ************************************************************** *)
(* Format.formatter -> current_unit: Parsetree.module_name ->     *)
(*   Parsetree.collection_def_desc Parsetree.ast ->               *)
(*     Env.TypeInformation.species_description -> unit            *)
(** {b Descr} : Emits the XML code for a collection definition.

    {b Rem}: Not exported outside this module.                    *)
(* ************************************************************** *)
let gen_doc_collection out_fmt env ~current_unit coll_def coll_descr =
  (* foc:collection. *)
  Format.fprintf out_fmt "@[<h 2><foc:collection>@\n";
  Format.fprintf out_fmt "<foc:fcl-name>%a</foc:fcl-name>@\n"
    Utils_docgen.pp_xml_vname coll_def.Parsetree.ast_desc.Parsetree.cd_name ;
  (* Information: foc:informations. *)
  let (_, _, i_descrip, i_mathml, i_latex, i_other) =
    Utils_docgen.extract_tagged_info_from_annotation
      coll_def.Parsetree.ast_annot in
  gen_doc_foc_informations out_fmt i_descrip i_mathml i_latex i_other ;
  (* foc:implements. *)
  gen_doc_species_expr
    out_fmt env ~current_unit coll_def.Parsetree.ast_desc.Parsetree.cd_body ;
  (* (%foc:component;)*. *)
  (* Do not [fold_right] otherwise methods will be processed in revere order
     and resulting environment will also be reversed. *)
  let env' =
    List.fold_left
      (fun accu_env meth ->
        gen_doc_method
          out_fmt accu_env
          (* No documentation will be found since in a collection there is no
             definition. All are inherited via the "implements" clause. So we
             pass the empty list of methods descriptions and in effect no doc
             will be found. *)
          [] meth)
      env
      coll_descr.Env.TypeInformation.spe_sig_methods in
  Format.fprintf out_fmt "@]</foc:collection>@\n@\n" ;
  env'
;;


let gen_doc_testing out_fmt env ~current_unit:_current_unit testing_def =
(* TODO TESTING: for now we only print the name of the testing, in the
  future we should make a link to or embed the testing report if
   generated. *)
  (* foc:collection. *)
  Format.fprintf out_fmt "@[<h 2><foc:testing>@\n" ;
  Format.fprintf out_fmt "<foc:fcl-name>%a</foc:fcl-name>@\n"
    Utils_docgen.pp_xml_vname
    testing_def.Parsetree.ast_desc.Parsetree.tstd_name ;
  Format.fprintf out_fmt "@]</foc:testing>@\n@\n" ;
  env


let gen_doc_concrete_type out_fmt ~current_unit ty_vname ty_descrip =
  let (ty_param_vars, ty_identity) =
    Types.scheme_split ty_descrip.Env.TypeInformation.type_identity in
  (* foc:type-def. *)
  Format.fprintf out_fmt "@[<h 2><foc:type-def>@\n" ;
  (* foc:fcl-name. *)
  Format.fprintf out_fmt "<foc:fcl-name infile=\"%s\">%a</foc:fcl-name>@\n"
    current_unit Utils_docgen.pp_xml_vname ty_vname ;
  (* foc:ty-var*. *)
  List.iter
    (fun ty ->
      Format.fprintf out_fmt "<foc:ty-var>" ;
      gen_doc_type_variable out_fmt ty ;
      Format.fprintf out_fmt "</foc:ty-var>@\n")
    ty_param_vars ;
  Format.fprintf out_fmt"@[<h 2><foc:type-def-body>" ;
  (match ty_descrip.Env.TypeInformation.type_kind with
   | Env.TypeInformation.TK_abstract ->
       (* TODO. We don't make any difference between an abstract type and
          an abbrev. Both are generated in XML as aliases. A bit weak... *)
       Format.fprintf out_fmt "<foc:tydef-alias>" ;
       gen_doc_type out_fmt ty_identity ;
       Format.fprintf out_fmt "</foc:tydef-alias>@\n"
   | Env.TypeInformation.TK_external _ ->
       Format.fprintf out_fmt "<foc:tydef-external></foc:tydef-external>@\n"
   | Env.TypeInformation.TK_variant constructors ->
       Format.fprintf out_fmt"@[<h 2><foc:tydef-sum>@\n" ;
       List.iter
         (fun (cstr_name, _, sch) ->
           Format.fprintf out_fmt"@[<h 2><foc:ty-val-constr>@\n" ;
           Format.fprintf out_fmt
             "<foc:fcl-name infile=\"%s\">%a</foc:fcl-name>@\n"
             current_unit Utils_docgen.pp_xml_vname cstr_name ;
           gen_doc_type out_fmt (snd (Types.scheme_split sch)) ;
           Format.fprintf out_fmt "@]</foc:ty-val-constr>@\n")
         constructors ;
       Format.fprintf out_fmt "@]</foc:tydef-sum>@\n"
   | Env.TypeInformation.TK_record fields ->
       Format.fprintf out_fmt"@[<h 2><foc:tydef-record>@\n" ;
       List.iter
         (fun (field_name, _, sch) ->
           Format.fprintf out_fmt "<foc:fcl-name>%a</foc:fcl-name>@\n"
             Utils_docgen.pp_xml_vname field_name ;
           gen_doc_type out_fmt (snd (Types.scheme_split sch)))
         fields ;
       Format.fprintf out_fmt "@]</foc:ty-record>@\n") ;
  Format.fprintf out_fmt "@]</foc:type-def-body>@\n" ;
  Format.fprintf out_fmt "@]</foc:type-def>@\n"
;;



let gen_doc_pcm out_fmt env ~current_unit = function
  | Infer.PCM_annotation_title ->
      env
  | Infer.PCM_use (_, comp_unit) ->
      Format.fprintf out_fmt
        "<foc:directive><foc:dir-load>%s</foc:dir-load></foc:directive>@\n"
        comp_unit ;
      env
  | Infer.PCM_open (_, comp_unit) ->
      Format.fprintf out_fmt
        "<foc:directive><foc:dir-open>%s</foc:dir-open></foc:directive>@\n"
        comp_unit ;
      env
  | Infer.PCM_coq_require comp_unit ->
      Format.fprintf out_fmt
        "<foc:directive><foc:dir-coq-require>%s</foc:dir-coq-require>\
         </foc:directive>@\n"
        comp_unit ;
      env
  | Infer.PCM_type (ty_vname, ty_descrip) ->
      Types.purge_type_simple_to_xml_variable_mapping () ;
      gen_doc_concrete_type out_fmt ~current_unit ty_vname ty_descrip ;
      env
  | Infer.PCM_let_def (let_def, schemes) ->
      Types.purge_type_simple_to_xml_variable_mapping () ;
      (* foc:global-fun *)
      gen_doc_let_bindings out_fmt env let_def None None "global-fun"
  | Infer.PCM_theorem (theo_def, _) ->
      Types.purge_type_simple_to_xml_variable_mapping () ;
      (* foc:theorem. *)
      let th_desc = theo_def.Parsetree.ast_desc in
      (* No history, hence pass [None]. *)
      gen_doc_theorem
        out_fmt env None th_desc.Parsetree.th_name th_desc.Parsetree.th_stmt
        theo_def.Parsetree.ast_annot
  | Infer.PCM_expr _ ->
      (* Since toplevel expressions are not usable for any developpement, we
         do not document them. *)
      env
  | Infer.PCM_species (species_def, species_descr, _, _) ->
      Types.purge_type_simple_to_xml_variable_mapping () ;
      gen_doc_species out_fmt env ~current_unit species_def species_descr
  | Infer.PCM_collection (coll_def, col_descr, _) ->
      Types.purge_type_simple_to_xml_variable_mapping () ;
      gen_doc_collection out_fmt env ~current_unit coll_def col_descr
  | Infer.PCM_testing (testing_def) ->
       Types.purge_type_simple_to_xml_variable_mapping () ;
      gen_doc_testing out_fmt env ~current_unit testing_def
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
    Utils_docgen.find_title_author_and_description ast_root in
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
     xmlns:mml=\"mathml2\" xmlns:foc=\"http://focal.inria.fr/site/index\">@\n";
  Format.fprintf out_fmt "<foc:fcl-name>%s</foc:fcl-name>@\n" current_unit ;
  Format.fprintf out_fmt "@[<v 2><foc:general-informations>@\n" ;
  (match title_opt with
    | Some title ->
        let title = Utils_docgen.xmlify_string title in
        Format.fprintf out_fmt "<foc:title>%s</foc:title>@\n" title
    | None -> ()) ;
  (match author_opt with
    | Some author ->
        let author = Utils_docgen.xmlify_string author in
        Format.fprintf out_fmt "<foc:author>%s</foc:author>@\n" author
    | None -> ()) ;
  (match description_opt with
    | Some description ->
        let description = Utils_docgen.xmlify_string description in
        Format.fprintf out_fmt "<foc:comments>%s</foc:comments>@\n" description
    | None -> ()) ;
  Format.fprintf out_fmt "@]</foc:general-informations>@\n@\n" ;
  (* We don't make documentation generation with information coming from other
     files. So we do not have persistent data for the documentation generation
     environment. Hence, we start with an empty environment and we throw the
     final environment. *)
  ignore
    (List.fold_left
      (fun accu_env pcm ->
        gen_doc_pcm out_fmt accu_env ~current_unit pcm)
      Env_docgen.empty
      pcms);
  Format.fprintf out_fmt "@]</foc:focdoc>@\n" ;
  close_out out_channel
;;
