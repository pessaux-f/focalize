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
(*  Copyright 2007, 2008 LIP6 and INRIA                                *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: sourcify.ml,v 1.87 2011-06-14 12:37:29 maarek Exp $ *)

open Parsetree;;

(** {b Desc} : Describe wether an [expr_desc] must appears in infix or
    prefix position as the functionnal part in an applicative expression.
    If the [expr_desc] is not an identifier expression, then is it
    considered as having an unspecified "fixitude".

    {b Exported} : No.                                                    *)
(* ********************************************************************** *)
type expr_desc_fixitude =
  | Fixitude_prefix    (** The functionnal expression is a prefix identifier. *)
  | Fixitude_infix     (** The functionnal expression is an infix identifier. *)
  | Fixitude_applic    (** The functionnal expression is not an identifier or
                           is neither infix nor prefix. *)
;;

let pp_vname ppf = function
  | Parsetree.Vlident s | Parsetree.Vuident s | Parsetree.Vpident s
  | Parsetree.Viident s | Parsetree.Vqident s -> Format.fprintf ppf "%s" s
;;

let pp_constr_name ppf = function
  | Parsetree.Vuident vname ->
      let l = String.length vname in
      let fixitude =
        if l < 2 then Fixitude_applic else
        begin
          match vname.[0] with
          | ':' | '`' when vname.[l - 1] = vname.[0] -> Fixitude_infix
          | _ -> Fixitude_applic
        end in
      begin
        match fixitude with
        | Fixitude_prefix
        | Fixitude_applic -> Format.fprintf ppf "%s" vname
        | Fixitude_infix -> Format.fprintf ppf "( %s )" vname
      end
  | Parsetree.Viident s | Parsetree.Vlident s | Parsetree.Vpident s
  | Parsetree.Vqident s ->
      Format.eprintf "Constructor with wrong ident %s@." s;
      assert false 
;;

let pp_bound_vname ppf = function
  | Parsetree.Vuident _
  | Parsetree.Vqident _ -> assert false
  | Parsetree.Vlident vname -> Format.fprintf ppf "%s" vname
  | Parsetree.Viident vname | Parsetree.Vpident vname -> Format.fprintf ppf "( %s )" vname
;;

let pp_expr_vname ppf = function
  | Parsetree.Vqident _ -> assert false
  | Parsetree.Vuident vname
  | Parsetree.Vlident vname -> Format.fprintf ppf "%s" vname
  | Parsetree.Viident vname | Parsetree.Vpident vname -> Format.fprintf ppf "( %s )" vname
;;

let pp_method_vname ppf = function
  | Parsetree.Vqident _ | Parsetree.Vuident _ -> assert false
  | Parsetree.Vlident vname -> Format.fprintf ppf "%s" vname
  | Parsetree.Viident vname | Parsetree.Vpident vname -> Format.fprintf ppf "( %s )" vname
;;

let pp_vnames sep ppf = Handy.pp_generic_separated_list sep pp_vname ppf
;;

let pp_node_label ppf (i, s) = Format.fprintf ppf "<%d>%s" i s
;;

let pp_node_labels sep ppf =
  Handy.pp_generic_separated_list sep pp_node_label ppf
;;

let pp_ast desc_printer_fct ppf ast =
  let pp_contents ppf = function
    | "" -> ()
    | contents -> Format.fprintf ppf "%s" contents in
  let print_annot_elem ppf ae =
    let contents = ae.Parsetree.ae_desc in
    match ae.Parsetree.ae_tag with
       | "" -> Format.fprintf ppf "(** %a*)@ " pp_contents contents
       | ("0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" as tag) ->
           Format.fprintf ppf "(** %s %a*)@ " tag pp_contents contents
       | tag -> Format.fprintf ppf "(** {%s}%a*)@ "  tag pp_contents contents
  in
  let print_annot_elems ppf aes =
      List.iter (fun d -> Format.fprintf ppf "%a@ " print_annot_elem d) aes in
  let print_annotation = function
    | [] -> ()
    | docs -> Format.fprintf ppf "@[<v>%a@]" print_annot_elems docs in
  (* First, print the annotation if some exists. *)
  print_annotation ast.Parsetree.ast_annot;
  (* Then, print the code itself. *)
  Format.fprintf ppf "%a" desc_printer_fct ast.Parsetree.ast_desc
;;

let pp_qualified_vname ppf = function
  | Parsetree.Vname vname -> Format.fprintf ppf "%a" pp_vname vname
  | Parsetree.Qualified (modname, vname) ->
      Format.fprintf ppf "%s#%a" modname pp_vname vname
;;

let pp_qualified_species ppf (modname, vname) =
  Format.fprintf ppf "%s#%a" modname pp_vname vname
;;

(* An abbrev to get simpler code. *)
let pp_qvname = pp_qualified_vname;;

let pp_ident_desc ppf = function
  | Parsetree.I_local vname -> Format.fprintf ppf "%a" pp_vname vname
  | Parsetree.I_global qvname -> Format.fprintf ppf "%a" pp_qvname qvname
;;

let pp_ident ppf = pp_ast pp_ident_desc ppf
;;
let pp_idents sep ppf = Handy.pp_generic_separated_list sep pp_ident ppf
;;

let pp_label_ident_desc ppf = function
  | Parsetree.LI ident ->
    Format.fprintf ppf "%a" pp_ident ident
;;
let pp_label_ident ppf = pp_ast pp_label_ident_desc ppf;;

let pp_expr_ident_desc ppf = function
  | Parsetree.EI_local vname -> Format.fprintf ppf "%a" pp_expr_vname vname
  | Parsetree.EI_global qvname ->
    Format.fprintf ppf "%a" pp_qvname qvname
  | Parsetree.EI_method (None, vname) ->
    Format.fprintf ppf "!%a" pp_method_vname vname
  | Parsetree.EI_method (Some coll_qvname, vname) ->
    Format.fprintf ppf "%a!%a" pp_qvname coll_qvname pp_method_vname vname
;;
let pp_expr_ident ppf = pp_ast pp_expr_ident_desc ppf
;;

let pp_infix_ident_desc ppf = function
  | Parsetree.EI_local vname -> Format.fprintf ppf "%a" pp_vname vname
  | Parsetree.EI_global qvname ->
    Format.fprintf ppf "%a" pp_qvname qvname
  | Parsetree.EI_method (None, vname) ->
    Format.fprintf ppf "!%a" pp_vname vname
  | Parsetree.EI_method (Some coll_qvname, vname) ->
    Format.fprintf ppf "%a!%a" pp_qvname coll_qvname pp_vname vname
;;
let pp_infix_ident ppf = pp_ast pp_infix_ident_desc ppf
;;

let pp_expr_idents sep ppf =
  Handy.pp_generic_separated_list sep pp_expr_ident ppf
;;

let pp_constructor_ident_desc ppf (Parsetree.CI ident) =
  Format.fprintf ppf "%a" pp_ident ident
;;
let pp_constructor_ident ppf = pp_ast pp_constructor_ident_desc ppf
;;


let vname_fixitude vname = match vname with
| Parsetree.Vpident _ -> Fixitude_prefix
| Parsetree.Viident _ -> Fixitude_infix
| Parsetree.Vuident vname ->
    let l = String.length vname in
    if l < 2 then Fixitude_applic else
    begin
      match vname.[0] with
      | ':' | '`' when vname.[l - 1] = vname.[0] -> Fixitude_infix
      | _ -> Fixitude_applic
    end
| Parsetree.Vlident _
| Parsetree.Vqident _ -> Fixitude_applic
;;

let ident_fixitude (ident:Parsetree.ident) =
  let vname = match ident.Parsetree.ast_desc with
  | I_local vname
  | I_global (Vname vname)
  | I_global (Qualified (_, vname)) -> vname in
  vname_fixitude vname
;;

(* ************************************************************************* *)
(* Parsetree.expr_desc -> expr_desc_fixitude                                 *)
(** {b Descr} : Checks wether an [expr_desc] is a legal binary or unary
    identifier, that can and must appear in infix or prefix  position as the
    functionnal part in an applicative expression. If the [expr_desc] is not
    an identifier expression, then it is considered as having an unspecified
    "fixitude". Same thing if the [expr_desc] is an identifier but is a
    [I_global] or [I_method] using an explicit scope information (in this
    case, it must always be syntactically used in a regular application way).

    {b Exported} : No.                                                       *)
(* ************************************************************************* *)
let expr_desc_fixitude = function
  | Parsetree.E_var id ->
      (* Discriminate according to the lexical tag. *)
      begin match id.Parsetree.ast_desc with
      | Parsetree.EI_local vname -> vname_fixitude vname
      | Parsetree.EI_global (Parsetree.Vname vname) -> vname_fixitude vname
      | Parsetree.EI_global (Parsetree.Qualified (_, _)) ->
          Fixitude_applic  (* So can't be printed as a syntactic operator. *)
      | Parsetree.EI_method (None, vname) -> vname_fixitude vname
      | Parsetree.EI_method (Some _, _) -> Fixitude_applic
      end
  | E_paren _ | E_external _ | E_tuple _ | E_sequence _
  | E_record_with (_, _) | E_record_access (_, _) | E_record _
  | E_let (_, _) | E_if (_, _, _) | E_match (_, _)
  | E_constr (_, _) | E_app (_, _)
  | E_fun (_, _) | E_const _ | E_self -> Fixitude_applic
;;

let rec pp_rep_type_def_desc ppf = function
  | Parsetree.RTE_ident ident -> Format.fprintf ppf "%a" pp_ident ident
  | Parsetree.RTE_fun (rtd1, rtd2) ->
      Format.fprintf ppf "@[<2>%a@ ->@ %a@]"
        pp_rep_type_def rtd1 pp_rep_type_def rtd2
  | Parsetree.RTE_app (ident, rtds) ->
      Format.fprintf ppf "%a@[<2>@ (%a)@]"
        pp_ident ident (pp_rep_type_defs ",") rtds
  | Parsetree.RTE_prod rtds ->
      Format.fprintf ppf "@[<2>%a@]" (pp_rep_type_defs "*") rtds
  | Parsetree.RTE_paren rtd -> Format.fprintf ppf "(%a)" pp_rep_type_def rtd

and pp_rep_type_defs sep ppf =
  Handy.pp_generic_separated_list sep pp_rep_type_def ppf

and pp_rep_type_def ppf = pp_ast pp_rep_type_def_desc ppf
;;

let pp_rep_type_def_representation =
  pp_ast 
    (fun ppf rep_type_def ->
      Format.fprintf ppf "@[<2>representation@ =@ %a@]"
        pp_rep_type_def_desc rep_type_def)
;;

let rec pp_type_expr_desc prio ppf = function
  | Parsetree.TE_ident ident -> Format.fprintf ppf "%a" pp_ident ident
  | Parsetree.TE_fun (te1, te2) ->
      if prio >= 2 then Format.fprintf ppf "@[<1>(";
      Format.fprintf ppf "@[<2>%a@ ->@ %a@]"
        (pp_type_expr_with_prio 2) te1 (pp_type_expr_with_prio 1) te2;
      if prio >= 2 then Format.fprintf ppf "@]"
  | Parsetree.TE_app (ident, tes) ->
      Format.fprintf ppf "%a@[<2>@ (%a)@]"
        pp_ident ident (pp_type_exprs_with_prio 0 ",") tes
  | Parsetree.TE_prod (tes) ->
      Format.fprintf ppf "@[<2>%a@]" (pp_type_exprs_with_prio 0 " *") tes
  | Parsetree.TE_self -> Format.fprintf ppf "Self"
  | Parsetree.TE_prop -> Format.fprintf ppf "prop"
  | Parsetree.TE_paren te ->
      Format.fprintf ppf "(%a)" (pp_type_expr_with_prio 0) te

(* ************************************************************************** *)
(* int -> string -> Format.formatter -> Parsetree.type_expr list -> unit      *)
(** {b Descr} : Pretty prints a [list] of [type_expr] value as FoCal source
    using the given current priority.
    This function is ONLY aimed to be used internally by [pp_type_expr_desc].

    {b Rem} : NEVER call somewhere else than in [pp_type_expr_desc]. This
    is internal stuff !

    {b Exported }: No.                                                        *)
(* ************************************************************************** *)
and pp_type_exprs_with_prio prio sep ppf =
  Handy.pp_generic_separated_list sep (pp_type_expr_with_prio prio) ppf
(* ******************************************************************* *)
(* int -> string -> Format.formatter -> Parsetree.type_expr -> unit    *)
(** {b Descr} : Pretty prints a [type_expr] value as FoCal source
    using the given current priority.
    This function is ONLY aimed to be used internally by
    [pp_type_expr_desc], [pp_type_exprs_with_prio] and[pp_type_exprs].

    {b Rem} : NEVER call somewhere else than in [pp_type_expr_desc],
    [pp_type_exprs_with_prio] and [pp_type_exprs].
    is internal stuff !

    {b Exported }: No.                                                 *)
(* ******************************************************************* *)
and pp_type_expr_with_prio prio ppf =
  pp_ast (pp_type_expr_desc prio) ppf
(* ************************************************************************* *)
(*  int -> string -> Format.formatter -> Parsetree.type_expr list -> unit    *)
(** {b Descr} : Pretty prints a [list] of [type_expr] value as FoCal source.
    The initial priority is 0, hence this function is initial pretty print
    entry point for 1 list of type expressions.

    {b Exported} : No.                                                       *)
(* ************************************************************************* *)
and pp_type_exprs sep ppf =
  Handy.pp_generic_separated_list sep (pp_type_expr_with_prio 0) ppf
(* ***************************************************************** *)
(* Format.formatter -> Parsetree.type_expr -> unit                   *)
(** {b Descr} : Pretty prints a [type_expr] value as FoCal source.
    The initial priority is 0, hence this function is initial pretty
    print entry point for 1 type expression.

    {b Exported} : No.                                               *)
(* ***************************************************************** *)
and pp_type_expr ppf = pp_ast (pp_type_expr_desc 0) ppf
;;

let pp_constant_desc ppf = function
  | Parsetree.C_int s | Parsetree.C_float s | Parsetree.C_bool s ->
      Format.fprintf ppf "%s" s
  | Parsetree.C_string s -> Format.fprintf ppf "\"%s\"" (String.escaped s)
  | Parsetree.C_char c ->
      let tmp_s = " " in
      tmp_s.[0] <- c;
      Format.fprintf ppf "%s" tmp_s
;;

let pp_constant = pp_ast pp_constant_desc
;;

let pp_local_flag ppf = function
  | Parsetree.LF_no_local -> ()
  | Parsetree.LF_local -> Format.fprintf ppf "local@ "
;;

let pp_logical_flag ppf = function
  | Parsetree.LF_no_logical -> ()
  | Parsetree.LF_logical -> Format.fprintf ppf "logical@ "
;;

let pp_rec_flag ppf = function
  | Parsetree.RF_no_rec -> ()
  | Parsetree.RF_rec -> Format.fprintf ppf "rec@ "
  | Parsetree.RF_structural -> Format.fprintf ppf "recstruct@ "
;;

(* *********************************************************************** *)
(* pp_let_def_binding_flags :                                              *)
(*   Format.formatter ->                                                   *)
(*     (Parsetree.rec_flag * Parsetree.log_flag * Parsetree.loc_flag) ->   *)
(*       unit                                                              *)
(** {b Descr} : Pretty prints a [let_def_desc] binding kind as FoCal
    source. It mostly determines if the binding is a "let" or a "logical".
    Aside this, it add the possible "local" and "rec" flags is needed.

    {b Rem} : Not exported ouside this module.                             *)
(* *********************************************************************** *)
let pp_let_def_binding_flags ppf (ld_rec, ld_logical, ld_local) =
  Format.fprintf ppf "%a%alet %a"
    pp_local_flag ld_local
    pp_logical_flag ld_logical
    pp_rec_flag ld_rec
;;

let rec pp_pat_desc ppf = function
  | Parsetree.P_const cst -> Format.fprintf ppf "%a" pp_constant cst
  | Parsetree.P_var vname -> Format.fprintf ppf "%a" pp_vname vname
  | Parsetree.P_as (pat, vname) ->
      Format.fprintf ppf "%a@ as@ %a" pp_pattern pat pp_vname vname
  | Parsetree.P_wild -> Format.fprintf ppf "_"
  | Parsetree.P_constr (constr_ident, [pat_left; pat_right]) ->
      let ident = match constr_ident.ast_desc with
      | CI ident -> ident in
      let fixitude = ident_fixitude ident in
      if fixitude = Fixitude_infix
      then
        Format.fprintf ppf "@[<2>%a@ %a@ %a@]"
          pp_pattern pat_left
          pp_constructor_ident constr_ident
          pp_pattern pat_right
      else
        Format.fprintf ppf "@[<2>%a@ (%a,@ %a)@]"
          pp_constructor_ident constr_ident
          pp_pattern pat_left
          pp_pattern pat_right
  | Parsetree.P_constr (ident, pats) ->
      Format.fprintf ppf "@[<2>%a@ %a@]"
        pp_constructor_ident ident (pp_patterns ",") pats
  | Parsetree.P_record lab_pat_lst ->
      Format.fprintf ppf "@[<2>{@ %a@ }@])"
        (Handy.pp_generic_separated_list
           ";"
           (fun local_ppf (label, pat) ->
             Format.fprintf local_ppf "%a@ =@ %a"
               pp_label_ident label pp_pattern pat))
        lab_pat_lst
  | Parsetree.P_tuple pats ->
      Format.fprintf ppf "@[<2>%a@]" (pp_patterns ",") pats
  | Parsetree.P_paren pat -> Format.fprintf ppf "@[<1>(%a)@]" pp_pattern pat

and pp_patterns sep ppf = function
  | [] -> ()
  | pats ->
      Format.fprintf ppf "(%a)"
        (Handy.pp_generic_separated_list sep pp_pattern) pats
and pp_pattern ppf = pp_ast pp_pat_desc ppf
;;

let pp_external_language ppf = function
  | Parsetree.EL_Caml -> Format.fprintf ppf "caml@ "
  | Parsetree.EL_Coq -> Format.fprintf ppf "coq@ "
  | Parsetree.EL_external s -> Format.fprintf ppf "%s@ " s
;;

let pp_external_code ppf ecode = Format.fprintf ppf "{*%s*}" ecode
;;

let pp_external_translation_desc ppf transl =
  Format.fprintf ppf "@[<2>@ |@ %a@ @]"
    (Handy.pp_generic_separated_list
       "|"
       (fun local_ppf (ext_lang, ext_code) ->
         Format.fprintf local_ppf "%a@ ->@ %a@ "
           pp_external_language ext_lang pp_external_code ext_code))
    transl
;;

let pp_external_translation ppf = pp_ast pp_external_translation_desc ppf;;

let pp_external_expr_desc ppf = function
  | { ee_internal = ty; ee_external = etrans; } ->
    Format.fprintf ppf "@[<2>internal@ %a@ external@ %a@]"
      pp_type_expr ty
      pp_external_translation etrans
;;

let pp_external_expr e_expr = pp_ast pp_external_expr_desc e_expr;;


let rec pp_species_def_desc ppf def =
  Format.fprintf ppf "@[<2>species %a " pp_vname def.Parsetree.sd_name;
  (* Prints the parameters only if some. *)
  if def.Parsetree.sd_params <> [] then
    begin
    Format.fprintf ppf "(@[<1>%a@])@ "
    (Handy.pp_generic_separated_list
       ","
       (fun local_ppf (vname, species_param_type) ->
         Format.fprintf local_ppf "%a %a"
           pp_vname vname pp_species_param_type species_param_type))
      def.Parsetree.sd_params
    end;
  let pp_inherits ppf () =
    (* Prints the ancestors only if some. *)
    if def.Parsetree.sd_inherits.Parsetree.ast_desc <> [] then
      begin
        Format.fprintf ppf "inherit %a@;;"
          (pp_species_exprs ",") def.Parsetree.sd_inherits.Parsetree.ast_desc
      end
  in
  Format.fprintf ppf " =@ %a@ %a@ end@]@ "
    pp_inherits ()
    pp_species_fields def.Parsetree.sd_fields

and pp_species_def ppf = pp_ast pp_species_def_desc ppf

and pp_species_param_type_desc ppf = function
  | Parsetree.SPT_in ident -> Format.fprintf ppf "in@ %a" pp_ident ident
  | Parsetree.SPT_is species_expr ->
      Format.fprintf ppf "is@ %a" pp_species_expr species_expr
and pp_species_param_type ppf = pp_ast pp_species_param_type_desc ppf

and pp_species_expr_desc ppf sed =
  Format.fprintf ppf "%a" pp_ident sed.Parsetree.se_name;
  if sed.Parsetree.se_params <> [] then
    begin
    Format.fprintf ppf "(@[<1>%a@])"
      (pp_species_params ",") sed.Parsetree.se_params
    end
and pp_species_exprs sep ppf =
  Handy.pp_generic_separated_list sep pp_species_expr ppf
and pp_species_expr ppf = pp_ast pp_species_expr_desc ppf

and pp_species_param_desc ppf = function
  | Parsetree.SP expr -> Format.fprintf ppf "%a" pp_expr expr
and pp_species_params sep ppf =
  Handy.pp_generic_separated_list sep pp_species_param ppf
and pp_species_param ppf = pp_ast pp_species_param_desc ppf

and pp_sig_def_desc ppf sdd =
  Format.fprintf ppf "@[<2>signature@ %a :@ %a@]"
    pp_vname sdd.Parsetree.sig_name pp_type_expr sdd.Parsetree.sig_type
and pp_sig_def ppf = pp_ast pp_sig_def_desc ppf

and pp_proof_def_desc ppf pdd =
  Format.fprintf ppf "@[<2>proof of@ %a@ =@ %a@]"
    pp_vname pdd.Parsetree.pd_name pp_proof pdd.Parsetree.pd_proof
and pp_proof_def ppf = pp_ast pp_proof_def_desc ppf

and pp_termination_proof_def_desc ppf tpdd =
  Format.fprintf ppf "@[<2>termination proof of@ %a@ =@ %a@]"
    pp_termination_proof_profiles tpdd.Parsetree.tpd_profiles
    pp_termination_proof tpdd.Parsetree.tpd_termination_proof
and pp_termination_proof_def ppf = pp_ast pp_termination_proof_def_desc ppf

and pp_termination_proof_profiles ppf =
  Handy.pp_generic_separated_list "," pp_termination_proof_profile ppf

and pp_termination_proof_profile_desc ppf tppd =
  Format.fprintf ppf "@[<2>{@ %a;@ %a@ }@]"
    pp_vname tppd.Parsetree.tpp_name pp_param_list tppd.Parsetree.tpp_params
and pp_termination_proof_profile ppf =
  pp_ast pp_termination_proof_profile_desc ppf

and pp_property_def_desc ppf pdd =
  Format.fprintf ppf "@[<2>property@ %a :@ %a@]"
    pp_vname pdd.Parsetree.prd_name
    pp_logical_expr pdd.Parsetree.prd_logical_expr
and pp_property_def ppf = pp_ast pp_property_def_desc ppf

and pp_properties ppf =
  Handy.pp_generic_separated_list "," pp_expr_ident ppf

and pp_species_field_desc ppf = function
  | Parsetree.SF_rep rep_type_def ->
      Format.fprintf ppf "%a@;;@]" pp_rep_type_def_representation rep_type_def
  | Parsetree.SF_sig sig_def ->
      Format.fprintf ppf "%a@;;" pp_sig_def sig_def
  | Parsetree.SF_let let_def ->
      Format.fprintf ppf "%a@;;" pp_let_def let_def
  | Parsetree.SF_property property_def ->
      Format.fprintf ppf "%a@;;" pp_property_def property_def
  | Parsetree.SF_theorem theorem_def ->
      Format.fprintf ppf "%a@;;" pp_theorem_def theorem_def
  | Parsetree.SF_proof proof_def ->
      Format.fprintf ppf "%a@;;" pp_proof_def proof_def
  | Parsetree.SF_termination_proof termination_proof_def ->
      Format.fprintf ppf "%a@;;" pp_termination_proof_def termination_proof_def

and pp_species_fields ppf =
  Handy.pp_generic_newlined_list pp_species_field ppf
and pp_species_field ppf = pp_ast pp_species_field_desc ppf

and pp_let_def_desc ppf ldd =
  Format.fprintf ppf "@[<2>%a"
    pp_let_def_binding_flags
    (ldd.Parsetree.ld_rec, ldd.Parsetree.ld_logical, ldd.Parsetree.ld_local);
  (* Now print the bindings. This is especially handled because bindings *)
  (* after the first one ar separated by "and" instead of "let".         *)
  (match ldd.Parsetree.ld_bindings with
   | [] ->
       (* The let construct should always at least bind one identifier ! *)
       assert false
   | [one] -> Format.fprintf ppf "%a" pp_binding one
   | first :: nexts ->
       Format.fprintf ppf "%a" pp_binding first;
       List.iter
         (fun b -> Format.fprintf ppf "@]@ @[<2>and %a" pp_binding b)
         nexts);
  Format.fprintf ppf "@]"

and pp_let_def ppf = pp_ast pp_let_def_desc ppf
and pp_let_defs ppf =
  Handy.pp_generic_separated_list ";" pp_let_def ppf

and pp_binding_body ppf = function
  | Parsetree.BB_logical prop -> pp_logical_expr ppf prop
  | Parsetree.BB_computational expr -> pp_expr ppf expr

and pp_binding_desc ppf bd =
  Format.fprintf ppf "%a" pp_bound_vname bd.Parsetree.b_name;
  (* Prints the parameters only if some. *)
  if bd.Parsetree.b_params <> [] then
    (begin
    Format.fprintf ppf "@ (%a)"
      (Handy.pp_generic_separated_list
         ","
         (fun local_ppf (vname, ty_expr_opt) ->
           (* Prints the type only if it is provided. *)
           Format.fprintf local_ppf "%a%a"
             pp_vname vname
             (Handy.pp_generic_option " : " pp_type_expr) ty_expr_opt))
      bd.Parsetree.b_params
    end);
    Format.fprintf ppf "%a@ =@ %a"
      (Handy.pp_generic_option " : " pp_type_expr) bd.Parsetree.b_type
      pp_binding_body bd.Parsetree.b_body

and pp_binding ppf = pp_ast pp_binding_desc ppf

and pp_theorem_def_desc ppf tdd =
  Format.fprintf ppf "@[<2>%atheorem %a :@ %a@ proof =@ %a@]"
    pp_local_flag tdd.Parsetree.th_local
    pp_vname tdd.Parsetree.th_name
    pp_logical_expr tdd.Parsetree.th_stmt
    pp_proof tdd.Parsetree.th_proof
and pp_theorem_def ppf = pp_ast pp_theorem_def_desc ppf

and pp_fact_desc ppf = function
  | Parsetree.F_definition idents ->
      Format.fprintf ppf "definition of %a" (pp_expr_idents ",") idents
  | Parsetree.F_property idents ->
      Format.fprintf ppf "property %a" (pp_expr_idents ",") idents
  | Parsetree.F_hypothesis vnames ->
      Format.fprintf ppf "hypothesis %a" (pp_vnames ",") vnames
  | Parsetree.F_node node_labels ->
      Format.fprintf ppf "step %a" (pp_node_labels ",") node_labels
  | Parsetree.F_type type_idents ->
      Format.fprintf ppf "type %a" (pp_idents ",") type_idents
and pp_facts sep ppf = Handy.pp_generic_separated_list sep pp_fact ppf
and pp_fact ppf = pp_ast pp_fact_desc ppf

and pp_enforced_dependency_desc ppf = function
  | Parsetree.Ed_definition idents ->
      Format.fprintf ppf "definition of %a" (pp_expr_idents ",") idents
  | Parsetree.Ed_property idents ->
      Format.fprintf ppf "property %a" (pp_expr_idents ",") idents
and pp_enforced_dependencies sep ppf =
  Handy.pp_generic_separated_list sep pp_enforced_dependency ppf
and pp_enforced_dependency ppf = pp_ast pp_enforced_dependency_desc ppf

and pp_proof_desc ppf = function
  | Parsetree.Pf_assumed enf_deps ->
      Format.fprintf ppf "@[<2>%a"
        (pp_enforced_dependencies " ") enf_deps;
      if enf_deps <> [] then Format.fprintf ppf " ";
      Format.fprintf ppf "assumed@]"
  | Parsetree.Pf_auto [] ->
      Format.fprintf ppf "@[<2>conclude@]"
  | Parsetree.Pf_auto facts ->
      Format.fprintf ppf "@[<2>by %a@]" (pp_facts "") facts
  | Parsetree.Pf_coq (enf_deps, s) ->
      Format.fprintf ppf "@[<2>coq proof@ %a"
        (pp_enforced_dependencies " ") enf_deps;
      if enf_deps <> [] then Format.fprintf ppf " ";
      Format.fprintf ppf "@ {*%s*}@]" s
  | Parsetree.Pf_node proof_nodes ->
      Format.fprintf ppf "%a" (pp_proof_nodes "") proof_nodes
and pp_proof ppf = pp_ast pp_proof_desc ppf

and pp_termination_proof_desc ppf = function
  | Parsetree.TP_structural vname ->
      Format.fprintf ppf "structural %a" pp_vname vname
  | Parsetree.TP_lexicographic facts ->
      Format.fprintf ppf "@[<2>lexicographic@ %a@]" (pp_facts "") facts
  | Parsetree.TP_measure (expr, param_list, proof) ->
      Format.fprintf ppf "@[<2>measure@ %a@ on %a@ %a@]"
        pp_expr expr pp_param_list param_list pp_proof proof
  | Parsetree.TP_order (expr, param_list, proof) ->
      Format.fprintf ppf "@[<2>order@ %a@ on %a@ %a@]"
        pp_expr expr pp_param_list param_list pp_proof proof
and pp_param_list =
    (Handy.pp_generic_separated_list
       ","
       (fun local_ppf (vname, ty_expr_opt) ->
         Format.fprintf local_ppf "(%a,@ %a)"
           pp_vname vname
           (Handy.pp_generic_explicit_option pp_type_expr) ty_expr_opt))
and pp_termination_proof ppf = pp_ast pp_termination_proof_desc ppf

and pp_proof_node_desc ppf = function
  | Parsetree.PN_sub (node_label, stmt, proof) ->
      Format.fprintf ppf "%a %a@ %a"
        pp_node_label node_label pp_statement stmt pp_proof proof
  | Parsetree.PN_qed (node_label, proof) ->
      Format.fprintf ppf "%a qed@ %a" pp_node_label node_label pp_proof proof
and pp_proof_nodes sep ppf = Handy.pp_generic_separated_list sep proof_node ppf
and proof_node ppf = pp_ast pp_proof_node_desc ppf

and pp_statement_desc ppf stmt =
  match stmt.Parsetree.s_hyps, stmt.Parsetree.s_concl with
  | [], Some logical_expr ->
    Format.fprintf ppf "prove %a" pp_logical_expr logical_expr
  | hyps, concl ->
    Format.fprintf ppf "%a@ %a"
      (pp_hyps "") hyps
      (Handy.pp_generic_option "prove " pp_logical_expr) concl
and pp_statement ppf = pp_ast pp_statement_desc ppf

and pp_hyp_desc ppf = function
  | Parsetree.H_variable (vname, te) ->
      Format.fprintf ppf "@[<2>assume %a :@ %a,@ @]"
        pp_vname vname pp_type_expr te
  | Parsetree.H_hypothesis (vname, prop) ->
      Format.fprintf ppf "@[<2>hypothesis %a :@ %a,@ @]"
        pp_vname vname pp_logical_expr prop
  | Parsetree.H_notation (vname, expr) ->
      Format.fprintf ppf "@[<2>notation %a =@ %a,@ @]"
        pp_vname vname pp_expr expr
and pp_hyps sep ppf = Handy.pp_generic_separated_list sep pp_hyp ppf
and pp_hyp ppf = pp_ast pp_hyp_desc ppf

and pp_logical_expr_desc ppf = function
  | Parsetree.Pr_forall (vnames, type_expr_opt, prop) ->
      Format.fprintf ppf "@[<2>all@ %a@ :@ %a,@ %a@]"
        (pp_vnames "") vnames pp_type_expr type_expr_opt
        pp_logical_expr prop
  | Parsetree.Pr_exists (vnames, type_expr_opt, prop) ->
      Format.fprintf ppf "@[<2>ex@ %a@ :@ %a,@ %a@]"
        (pp_vnames "") vnames pp_type_expr type_expr_opt
        pp_logical_expr prop
  | Parsetree.Pr_imply (p1, p2) ->
      Format.fprintf ppf "@[<2>%a@ ->@ %a@]"
        pp_logical_expr p1 pp_logical_expr p2
  | Parsetree.Pr_or (p1, p2) ->
      Format.fprintf ppf "@[<2>%a@ \\/@ %a@]"
        pp_logical_expr p1 pp_logical_expr p2
  | Parsetree.Pr_and (p1, p2) ->
      Format.fprintf ppf "@[<2>%a@ /\\@ %a@]"
        pp_logical_expr p1 pp_logical_expr p2
  | Parsetree.Pr_equiv (p1, p2) ->
      Format.fprintf ppf "@[<2>%a@ <->@ %a@]"
        pp_logical_expr p1 pp_logical_expr p2
  | Parsetree.Pr_not p ->
      Format.fprintf ppf "@[<2>~@ %a@]" pp_logical_expr p
  | Parsetree.Pr_expr e -> Format.fprintf ppf "%a" pp_expr e
  | Parsetree.Pr_paren p ->
      Format.fprintf ppf "@[<1>(%a)@]" pp_logical_expr p
and pp_logical_expr ppf = pp_ast pp_logical_expr_desc ppf

and pp_expr_desc ppf = function
  | Parsetree.E_self -> Format.fprintf ppf "Self"
  | Parsetree.E_const cst -> Format.fprintf ppf "%a" pp_constant cst
  | Parsetree.E_fun (vnames, expr) ->
      Format.fprintf ppf "@[<2>function %a ->@ %a@]"
        (pp_vnames "") vnames pp_expr expr
  | Parsetree.E_var id -> Format.fprintf ppf "%a" pp_expr_ident id
  | Parsetree.E_app ({ ast_desc = Parsetree.E_var id; ast_loc = _; ast_annot = _; ast_type = _ } as expr, [left; right]) ->
      let fixitude = expr_desc_fixitude expr.Parsetree.ast_desc in
      if fixitude = Fixitude_infix
      then
        Format.fprintf ppf "@[<2>%a@ %a@ %a@]"
          pp_expr left pp_infix_ident id pp_expr right
      else
        Format.fprintf ppf "@[<2>%a@ (%a,@ %a)@]"
          pp_expr expr pp_expr left pp_expr right
  | Parsetree.E_app (expr, exprs) ->
      (begin
      (* Especially handle the case where the functionnal expression is *)
      (* an infix or a prefix identifier. In this case, no application  *)
      (* parens must appear and the operator must be inserted according *)
      (* to its "fixitude".                                             *)
      match expr_desc_fixitude expr.Parsetree.ast_desc with
       | Fixitude_infix ->
           (* Infix operator. *)
           (begin
           match exprs with
            | [first; second] ->
        Format.fprintf ppf "@[<2>%a@ %a@ %a@]"
          pp_expr first pp_expr expr pp_expr second
            | _ -> assert false   (* Infix operators are binary. *)
           end)
       | Fixitude_prefix ->
           (* Prefix operator. *)
           (begin
           match exprs with
            | [one] ->
        Format.fprintf ppf "@[<2>%a@ %a@]" pp_expr expr pp_expr one
            | _ -> assert false   (* Prefix operators are unary. *)
           end)
       | Fixitude_applic ->
           Format.fprintf ppf "@[<2>%a@ (%a)@]"
             pp_expr expr (pp_exprs ",") exprs
      end)
  | Parsetree.E_constr (constr_ident, [expr_left; expr_right]) ->
      let ident = match constr_ident.ast_desc with
      | CI ident -> ident in
      let fixitude = ident_fixitude ident in
      if fixitude = Fixitude_infix
      then
        Format.fprintf ppf "@[<2>%a@ %a@ %a@]"
          pp_expr expr_left pp_constructor_ident constr_ident pp_expr expr_right
      else
        Format.fprintf ppf "@[<2>%a@ (%a,@ %a)@]"
          pp_constructor_ident constr_ident pp_expr expr_left pp_expr expr_right
  | Parsetree.E_constr (constr_ident, exprs) ->
      Format.fprintf ppf "@[<2>%a" pp_constructor_ident constr_ident;
      if exprs <> [] then Format.fprintf ppf "@ (%a)" (pp_exprs ",") exprs;
      Format.fprintf ppf "@]"
  | Parsetree.E_match (expr, pat_exprs) ->
      (begin
      Format.fprintf ppf "@[<2>match@ %a@ with@ " pp_expr expr;
      List.iter
        (fun (pat, e) ->
          Format.fprintf ppf "@[<2>| %a ->@ %a@]" pp_pattern pat pp_expr e)
        pat_exprs;
      Format.fprintf ppf "@]"
      end)
  | Parsetree.E_if (expr1, expr2, expr3) ->
      Format.fprintf ppf "@[<2>if@ %a@ then@ %a@ else@ %a@]"
        pp_expr expr1 pp_expr expr2 pp_expr expr3
  | Parsetree.E_let (let_def, expr) ->
      Format.fprintf ppf "%a@ in@ %a@]" pp_let_def let_def pp_expr expr
  | Parsetree.E_record label_exprs ->
      Format.fprintf ppf "{@[<2>@ %a@ @]}"
        (Handy.pp_generic_separated_list
           ";"
           (fun local_ppf (lab_name, e) ->
             Format.fprintf local_ppf "%a@ = @ %a"
               pp_label_ident lab_name pp_expr e))
        label_exprs
  | Parsetree.E_record_access (expr, label_name) ->
      Format.fprintf ppf "%a.%a" pp_expr expr pp_label_ident label_name
  | Parsetree.E_record_with (expr, label_exprs) ->
      Format.fprintf ppf "{@[<2>@ %a@ with@ %a@ @]}"
        pp_expr expr
        (Handy.pp_generic_separated_list
           ";"
           (fun local_ppf (lab_name, e) ->
             Format.fprintf local_ppf "%a@ =@ %a"
               pp_label_ident lab_name pp_expr e))
        label_exprs
  | Parsetree.E_tuple exprs ->
      Format.fprintf ppf "@[<1>(%a)@]" (pp_exprs ",") exprs
  | Parsetree.E_sequence exprs ->
      Format.fprintf ppf "@[<2>begin@ %a@ end@]" (pp_exprs ";") exprs
  | Parsetree.E_external external_expr ->
      Format.fprintf ppf "@[<2>%a@]"
        pp_external_expr external_expr
  | Parsetree.E_paren expr ->
      Format.fprintf ppf "@[<1>(%a)@]" pp_expr expr
and pp_exprs sep ppf = Handy.pp_generic_separated_list sep pp_expr ppf
and pp_expr ppf = pp_ast pp_expr_desc ppf
;;

let pp_collection_def_desc ppf cdd =
  Format.fprintf ppf "@[<2>collection@ %a =@ implement@ %a;@ end@;@]@ "
    pp_vname cdd.Parsetree.cd_name pp_species_expr cdd.Parsetree.cd_body
;;
let pp_collection_def ppf = pp_ast pp_collection_def_desc ppf
;;

let pp_testing_context_phrase_desc ppf = function
  | Parsetree.TstCtxPh_collection collection_def ->
      Format.fprintf ppf "%a;"
        pp_collection_def collection_def
  | Parsetree.TstCtxPh_let let_def ->
      Format.fprintf ppf "%a;"
        pp_let_def let_def
  | Parsetree.TstCtxPh_property property_def ->
      Format.fprintf ppf "%a;"
        pp_property_def property_def
;;

let pp_testing_context_phrase ppf =
  pp_ast pp_testing_context_phrase_desc ppf
;;

let pp_testing_context ppf =
  Handy.pp_generic_newlined_list pp_testing_context_phrase ppf
;;

let pp_testing_expr ppf tste =
  let tsted = tste.Parsetree.ast_desc in
  Format.fprintf ppf "@[<2>%a@]@;@[<2>testing@ :@;%a@]@;@[<2>parameters@ :@;%a@]@ "
    pp_testing_context tsted.Parsetree.tst_context
    pp_properties tsted.Parsetree.tst_properties
    pp_let_defs tsted.Parsetree.tst_parameters
;;

let pp_testing_def_desc ppf tstdd =
  Format.fprintf ppf "@[<2>testing@ %a =@ %a@ end@;@]@ "
    pp_vname tstdd.Parsetree.tstd_name
    pp_testing_expr tstdd.Parsetree.tstd_body
;;
let pp_testing_def ppf =
  pp_ast pp_testing_def_desc ppf
;;

let pp_tmp_TD_union ppf l =
  Format.fprintf ppf "@[<2>| %a@]"
    (Handy.pp_generic_separated_list
       "|"
       (fun local_ppf (constr_name, type_exprs) ->
         Format.fprintf local_ppf "%a" pp_constr_name constr_name;
         (* Print constructor's arguments if some. *)
         if type_exprs <> [] then
           Format.fprintf ppf " (@[<1>%a@])" (pp_type_exprs ",") type_exprs))
    l
;;

let pp_regular_type_def_body_desc ppf = function
  | Parsetree.RTDB_alias te ->
      Format.fprintf ppf "@[<2>alias@ %a@]" pp_type_expr te
  | Parsetree.RTDB_union l -> Format.fprintf ppf "%a" pp_tmp_TD_union l
  | Parsetree.RTDB_record lab_exprs ->
      Format.fprintf ppf "@[<2>{@ %a@ }@]"
        (Handy.pp_generic_separated_list
           ";"
           (fun local_ppf (lab, e) ->
             Format.fprintf local_ppf "%a@ =@ %a" pp_vname lab pp_type_expr e))
        lab_exprs
;;
let pp_regular_type_def_body ppf =
  pp_ast pp_regular_type_def_body_desc ppf
;;

let pp_external_binding ppf eb =
  let (vname, e_trans) = eb.Parsetree.ast_desc in
  Format.fprintf ppf "%a =@ %a" pp_constr_name vname pp_external_translation e_trans
;;
let pp_external_mapping ppf emap =
  match emap.Parsetree.ast_desc with
  | [] -> ()
  | eb :: ebs ->
    Format.fprintf ppf "@[<2>with %a@]@ " pp_external_binding eb;
    List.iter
      (fun eb ->
        Format.fprintf ppf "@[<2>and %a@]@ " pp_external_binding eb)
      ebs (*.Parsetree.ast_desc*)
;;

let pp_external_type_def_body_desc ppf ex_tydef_body =
  Format.fprintf ppf "internal";
  (match ex_tydef_body.Parsetree.etdb_internal with
   | None -> Format.fprintf ppf "@ "
   | Some regular_tydef_body ->
       Format.fprintf ppf "@ %a@ "
         pp_regular_type_def_body regular_tydef_body);
  Format.fprintf ppf "external@ %a@ "
    pp_external_translation ex_tydef_body.Parsetree.etdb_external;
  Format.fprintf ppf "%a"
    pp_external_mapping ex_tydef_body.Parsetree.etdb_mapping
;;
let pp_external_type_def_body ppf =
  pp_ast pp_external_type_def_body_desc ppf
;;

let pp_type_def_body_simple_desc ppf = function
  | Parsetree.TDBS_regular regular_tydef_body ->
      Format.fprintf ppf "%a" pp_regular_type_def_body regular_tydef_body
  | Parsetree.TDBS_external external_tydef_body ->
      Format.fprintf ppf "%a" pp_external_type_def_body external_tydef_body
;;
let pp_type_def_body_simple ppf = pp_ast pp_type_def_body_simple_desc ppf
;;

let pp_type_def_body_desc ppf = function
  | Parsetree.TDB_abstract tydef_body_simple ->
      Format.fprintf ppf "@[<2>abstract@ ([@ %a@ ])@]"
        pp_type_def_body_simple tydef_body_simple
  | Parsetree.TDB_private tydef_body_simple ->
      Format.fprintf ppf "@[<2>private@ ([@ %a@ ])@]"
        pp_type_def_body_simple tydef_body_simple
  | Parsetree.TDB_public tydef_body_simple ->
      Format.fprintf ppf "@[<2>public@ %a@ @]"
        pp_type_def_body_simple tydef_body_simple
  | Parsetree.TDB_relational tydef_body_simple ->
      Format.fprintf ppf "@[<2>relational@ ([@ %a@ ])@]"
        pp_type_def_body_simple tydef_body_simple
;;
let pp_type_def_body ppf = pp_ast pp_type_def_body_desc ppf;;

let pp_type_def_desc ppf td =
  (* The type's name. *)
  Format.fprintf ppf "@[<2>type %a " pp_vname td.Parsetree.td_name;
  (* Print type's parameters if some. *)
  if td.Parsetree.td_params <> [] then
    Format.fprintf ppf "(%a) "
      (Handy.pp_generic_separated_list
         ","
         (fun local_ppf s -> Format.fprintf local_ppf "%a" pp_vname s))
      td.Parsetree.td_params;
  Format.fprintf ppf "=@ %a@]" pp_type_def_body td.Parsetree.td_body
;;
let pp_type_def ppf = pp_ast pp_type_def_desc ppf;;

let rec pp_phrase_desc ppf = function
  | Parsetree.Ph_annotation_title -> ()
  | Parsetree.Ph_use fname ->
      Format.fprintf ppf "@[<2>use@ \"%s\";;@]" fname
  | Parsetree.Ph_open fname ->
      Format.fprintf ppf "@[<2>open@ \"%s\";;@]" fname
  | Parsetree.Ph_coq_require fname ->
      Format.fprintf ppf "@[<2>coq_require@ \"%s\";;@]" fname
  | Parsetree.Ph_species s_def ->
      Format.fprintf ppf "%a;;" pp_species_def s_def
  | Parsetree.Ph_collection collection_def ->
      Format.fprintf ppf "%a;;" pp_collection_def collection_def
  | Parsetree.Ph_testing testing_def ->
      Format.fprintf ppf "%a;;"
        pp_testing_def testing_def
  | Parsetree.Ph_type type_def ->
      Format.fprintf ppf "%a;;" pp_type_def type_def
  | Parsetree.Ph_let let_def -> Format.fprintf ppf "%a;;" pp_let_def let_def
  | Parsetree.Ph_theorem t_def ->
      Format.fprintf ppf "%a;;" pp_theorem_def t_def
  | Parsetree.Ph_expr expr -> Format.fprintf ppf "%a;;" pp_expr expr
and pp_phrase ppf = pp_ast pp_phrase_desc ppf
and pp_phrases ppf = Handy.pp_generic_newlined_list pp_phrase ppf
;;

let pp_file_desc ppf = function
  | Parsetree.File phrases -> Format.fprintf ppf "%a@." pp_phrases phrases
;;
let pp_file ppf = pp_ast pp_file_desc ppf
;;

let pp_simple_species_expr_as_effective_parameter ppf = function
  | Parsetree_utils.SPE_Self -> Format.fprintf ppf "Self"
  | Parsetree_utils.SPE_Species (qvname, _) ->
      Format.fprintf ppf "%a" pp_qualified_vname qvname
  | Parsetree_utils.SPE_Expr_entity expr ->
      Format.fprintf ppf "%a" pp_expr expr
;;

let pp_simple_species_expr ppf sse =
  Format.fprintf ppf "@[<2>%a" pp_ident sse.Parsetree_utils.sse_name;
  if sse.Parsetree_utils.sse_effective_args <> [] then
    (begin
    Format.fprintf ppf "@ (%a)"
      (Handy.pp_generic_separated_list
         "," pp_simple_species_expr_as_effective_parameter)
      sse.Parsetree_utils.sse_effective_args
    end);
  Format.fprintf ppf "@]"
;;

