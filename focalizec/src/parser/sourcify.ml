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

(* $Id: sourcify.ml,v 1.50 2008-08-18 08:46:07 pessaux Exp $ *)


(* *********************************************************** *)
(* Format.formatter -> Parsetree.vname -> unit                 *)
(** {b Descr} : Pretty prints a [vname] value as FoCal source.

    {b Rem} : Exported ouside this module.                     *)
(* *********************************************************** *)
let pp_vname ppf = function
  | Parsetree.Vlident s -> Format.fprintf ppf "%s" s
  | Parsetree.Vuident s -> Format.fprintf ppf "%s" s
  | Parsetree.Vpident s -> Format.fprintf ppf "%s" s
  | Parsetree.Viident s -> Format.fprintf ppf "%s" s
  | Parsetree.Vqident s -> Format.fprintf ppf "%s" s
;;
(* ******************************************************************** *)
(* string -> Format.formatter -> Parsetree.vname list -> unit           *)
(** {b Descr} : Pretty prints a [list] of [vname] value as FoCal source.

    {b Rem} : Not exported ouside this module.                          *)
(* ******************************************************************** *)
let pp_vnames sep ppf = Handy.pp_generic_separated_list sep pp_vname ppf
;;



(* *************************************************************** *)
(* pp_node_label : Format.formatter -> int * string -> unit        *)
(** {b Descr} : Pretty prints a [node_label] value as FoCal source.

    {b Rem} : Not exported ouside this module.                     *)
(* *************************************************************** *)
let pp_node_label ppf (i, s) = Format.fprintf ppf "<%d>%s" i s
;;
(* ************************************************************************** *)
(* pp_node_labels :                                                           *)
(*   string -> Format.formatter -> (int * string) list -> unit                *)
(** {b Descr} : Pretty prints a [list] of [node_label] value as FoCal source.

    {b Rem} : Not exported ouside this module.                                *)
(* ************************************************************************** *)
let pp_node_labels sep ppf =
  Handy.pp_generic_separated_list sep pp_node_label ppf
;;



(* ******************************************************************* *)
(* pp_ast :                                                            *)
(*   (Format.formatter -> 'a -> unit) ->                               *)
(*     Format.formatter -> ('a, string) Parsetree.ast -> unit          *)
(** {b Descr} : Wrapper to apply pretty-printing only on the 'ast_doc'
    and 'ast_desc' fields of a ast. Ignores all other fields.

    {b Rem} : Not exported ouside this module.                         *)
(* ******************************************************************* *)
let pp_ast desc_printer_fct ppf ast =
  let print_doc_elem ppf de =
    Format.fprintf ppf "(** %s *)@ " de.Parsetree.de_desc in
  let print_doc_elems ppf des =
      List.iter (fun d -> Format.fprintf ppf "%a@ " print_doc_elem d) des in
  let print_documentation = function
    | [] -> ()
    | docs -> Format.fprintf ppf "@[<v>%a@]" print_doc_elems docs in
  (* First, print the documentation if some exists. *)
  print_documentation ast.Parsetree.ast_doc;
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
(* *********************************************************** *)
(* Format.formatter -> Parsetree.ident- > unit                 *)
(** {b Descr} : Pretty prints a [ident] value as FoCal source.

    {b Rem} : Exported ouside this module.                     *)
(* *********************************************************** *)
let pp_ident ppf = pp_ast pp_ident_desc ppf
;;
(* ********************************************************************* *)
(* string -> Format.formatter -> Parsetree.ident list -> unit            *)
(** {b Descr} : Pretty prints a [list] of [ident] value as FoCal source.

    {b Rem} : Not exported ouside this module.                           *)
(* ********************************************************************* *)
let pp_idents sep ppf = Handy.pp_generic_separated_list sep pp_ident ppf
;;



(* ********************************************************** *)
(* Format.formatter -> Parsetree.label_ident_desc -> unit     *)
(** {b Descr} : Pretty prints a [label_ident_desc] value as a
    Caml-like structure.

    {b Rem} : Not exported ouside this module.                 *)
(* *********************************************************** *)
let pp_label_ident_desc ppf = function
  | Parsetree.LI qvname ->
    Format.fprintf ppf "%a" pp_qvname qvname
;;
(* *************************************************************** *)
(* Format.formatter -> Parsetree.label_ident -> unit               *)
(** {b Descr} : Pretty prints a [label_ident] value as a Caml-like
       structure.

    {b Rem} : Not exported ouside this module.                     *)
(* *************************************************************** *)
let pp_label_ident ppf = pp_ast pp_label_ident_desc ppf;;



(* ********************************************************************** *)
(* Format.formatter -> Parsetree.expr_ident_desc -> unit                  *)
(** {b Descr} : Pretty prints a [expr_ident_desc] value as a FoCal source.

    {b Rem} : Not exported ouside this module.                            *)
(* ********************************************************************** *)
let pp_expr_ident_desc ppf = function
  | Parsetree.EI_local vname -> Format.fprintf ppf "%a" pp_vname vname
  | Parsetree.EI_global qvname ->
    Format.fprintf ppf "%a" pp_qvname qvname
  | Parsetree.EI_method (None, vname) ->
    Format.fprintf ppf "!%a" pp_vname vname
  | Parsetree.EI_method (Some coll_qvname, vname) ->
    Format.fprintf ppf "%a!%a" pp_qvname coll_qvname pp_vname vname
;;
(* **************************************************************** *)
(* Format.formatter -> Parsetree.expr_ident- > unit                 *)
(** {b Descr} : Pretty prints a [expr_ident] value as FoCal source.

    {b Rem} : Exported ouside this module.                          *)
(* **************************************************************** *)
let pp_expr_ident ppf = pp_ast pp_expr_ident_desc ppf
;;
(* ************************************************************************** *)
(* string -> Format.formatter -> Parsetree.expr_ident list -> unit            *)
(** {b Descr} : Pretty prints a [list] of [expr_ident] value as FoCal source.

    {b Rem} : Not exported ouside this module.                                *)
(* ************************************************************************** *)
let pp_expr_idents sep ppf =
  Handy.pp_generic_separated_list sep pp_expr_ident ppf
;;



let pp_constructor_ident_desc ppf (Parsetree.CI qvname) =
  Format.fprintf ppf "%a" pp_qvname qvname
;;
let pp_constructor_ident ppf = pp_ast pp_constructor_ident_desc ppf
;;



(* ********************************************************************** *)
(*  [Type] expr_desc_fixitude                                             *)
(** [Desc] : Describe wether an [expr_desc] must appears in infix of
    prefix position as the functionnal part in an applicative expression.
    If the [expr_desc] is not an identifier expression, then is it
    considered as having an unspecified "fixitude".

    {b Rem} : Not exported ouside this module.                            *)
(* ********************************************************************** *)
type expr_desc_fixitude =
  | Fixitude_prefix     (* The functionnal expression is a prefix identifier. *)
  | Fixitude_infix      (* The functionnal expression is an infix identifier. *)
  | Fixitude_applic     (* The functionnal expression is not an identifier or *)
                        (* is neither infix nor prefix.                       *)
;;



(* ************************************************************************* *)
(* expr_desc_fixitude : Parsetree.expr_desc -> expr_desc_fixitude            *)
(** {b Descr} : Checks wether an [expr_desc] is a legal binary or unary
    identifier, that can and must appear in infix or prefix  position as the
    functionnal part in an applicative expression. If the [expr_desc] is not
    an identifier expression, then it is considered as having an unspecified
    "fixitude". Same thing if the [expr_desc] is an identifier but is a
    [I_global] or [I_method] using an explicit scope information (in this
    case, it must always be syntactically used in a regular application way).

    {b Rem} : Not exported ouside this module.                               *)
(* ************************************************************************* *)
let expr_desc_fixitude = function
  | Parsetree.E_var id ->
      (* Discriminate according to the lexical tag. *)
      let fixitude_of_vname = function
        | Parsetree.Vpident _ -> Fixitude_prefix
        | Parsetree.Viident _ -> Fixitude_infix
        | Parsetree.Vlident _
        | Parsetree.Vuident _
        | Parsetree.Vqident _ -> Fixitude_applic in
      begin match id.Parsetree.ast_desc with
      | Parsetree.EI_local vname -> fixitude_of_vname vname
      | Parsetree.EI_global (Parsetree.Vname vname) -> fixitude_of_vname vname
      | Parsetree.EI_global (Parsetree.Qualified (_, _)) ->
          Fixitude_applic  (* So can't be printed as a syntactic operator. *)
      | Parsetree.EI_method (None, vname) -> fixitude_of_vname vname
      | Parsetree.EI_method (Some _, _) -> Fixitude_applic
      end
  | _ -> Fixitude_applic
;;



(* *********************************************************************** *)
(* pp_rep_type_def_desc :                                                  *)
(*   Format.formatter -> Parsetree.rep_type_def_desc -> unit               *)
(** {b Descr} : Pretty prints a [rep_type_def_desc] value as FoCal source.

    {b Rem} : Not exported ouside this module.                             *)
(* *********************************************************************** *)
let rec pp_rep_type_def_desc ppf = function
  | Parsetree.RTE_ident ident -> Format.fprintf ppf "%a" pp_ident ident
  | Parsetree.RTE_fun (rtd1, rtd2) ->
      Format.fprintf ppf "@[<2>%a@ ->@ %a@]"
        pp_rep_type_def rtd1 pp_rep_type_def rtd2
  | Parsetree.RTE_app (ident, rtds) ->
      Format.fprintf ppf "%a@[<2>@ (%a)@]"
        pp_ident ident (pp_rep_type_defs ",") rtds
  | Parsetree.RTE_prod rtds ->
      Format.fprintf ppf "@[<2>(%a)@]" (pp_rep_type_defs "*") rtds
  | Parsetree.RTE_paren rtd -> Format.fprintf ppf "(%a)" pp_rep_type_def rtd
(* ******************************************************************** *)
(* pp_rep_type_defs :                                                   *)
(*   string -> Format.formatter -> Parsetree.rep_type_def list -> unit  *)
(** {b Descr} : Pretty prints a [list] of [rep_type_def] value as FoCal
    source.

    {b Rem} : Not exported ouside this module.                          *)
(* ******************************************************************** *)
and pp_rep_type_defs sep ppf =
  Handy.pp_generic_separated_list sep pp_rep_type_def ppf
(* ****************************************************************** *)
(* pp_rep_type_def :                                                  *)
(*   Format.formatter -> Parsetree.rep_type_def -> unit               *)
(** {b Descr} : Pretty prints a [rep_type_def] value as FoCal source.

    {b Rem} : Not exported ouside this module.                        *)
(* ****************************************************************** *)
and pp_rep_type_def ppf = pp_ast pp_rep_type_def_desc ppf
;;



(* ******************************************************************** *)
(* int -> Format.formatter -> Parsetree.type_expr_desc -> unit          *)
(** {b Descr} : Pretty prints a [type_expr_desc] value as FoCal source.

    {b Rem} : Not exported ouside this module.                          *)
(* ******************************************************************** *)
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
        Format.fprintf ppf "@[<2>(%a)@]" (pp_type_exprs_with_prio 0 " *") tes
    | Parsetree.TE_self -> Format.fprintf ppf "Self"
    | Parsetree.TE_prop -> Format.fprintf ppf "prop"
    | Parsetree.TE_paren te ->
        Format.fprintf ppf "(%a)" (pp_type_expr_with_prio 0) te
(* ************************************************************************** *)
(* int -> string -> Format.formatter -> Parsetree.type_expr list -> unit      *)
(** {b Descr} : Pretty prints a [list] of [type_expr] value as FoCal source
    using the given current priority.
    This function is ONLY aimed to be used internally by [pp_type_expr_desc].

    {b Rem} : Not exported ouside this module.
    NEVER call somewhere else than in [pp_type_expr_desc]. This
    is internal stuff !                                                       *)
(* ************************************************************************** *)
and pp_type_exprs_with_prio prio sep ppf =
  Handy.pp_generic_separated_list sep (pp_type_expr_with_prio prio) ppf
(* ******************************************************************* *)
(* int -> string -> Format.formatter -> Parsetree.type_expr -> unit    *)
(** {b Descr} : Pretty prints a [type_expr] value as FoCal source
    using the given current priority.
    This function is ONLY aimed to be used internally by
    [pp_type_expr_desc], [pp_type_exprs_with_prio] and[pp_type_exprs].

    {b Rem} : Not exported ouside this module.
    NEVER call somewhere else than in [pp_type_expr_desc],
    [pp_type_exprs_with_prio] and [pp_type_exprs].
    is internal stuff !                                                *)
(* ******************************************************************* *)
and pp_type_expr_with_prio prio ppf =
  pp_ast (pp_type_expr_desc prio) ppf
(* ************************************************************************* *)
(*  int -> string -> Format.formatter -> Parsetree.type_expr list -> unit    *)
(** {b Descr} : Pretty prints a [list] of [type_expr] value as FoCal source.
    The initial priority is 0, hence this function is initial pretty print
    entry point for 1 list of type expressions.

    {b Rem} : Not exported ouside this module.                               *)
(* ************************************************************************* *)
and pp_type_exprs sep ppf =
  Handy.pp_generic_separated_list sep (pp_type_expr_with_prio 0) ppf
(* ***************************************************************** *)
(* pp_type_expr : Format.formatter -> Parsetree.type_expr -> unit    *)
(** {b Descr} : Pretty prints a [type_expr] value as FoCal source.
    The initial priority is 0, hence this function is initial pretty
    print entry point for 1 type expression.

    {b Rem} : Not exported ouside this module.                       *)
(* ***************************************************************** *)
and pp_type_expr ppf = pp_ast (pp_type_expr_desc 0) ppf
;;



(* ******************************************************************* *)
(* pp_constant_desc :                                                  *)
(*   Format.formatter -> Parsetree.constant_desc -> unit               *)
(** {b Descr} : Pretty prints a [constant_desc] value as FoCal source.

    {b Rem} : Not exported ouside this module.                         *)
(* ******************************************************************* *)
let pp_constant_desc ppf = function
  | Parsetree.C_int s | Parsetree.C_float s | Parsetree.C_bool s ->
      Format.fprintf ppf "%s" s
  | Parsetree.C_string s -> Format.fprintf ppf "\"%s\"" s
  | Parsetree.C_char c ->
      let tmp_s = " " in
      tmp_s.[0] <- c;
      Format.fprintf ppf "%s" tmp_s
;;
(* ************************************************************** *)
(* pp_constant : Format.formatter -> Parsetree.constant -> unit   *)
(** {b Descr} : Pretty prints a [constant] value as FoCal source.

    {b Rem} : Not exported ouside this module.                    *)
(* ************************************************************** *)
let pp_constant = pp_ast pp_constant_desc
;;



(* ************************************************************** *)
(* pp_loc_flag : Format.formatter -> Parsetree.loc_flag -> unit   *)
(** {b Descr} : Pretty prints a [loc_flag] value as FoCal source.

    {b Rem} : Not exported ouside this module.                    *)
(* ************************************************************** *)
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


(* ************************************************************** *)
(* pp_pat_desc : Format.formatter -> Parsetree.pat_desc -> unit   *)
(** {b Descr} : Pretty prints a [pat_desc] value as FoCal source.

    {b Rem} : Not exported ouside this module.                    *)
(* ************************************************************** *)
let rec pp_pat_desc ppf = function
  | Parsetree.P_const cst -> Format.fprintf ppf "%a" pp_constant cst
  | Parsetree.P_var vname -> Format.fprintf ppf "%a" pp_vname vname
  | Parsetree.P_as (pat, vname) ->
      Format.fprintf ppf "%a@ as@ %a" pp_pattern pat pp_vname vname
  | Parsetree.P_wild -> Format.fprintf ppf "_"
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
      Format.fprintf ppf "@[<2>(%a)@]" (pp_patterns ",") pats
  | Parsetree.P_paren pat -> Format.fprintf ppf "@[<1>(%a)@]" pp_pattern pat
(* *********************************************************************** *)
(* pp_patterns :                                                           *)
(*   string -> Format.formatter -> Parsetree.pattern list -> unit          *)
(** {b Descr} : Pretty prints a [list] of [pattern] value as FoCal source.

    {b Rem} : Not exported ouside this module.                             *)
(* *********************************************************************** *)
and pp_patterns sep ppf = Handy.pp_generic_separated_list sep pp_pattern ppf
(* ************************************************************* *)
(* pp_pattern : Format.formatter -> Parsetree.pattern -> unit    *)
(** {b Descr} : Pretty prints a [pattern] value as FoCal source.

    {b Rem} : Not exported ouside this module.                   *)
(* ************************************************************* *)
and pp_pattern ppf = pp_ast pp_pat_desc ppf
;;



(* *********************************************************************** *)
(* pp_external_language :                                                  *)
(*   Format.formatter -> Parsetree.external_language -> unit               *)
(** {b Descr} : Pretty prints a [external_language] value as FoCal source.

    {b Rem} : Not exported ouside this module.                             *)
(* *********************************************************************** *)
let pp_external_language ppf = function
  | Parsetree.EL_Caml -> Format.fprintf ppf "caml@ "
  | Parsetree.EL_Coq -> Format.fprintf ppf "coq@ "
  | Parsetree.EL_external s -> Format.fprintf ppf "%s@ " s
;;



(* ************************************************************************* *)
(* pp_external_expression :                                                  *)
(*   Format.formatter -> Parsetree.external_expression -> unit               *)
(** {b Descr} : Pretty prints a [external_expression] value as FoCal source.

    {b Rem} : Not exported ouside this module.                               *)
(* ************************************************************************* *)
let pp_external_expression ppf eexpr = Format.fprintf ppf "\"%s\"" eexpr
;;



(* ************************************************************************ *)
(*   Format.formatter -> Parsetree.external_expr_desc -> unit               *)
(** {b Descr} : Pretty prints a [external_expr_desc] value as FoCal source.

    {b Rem} : Not exported ouside this module.                              *)
(* ************************************************************************ *)
let pp_external_expr_desc ppf lst =
  Format.fprintf ppf "@[<2>@ |@ %a@ @]"
    (Handy.pp_generic_separated_list
       "|"
       (fun local_ppf (ext_lang, ext_expr) ->
         Format.fprintf local_ppf "%a@ ->@ %a@ "
           pp_external_language ext_lang pp_external_expression ext_expr))
    lst
;;
(* ******************************************************************* *)
(* pp_external_expr :                                                  *)
(*   Format.formatter -> Parsetree.external_expr -> unit               *)
(** {b Descr} : Pretty prints a [external_expr] value as FoCal source.

    {b Rem} : Not exported ouside this module.                         *)
(* ******************************************************************* *)
let pp_external_expr ppf = pp_ast pp_external_expr_desc ppf;;



(* ********************************************************************** *)
(* pp_species_def_desc :                                                  *)
(*   Format.formatter -> Parsetree.species_def_desc -> unit               *)
(** {b Descr} : Pretty prints a [species_def_desc] value as FoCal source.

    {b Rem} : Not exported ouside this module.                            *)
(* ********************************************************************** *)
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
  (* Prints the ancestors only if some. *)
  if def.Parsetree.sd_inherits.Parsetree.ast_desc <> [] then
    begin
    Format.fprintf ppf "inherits %a"
      (pp_species_exprs ",") def.Parsetree.sd_inherits.Parsetree.ast_desc
    end;
  Format.fprintf ppf " =@\n%a@\nend;;@]@\n"
    pp_species_fields def.Parsetree.sd_fields
(* ***************************************************************** *)
(* pp_species_def :                                                  *)
(*   Format.formatter -> Parsetree.species_def -> unit               *)
(** {b Descr} : Pretty prints a [species_def] value as FoCal source.

    {b Rem} : Not exported ouside this module.                       *)
(* ***************************************************************** *)
and pp_species_def ppf = pp_ast pp_species_def_desc ppf



(* ********************************************************************* *)
(* pp_species_param_type_desc :                                          *)
(*   Format.formatter -> Parsetree.species_param_type_desc -> unit       *)
(** {b Descr} : Pretty prints a [species_param_type_desc] value as FoCal
    source.

    {b Rem} : Not exported ouside this module.                           *)
(* ********************************************************************* *)
and pp_species_param_type_desc ppf = function
  | Parsetree.SPT_in ident -> Format.fprintf ppf "in@ %a" pp_ident ident
  | Parsetree.SPT_is species_expr ->
      Format.fprintf ppf "is@ %a" pp_species_expr species_expr
(* ********************************************************************* *)
(* pp_species_param_type :                                               *)
(*   Format.formatter -> Parsetree.species_param_type -> unit            *)
(** {b Descr} : Pretty prints a [species_param_type_desc] value as FoCal
    source.

    {b Rem} : Not exported ouside this module.                           *)
(* ********************************************************************* *)
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
    pp_termination_proof_profiles tpdd.Parsetree.tpd_profiles pp_termination_proof tpdd.Parsetree.tpd_termination_proof
and pp_termination_proof_def ppf = pp_ast pp_termination_proof_def_desc ppf

and pp_termination_proof_profiles ppf =
  Handy.pp_generic_separated_list "," pp_termination_proof_profile ppf

and pp_termination_proof_profile_desc ppf tppd =
  Format.fprintf ppf "@[<2>{@ %a;@ %a@ }@]"
    pp_vname tppd.Parsetree.tpp_name pp_param_list tppd.Parsetree.tpp_args
and pp_termination_proof_profile ppf = pp_ast pp_termination_proof_profile_desc ppf



and pp_property_def_desc ppf pdd =
  Format.fprintf ppf "@[<2>property@ %a :@ %a@]"
    pp_vname pdd.Parsetree.prd_name pp_logical_expr pdd.Parsetree.prd_logical_expr
and pp_property_def ppf = pp_ast pp_property_def_desc ppf



and pp_species_field_desc ppf = function
  | Parsetree.SF_rep rep_type_def ->
      Format.fprintf ppf "@[<2>rep@ =@ %a@;@]" pp_rep_type_def rep_type_def
  | Parsetree.SF_sig sig_def ->
      Format.fprintf ppf "%a@;" pp_sig_def sig_def
  | Parsetree.SF_let let_def ->
      Format.fprintf ppf "%a@;" pp_let_def let_def
  | Parsetree.SF_property property_def ->
      Format.fprintf ppf "%a@;" pp_property_def property_def
  | Parsetree.SF_theorem theorem_def ->
      Format.fprintf ppf "%a@;" pp_theorem_def theorem_def
  | Parsetree.SF_proof proof_def ->
      Format.fprintf ppf "%a@;" pp_proof_def proof_def
  | Parsetree.SF_termination_proof termination_proof_def ->
      Format.fprintf ppf "%a@;" pp_termination_proof_def termination_proof_def
and pp_species_fields ppf = Handy.pp_generic_newlined_list pp_species_field ppf
and pp_species_field ppf = pp_ast pp_species_field_desc ppf



(* ******************************************************************** *)
(* pp_let_def_desc : Format.formatter -> Parsetree.let_def_desc -> unit *)
(** {b Descr} : Pretty prints a [let_def_desc] value as FoCal source.

    {b Rem} : Not exported ouside this module.                          *)
(* ******************************************************************** *)
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
         (fun b -> Format.fprintf ppf "@]@\n@[<2>and %a" pp_binding b)
         nexts);
  Format.fprintf ppf "@]"
(* ************************************************************* *)
(* pp_let_def : Format.formatter -> Parsetree.let_def -> unit    *)
(** {b Descr} : Pretty prints a [let_def] value as FoCal source.

    {b Rem} : Not exported ouside this module.                   *)
(* ************************************************************* *)
and pp_let_def ppf = pp_ast pp_let_def_desc ppf



(* ****************************************************************** *)
(* Format.formatter -> Parsetree.binding_body -> unit                 *)
(** {b Descr} : Pretty prints a [binding_body] value as FoCal source.

    {b Rem} : Not exported ouside this module.                        *)
(* ****************************************************************** *)
and pp_binding_body ppf = function
  | Parsetree.BB_logical prop -> pp_logical_expr ppf prop
  | Parsetree.BB_computational expr -> pp_expr ppf expr



(* ****************************************************************** *)
(* pp_binding_desc : Format.formatter -> Parsetree.binding_desc ->    *)
(*          unit                                                      *)
(** {b Descr} : Pretty prints a [binding_desc] value as FoCal source.

    {b Rem} : Not exported ouside this module.                        *)
(* ****************************************************************** *)
and pp_binding_desc ppf bd =
  Format.fprintf ppf "%a" pp_vname bd.Parsetree.b_name;
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
             (Handy.pp_generic_option " in " pp_type_expr) ty_expr_opt))
      bd.Parsetree.b_params
    end);
    Format.fprintf ppf "%a@ =@ %a"
      (Handy.pp_generic_option " in " pp_type_expr) bd.Parsetree.b_type
      pp_binding_body bd.Parsetree.b_body
(* ************************************************************* *)
(* pp_binding : Format.formatter -> Parsetree.binding -> unit    *)
(** {b Descr} : Pretty prints a [binding] value as FoCal source.

    {b Rem} : Not exported ouside this module.                   *)
(* ************************************************************* *)
and pp_binding ppf = pp_ast pp_binding_desc ppf



(* ********************************************************************** *)
(* pp_theorem_def_desc :                                                  *)
(*   Format.formatter -> Parsetree.theorem_def_desc -> unit               *)
(** {b Descr} : Pretty prints a [theorem_def_desc] value as FoCal source.

    {b Rem} : Not exported ouside this module.                            *)
(* ********************************************************************** *)
and pp_theorem_def_desc ppf tdd =
  Format.fprintf ppf "@[<2>theorem %a :@ %a@ %a@\nproof:@ %a@]"
    pp_vname tdd.Parsetree.th_name
    pp_local_flag tdd.Parsetree.th_local
    pp_logical_expr tdd.Parsetree.th_stmt
    pp_proof tdd.Parsetree.th_proof
(* ****************************************************************** *)
(* pp_theorem_def : Format.formatter -> Parsetree.theorem_def -> unit *)
(** {b Descr} : Pretty prints a [theorem_def] value as FoCal source.

    {b Rem} : Not exported ouside this module.                        *)
(* ****************************************************************** *)
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
and pp_facts sep ppf = Handy.pp_generic_separated_list sep pp_fact ppf
and pp_fact ppf = pp_ast pp_fact_desc ppf



and pp_proof_desc ppf = function
  | Parsetree.Pf_assumed reason ->
      Format.fprintf ppf "@[<2>assumed@ {*%s*}@]" reason
  | Parsetree.Pf_auto facts ->
      Format.fprintf ppf "@[<2>by %a@]" (pp_facts "") facts
  | Parsetree.Pf_coq s -> Format.fprintf ppf "@[<2>coq proof@ {*%s*}@]" s
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
      Format.fprintf ppf "%a %a@\n%a"
        pp_node_label node_label pp_statement stmt pp_proof proof
  | Parsetree.PN_qed (node_label, proof) ->
      Format.fprintf ppf "%a qed@\n%a" pp_node_label node_label pp_proof proof
and pp_proof_nodes sep ppf = Handy.pp_generic_separated_list sep proof_node ppf
and proof_node ppf = pp_ast pp_proof_node_desc ppf



and pp_statement_desc ppf stmt =
  Format.fprintf ppf "%a@ %a"
    (pp_hyps "") stmt.Parsetree.s_hyps
    (Handy.pp_generic_option "prove " pp_logical_expr)
    stmt.Parsetree.s_concl
and pp_statement ppf = pp_ast pp_statement_desc ppf



and pp_hyp_desc ppf = function
  | Parsetree.H_variable (vname, te) ->
      Format.fprintf ppf "@[<2>assume %a in@ %a,@ @]"
        pp_vname vname pp_type_expr te
  | Parsetree.H_hypothesis (vname, prop) ->
      Format.fprintf ppf "@[<2>assume %a :@ %a,@ @]"
        pp_vname vname pp_logical_expr prop
  | Parsetree.H_notation (vname, expr) ->
      Format.fprintf ppf "@[<2>notation %a =@ %a,@ @]"
        pp_vname vname pp_expr expr
and pp_hyps sep ppf = Handy.pp_generic_separated_list sep pp_hyp ppf
and pp_hyp ppf = pp_ast pp_hyp_desc ppf



and pp_logical_expr_desc ppf = function
  | Parsetree.Pr_forall (vnames, type_expr_opt, prop) ->
      Format.fprintf ppf "@[<2>all@ %a@ in@ %a,@ %a@]"
        (pp_vnames "") vnames pp_type_expr type_expr_opt
        pp_logical_expr prop
  | Parsetree.Pr_exists (vnames, type_expr_opt, prop) ->
      Format.fprintf ppf "@[<2>ex@ %a@ in@ %a,@ %a@]"
        (pp_vnames "") vnames pp_type_expr type_expr_opt
        pp_logical_expr prop
  | Parsetree.Pr_imply (p1, p2) ->
      Format.fprintf ppf "@[<2>%a@ ->@ (%a)@]"
        pp_logical_expr p1 pp_logical_expr p2
  | Parsetree.Pr_or (p1, p2) ->
      Format.fprintf ppf "@[<2>(%a@ or@ %a)@]"
        pp_logical_expr p1 pp_logical_expr p2
  | Parsetree.Pr_and (p1, p2) ->
      Format.fprintf ppf "@[<2>%a@ and@ %a@]"
        pp_logical_expr p1 pp_logical_expr p2
  | Parsetree.Pr_equiv (p1, p2) ->
      Format.fprintf ppf "@[<2>%a@ <->@ %a@]"
        pp_logical_expr p1 pp_logical_expr p2
  | Parsetree.Pr_not p ->
      Format.fprintf ppf "@[<2>not@ %a@]" pp_logical_expr p
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
  | Parsetree.E_constr (cstr_expr, exprs) ->
      Format.fprintf ppf "@[<2>%a" pp_constructor_ident cstr_expr;
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
  | Parsetree.E_external external_expr ->
      Format.fprintf ppf "@[<2>external@\n%a@\nend@]"
        pp_external_expr external_expr
  | Parsetree.E_paren expr ->
      Format.fprintf ppf "@[<1>(%a)@]" pp_expr expr
and pp_exprs sep ppf = Handy.pp_generic_separated_list sep pp_expr ppf
and pp_expr ppf = pp_ast pp_expr_desc ppf
;;



let pp_collection_def_desc ppf cdd =
  Format.fprintf ppf "@[<2>collection@ %a@ implements@ %a@\nend@;;@]@\n"
    pp_vname cdd.Parsetree.cd_name pp_species_expr cdd.Parsetree.cd_body
;;
let pp_collection_def ppf = pp_ast pp_collection_def_desc ppf
;;



let pp_tmp_TD_union ppf l =
  Format.fprintf ppf "@[<2>| %a@]"
    (Handy.pp_generic_separated_list
       "|"
       (fun local_ppf (constr_name, type_exprs) ->
         Format.fprintf local_ppf "%a" pp_vname constr_name;
         (* Print constructor's arguments if some. *)
         if type_exprs <> [] then
           Format.fprintf ppf " (@[<1>%a@])" (pp_type_exprs " *") type_exprs))
    l
;;



let pp_simple_type_def_body_desc ppf = function
  | Parsetree.STDB_alias te ->
      Format.fprintf ppf "@[<2>alias@ %a@]" pp_type_expr te
  | Parsetree.STDB_union l -> Format.fprintf ppf "%a" pp_tmp_TD_union l
  | Parsetree.STDB_record lab_exprs ->
      Format.fprintf ppf "@[<2>{@ %a@ }@]"
        (Handy.pp_generic_separated_list
           ";"
           (fun local_ppf (lab, e) ->
             Format.fprintf local_ppf "%a@ =@ %a" pp_vname lab pp_type_expr e))
        lab_exprs
;;
let pp_simple_type_def_body ppf =
  pp_ast pp_simple_type_def_body_desc ppf
;;



let pp_external_binding ppf eb =
  let (vname, external_expr) = eb.Parsetree.ast_desc in
  Format.fprintf ppf "%a =@ %a" pp_vname vname pp_external_expr external_expr
;;
let pp_external_bindings ppf ebs =
  List.iter
    (fun binding ->
      Format.fprintf ppf "@[<2>and %a@]@\n" pp_external_binding binding)
    ebs.Parsetree.ast_desc
;;



let pp_external_type_def_body_desc ppf ex_tydef_body =
  Format.fprintf ppf "internal";
  (match ex_tydef_body.Parsetree.etdb_internal with
   | None -> Format.fprintf ppf "@\n"
   | Some simple_tydef_body ->
       Format.fprintf ppf "@ %a@\n"
         pp_simple_type_def_body simple_tydef_body);
  Format.fprintf ppf "external@ %a@\n"
    pp_external_expr ex_tydef_body.Parsetree.etdb_external;
  Format.fprintf ppf "%a"
    pp_external_bindings ex_tydef_body.Parsetree.etdb_bindings
;;
let pp_external_type_def_body ppf =
  pp_ast pp_external_type_def_body_desc ppf
;;



let pp_type_def_body_desc ppf = function
  | Parsetree.TDB_simple simple_tydef_body ->
      Format.fprintf ppf "%a" pp_simple_type_def_body simple_tydef_body
  | Parsetree.TDB_external external_tydef_body ->
      Format.fprintf ppf "%a" pp_external_type_def_body external_tydef_body
;;
let pp_type_def_body ppf = pp_ast pp_type_def_body_desc ppf
;;



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



let pp_phrase_desc ppf = function
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
  | Parsetree.Ph_type type_def ->
      Format.fprintf ppf "%a;;" pp_type_def type_def
  | Parsetree.Ph_let let_def -> Format.fprintf ppf "%a;;" pp_let_def let_def
  | Parsetree.Ph_theorem t_def ->
      Format.fprintf ppf "%a;;" pp_theorem_def t_def
  | Parsetree.Ph_expr expr -> Format.fprintf ppf "%a;;" pp_expr expr
;;
let pp_phrase ppf = pp_ast pp_phrase_desc ppf
;;
let pp_phrases ppf = Handy.pp_generic_newlined_list pp_phrase ppf
;;



(* *************************************************************** *)
(* pp_file_desc : Format.formatter -> Parsetree.file_desc -> unit  *)
(** {b Descr} : Pretty prints a [file_desc] value as FoCal source.

    {b Rem} : Not exported ouside this module.                     *)
(* *************************************************************** *)
let pp_file_desc ppf = function
  | Parsetree.File phrases -> Format.fprintf ppf "%a" pp_phrases phrases
;;
(* ********************************************************** *)
(* pp_file : Format.formatter -> Parsetree.file -> unit       *)
(** {b Descr} : Pretty prints a [file] value as FoCal source.

    {b Rem} : Exported ouside this module.                    *)
(* ********************************************************** *)
let pp_file ppf = pp_ast pp_file_desc ppf
;;



(** {Rem} : Not exported outside this module. *)
let pp_simple_species_expr_as_effective_parameter ppf = function
  | Parsetree_utils.SPE_Self -> Format.fprintf ppf "Self"
  | Parsetree_utils.SPE_Species (qvname, _) ->
      Format.fprintf ppf "%a" pp_qualified_vname qvname
  | Parsetree_utils.SPE_Expr_entity expr ->
      Format.fprintf ppf "%a" pp_expr expr
;;

(** Stuff for printing [Parsetree_utils.simple_species_expr] *)
let pp_simple_species_expr ppf sse =
  Format.fprintf ppf "@[<2>%a" pp_ident sse.Parsetree_utils.sse_name ;
  if sse.Parsetree_utils.sse_effective_args <> [] then
    (begin
    Format.fprintf ppf "@ (%a)"
      (Handy.pp_generic_separated_list
         "," pp_simple_species_expr_as_effective_parameter)
      sse.Parsetree_utils.sse_effective_args
    end) ;
  Format.fprintf ppf "@]"
;;

