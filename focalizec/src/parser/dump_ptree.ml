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

(* $Id: dump_ptree.ml,v 1.18 2007-10-09 08:38:15 pessaux Exp $ *)


(* *********************************************************************** *)
(* pp_position : Format.formatter -> Lexing.position -> unit               *)
(** {b Descr} : Pretty prints a [position] value as a Caml-like structure.

    {b Rem} : Not exported ouside this module.                             *)
(* *********************************************************************** *)
let pp_position ppf pos =
  Format.fprintf ppf "@[<2>{@ %s ;@ %d ;@ %d ;@ %d @]}"
    pos.Lexing.pos_fname pos.Lexing.pos_lnum
    pos.Lexing.pos_bol pos.Lexing.pos_cnum
;;



(* *********************************************************************** *)
(* pp_location : Format.formatter -> Parsetree.location -> unit            *)
(** {b Descr} : Pretty prints a [location] value as a Caml-like structure.

    {b Rem} : Not exported ouside this module.                             *)
(* *********************************************************************** *)
let pp_location ppf loc =
  Format.fprintf ppf "@[<2>{@ %a ;@ %a @]}"
    pp_position loc.Location.l_beg pp_position loc.Location.l_end
;;



(* ******************************************************************** *)
(* pp_vname : Format.formatter -> Parsetree.vname -> unit               *)
(** {b Descr} : Pretty prints a [vname] value as a Caml-like structure.

    {b Rem} : Not exported ouside this module.                          *)
(* ******************************************************************** *)
let pp_vname ppf = function
  | Parsetree.Vlident s -> Format.fprintf ppf "@[<2>Vlident@ (%s@])" s
  | Parsetree.Vuident s -> Format.fprintf ppf "@[<2>Vuident@ (%s@])" s
  | Parsetree.Vpident s -> Format.fprintf ppf "@[<2>Vpident@ (%s@])" s
  | Parsetree.Viident s -> Format.fprintf ppf "@[<2>Viident@ (%s@])" s
  | Parsetree.Vqident s -> Format.fprintf ppf "@[<2>Vqident@ (%s@])" s
;;
(* ******************************************************************* *)
(* pp_vnames : Format.formatter -> Parsetree.vname list -> unit *)
(** {b Descr} : Pretty prints a [list] of [vname] value as a Caml-like
              structure.

    {b Rem} : Not exported ouside this module.                         *)
(* ******************************************************************* *)
let pp_vnames ppf = Handy.pp_generic_separated_list "," pp_vname ppf ;;



(* ************************************************************************* *)
(* pp_node_label : Format.formatter -> int * string -> unit                  *)
(** {b Descr} : Pretty prints a [node_label] value as a Caml-like structure.

    {b Rem} : Not exported ouside this module.                               *)
(* ************************************************************************* *)
let pp_node_label ppf (i, s) = Format.fprintf ppf "(%d,@ %s)" i s ;;
(* ************************************************************************ *)
(* pp_node_labels : Format.formatter -> (int * string) list -> unit         *)
(** {b Descr} : Pretty prints a [list] of [node_label] value as a Caml-like
              structure.

    {b Rem} : Not exported ouside this module.                              *)
(* ************************************************************************ *)
let pp_node_labels ppf =
  Handy.pp_generic_separated_list "," pp_node_label ppf
;;



(* ********************************************************************** *)
(* pp_generic_ast :                                                       *)
(*          (Format.formatter -> 'a -> unit) ->                           *)
(*            Format.formatter -> ('a, 'b) Parsetree.generic_ast -> unit  *)
(** {b Descr} : Wrapper to apply pretty-printing only on the 'desc' field
              of a generic_ast. Ignores all other fields.
    {b Rem} : Not exported ouside this module.                            *)
(* ********************************************************************** *)
let pp_generic_ast desc_printer_fct ppf g_ast =
  Format.fprintf ppf "%a@\n%a"
    pp_location g_ast.Parsetree.ast_loc
    desc_printer_fct g_ast.Parsetree.ast_desc
;;



(* ************************************************************************* *)
(* pp_ident_desc : Format.formatter -> Parsetree.ident_desc -> unit          *)
(** {b Descr} : Pretty prints a [ident_desc] value as a Caml-like structure.

    {b Rem} : Not exported ouside this module.                               *)
(* ************************************************************************* *)
let pp_ident_desc ppf = function
  | Parsetree.I_local vname ->
      Format.fprintf ppf "@[<2>I_local@ (%a@])" pp_vname vname
  | Parsetree.I_global (fname_opt, vname) ->
      let fname =
	(match fname_opt with None -> "None" | Some n -> "Some (" ^ n ^ ")") in
      Format.fprintf ppf "@[<2>I_global@ (%s,@ %a@])" fname pp_vname vname
;;
(* ******************************************************************** *)
(* pp_ident : Format.formatter -> Parsetree.ident- > unit               *)
(** {b Descr} : Pretty prints a [ident] value as a Caml-like structure.

    {b Rem} : Not exported ouside this module.                          *)
(* ******************************************************************** *)
let pp_ident ppf = pp_generic_ast pp_ident_desc ppf ;;
(* ******************************************************************* *)
(* pp_idents : Format.formatter -> Parsetree.ident list -> unit        *)
(** {b Descr} : Pretty prints a [list] of [ident] value as a Caml-like
              structure.

    {b Rem} : Not exported ouside this module.                         *)
(* ******************************************************************* *)
let pp_idents ppf = Handy.pp_generic_separated_list "," pp_ident ppf ;;



(* ******************************************************************* *)
(* Format.formatter -> Parsetree.expr_ident_desc -> unit               *)
(** {b Descr} : Pretty prints a [expr_ident_desc] value as a Caml-like
              structure.

    {b Rem} : Not exported ouside this module.                         *)
(* ******************************************************************* *)
let pp_expr_ident_desc ppf = function
  | Parsetree.EI_local vname ->
      Format.fprintf ppf "@[<2>I_local@ (%a@])" pp_vname vname
  | Parsetree.EI_global (fname_opt, vname) ->
      let fname =
	(match fname_opt with None -> "None" | Some n -> "Some (" ^ n ^ ")") in
      Format.fprintf ppf "@[<2>I_global@ (%s,@ %a@])" fname pp_vname vname
  | Parsetree.EI_method (species_opt, meth_vname) ->
      (begin
      match species_opt with
       | None ->
	   Format.fprintf ppf "@[<2>I_method@ (None,@ %a@])" pp_vname meth_vname
       | Some species_qualifier ->
	   (begin
	   match species_qualifier with
	    | (None, species_vname) ->
		Format.fprintf ppf
		  "@[<2>I_method@ (Some (None,@ %a),@ %a@])"
		  pp_vname species_vname pp_vname meth_vname
	    | ((Some module_name), species_vname) ->
		Format.fprintf ppf
		  "@[<2>I_method@ (Some ((Some %s),@ %a),@ %a@])"
		  module_name pp_vname species_vname pp_vname meth_vname
	   end)
      end)
;;
(* ************************************************************************ *)
(* Format.formatter -> Parsetree.expr_ident -> unit                         *)
(** {b Descr} : Pretty prints a [expr_ident] value as a Caml-like structure.

    {b Rem} : Not exported ouside this module.                              *)
(* ******
****************************************************************** *)
let pp_expr_ident ppf = pp_generic_ast pp_expr_ident_desc ppf ;;
(* ************************************************************** *)
(* Format.formatter -> Parsetree.expr_ident list -> unit          *)
(** {b Descr} : Pretty prints a [list] of [expr_ident] value as a
              Caml-like structure.

    {b Rem} : Not exported ouside this module.                    *)
(* ************************************************************** *)
let pp_expr_idents ppf =
  Handy.pp_generic_separated_list "," pp_expr_ident ppf
;;



let pp_constructor_ident_desc ppf (Parsetree.CI (fname_opt, vname)) =
  let fname =
    (match fname_opt with None -> "None" | Some n -> "Some (" ^ n ^ ")") in
  Format.fprintf ppf "@[<2>CE@ (%s,@ %a@])" fname pp_vname vname
;;
let pp_constructor_ident ppf = pp_generic_ast pp_constructor_ident_desc ppf ;;



(* ********************************************************************* *)
(* pp_rep_type_def_desc :                                                *)
(*          Format.formatter -> Parsetree.rep_type_def_desc -> unit      *)
(** {b Descr} : Pretty prints a [rep_type_def_desc] value as a Caml-like
              structure.

    {b Rem} : Not exported ouside this module.                           *)
(* ********************************************************************* *)
let rec pp_rep_type_def_desc ppf = function
  | Parsetree.RTE_ident ident ->
      Format.fprintf ppf "@[<2>RTE_ident@ (%a@])" pp_ident ident
  | Parsetree.RTE_fun (rtd1, rtd2) ->
      Format.fprintf ppf "@[<2>RTE_fun@ (%a,@ %a@])"
	pp_rep_type_def rtd1 pp_rep_type_def rtd2
  | Parsetree.RTE_app (ident, rtds) ->
      Format.fprintf ppf "@[<2>RTE_app@ (%a,@ [@ %a@ ]@])"
	pp_ident ident pp_rep_type_defs rtds
  | Parsetree.RTE_prod rtds ->
      Format.fprintf ppf "@[<2>RTE_prod@ (%a@])" pp_rep_type_defs rtds
  | Parsetree.RTE_paren rtd ->
      Format.fprintf ppf "@[<2>RTE_paren@ (%a@])" pp_rep_type_def rtd
(* ************************************************************************* *)
(* pp_rep_type_defs :                                                        *)
(*          Format.formatter -> Parsetree.rep_type_def list -> unit          *)
(** {b Descr} : Pretty prints a [list] of [rep_type_def] value as a Caml-like
              structure.

    {b Rem} : Not exported ouside this module.                               *)
(* ************************************************************************* *)
and pp_rep_type_defs ppf =
  Handy.pp_generic_separated_list "," pp_rep_type_def ppf
(* ************************************************************************** *)
(* pp_rep_type_def :                                                          *)
(*          Format.formatter -> Parsetree.rep_type_def -> unit                *)
(** {b Descr} : Pretty prints a [rep_type_def] value as a Caml-like structure.

    {b Rem} : Not exported ouside this module.                                *)
(* ************************************************************************** *)
and pp_rep_type_def ppf = pp_generic_ast pp_rep_type_def_desc ppf ;;



(* ****************************************************************** *)
(* pp_type_expr_desc :                                                *)
(*           Format.formatter -> Parsetree.type_expr_desc -> unit     *)
(** {b Descr} : Pretty prints a [type_expr_desc] value as a Caml-like
              structure.

    {b Rem} : Not exported ouside this module.                        *)
(* ****************************************************************** *)
let rec pp_type_expr_desc ppf = function
  | Parsetree.TE_ident ident ->
      Format.fprintf ppf "@[<2>TE_ident@ (%a@])" pp_ident ident
  | Parsetree.TE_fun (te1, te2) ->
      Format.fprintf ppf "@[<2>TE_fun@ (%a,@ %a@])"
	pp_type_expr te1 pp_type_expr te2
  | Parsetree.TE_app (ident, tes) ->
      Format.fprintf ppf "@[<2>TE_app@ (%a,@ %a@])"
	pp_ident ident pp_type_exprs tes
  | Parsetree.TE_prod (tes) ->
      Format.fprintf ppf "@[<2>TE_prod@ (%a])" pp_type_exprs tes
  | Parsetree.TE_self -> Format.fprintf ppf "TE_self"
  | Parsetree.TE_prop -> Format.fprintf ppf "TE_prop"
  | Parsetree.TE_paren te ->
      Format.fprintf ppf "@[<2>TE_paren@ (%a@])" pp_type_expr te
(* ************************************************************* *)
(* pp_type_exprs :                                               *)
(*          Format.formatter -> Parsetree.type_expr list -> unit *)
(** {b Descr} : Pretty prints a [list] of [type_expr] value as a
              Caml-like structure.

    {b Rem} : Not exported ouside this module.                   *)
(* ************************************************************* *)
and pp_type_exprs ppf = Handy.pp_generic_separated_list "," pp_type_expr ppf
(* ************************************************************************ *)
(* pp_type_expr : Format.formatter -> Parsetree.type_expr -> unit           *)
(** {b Descr} : Pretty prints a [type_expr] value as a Caml-like structure.

    {b Rem} : Not exported ouside this module.                              *)
(* ************************************************************************ *)
and pp_type_expr ppf = pp_generic_ast pp_type_expr_desc ppf ;;



(* ***************************************************************** *)
(* pp_constant_desc :                                                *)
(*          Format.formatter -> Parsetree.constant_desc -> unit      *)
(** {b Descr} : Pretty prints a [constant_desc] value as a Caml-like
              structure.

    {b Rem} : Not exported ouside this module.                       *)
(* ***************************************************************** *)
let pp_constant_desc ppf = function
  | Parsetree.C_int s -> Format.fprintf ppf "@[<2>C_int@ (%s@])" s
  | Parsetree.C_float s -> Format.fprintf ppf "@[<2>C_float@ (%s@])" s
  | Parsetree.C_bool s -> Format.fprintf ppf "@[<2>C_bool@ (%s@])" s
  | Parsetree.C_string s -> Format.fprintf ppf "@[<2>C_string@ (%s@])" s
  | Parsetree.C_char c ->
      let tmp_s = " " in
      tmp_s.[0] <- c ;
      Format.fprintf ppf "@[<2>C_char@ (%s@])" tmp_s
;;
(* *********************************************************************** *)
(* pp_constant : Format.formatter -> Parsetree.constant -> unit            *)
(** {b Descr} : Pretty prints a [constant] value as a Caml-like structure.

    {b Rem} : Not exported ouside this module.                             *)
(* *********************************************************************** *)
let pp_constant = pp_generic_ast pp_constant_desc
;;



(* *********************************************************************** *)
(* Format.formatter -> Parsetree.rec_flag -> unit                          *)
(** {b Descr} : Pretty prints a [rec_flag] value as a Caml-like structure.

    {b Rem} : Not exported ouside this module.                             *)
(* *********************************************************************** *)
let pp_rec_flag ppf = function
  | Parsetree.RF_no_rec -> Format.fprintf ppf "RF_no_rec"
  | Parsetree.RF_rec -> Format.fprintf ppf "RF_rec"
;;



(* *********************************************************************** *)
(* Format.formatter -> Parsetree.log_flag -> unit                          *)
(** {b Descr} : Pretty prints a [log_flag] value as a Caml-like structure.

    {b Rem} : Not exported ouside this module.                             *)
(* *********************************************************************** *)
let pp_logical_flag ppf = function
  | Parsetree.LF_no_logical -> Format.fprintf ppf "LF_no_logical"
  | Parsetree.LF_logical -> Format.fprintf ppf "LF_logical"
;;



(* *********************************************************************** *)
(* Format.formatter -> Parsetree.loc_flag -> unit                          *)
(** {b Descr} : Pretty prints a [loc_flag] value as a Caml-like structure.

    {b Rem} : Not exported ouside this module.                             *)
(* *********************************************************************** *)
let pp_local_flag ppf = function
  | Parsetree.LF_no_local -> Format.fprintf ppf "LF_no_local"
  | Parsetree.LF_local -> Format.fprintf ppf "LF_local"
;;



(* *********************************************************************** *)
(* Format.formatter -> Parsetree.pat_desc -> unit                          *)
(** {b Descr} : Pretty prints a [pat_desc] value as a Caml-like structure.

    {b Rem} : Not exported ouside this module.                             *)
(* *********************************************************************** *)
let rec pp_pat_desc ppf = function
  | Parsetree.P_const cst ->
      Format.fprintf ppf "@[<2>P_const@ (%a@])" pp_constant cst
  | Parsetree.P_var vname ->
      Format.fprintf ppf "@[<2>P_var@ (%a@])" pp_vname vname
  | Parsetree.P_as (pat, vname) ->
      Format.fprintf ppf "@[<2>P_as@ (%a,@ %a@])" pp_pattern pat pp_vname vname
  | Parsetree.P_wild -> Format.fprintf ppf "P_wild"
  | Parsetree.P_constr (ident, pats) ->
      Format.fprintf ppf "@[<2>P_app@ (%a,@ %a@])"
	pp_constructor_ident ident pp_patterns pats
  | Parsetree.P_record lab_pat_lst ->
      Format.fprintf ppf "@[<2>P_record@ ([@ %a@ ]@])"
	(Handy.pp_generic_separated_list
	   ";"
	   (fun local_ppf (label, pat) ->
	     Format.fprintf local_ppf "(%s,@ %a)" label pp_pattern pat))
	lab_pat_lst
  | Parsetree.P_tuple pats ->
      Format.fprintf ppf "@[<2>P_tuple@ ([@ %a@ ]@])" pp_patterns pats
  | Parsetree.P_paren pat ->
      Format.fprintf ppf "@[<2>P_paren@ (%a@])" pp_pattern pat
(* ********************************************************************* *)
(* Format.formatter -> Parsetree.pattern list -> unit                    *)
(** {b Descr} : Pretty prints a [list] of [pattern] value as a Caml-like
              structure.

    {b Rem} : Not exported ouside this module.                           *)
(* ********************************************************************* *)
and pp_patterns ppf = Handy.pp_generic_separated_list "," pp_pattern ppf
(* ********************************************************************** *)
(* Format.formatter -> Parsetree.pattern -> unit                          *)
(** {b Descr} : Pretty prints a [pattern] value as a Caml-like structure.

    {b Rem} : Not exported ouside this module.                            *)
(* ********************************************************************** *)
and pp_pattern ppf = pp_generic_ast pp_pat_desc ppf ;;



(* ********************************************************************* *)
(* Format.formatter -> Parsetree.external_language -> unit               *)
(** {b Descr} : Pretty prints a [external_language] value as a Caml-like
              structure.

    {b Rem} : Not exported ouside this module.                           *)
(* ********************************************************************* *)
let pp_external_language ppf = function
  | Parsetree.EL_Caml -> Format.fprintf ppf "EL_Caml"
  | Parsetree.EL_Coq -> Format.fprintf ppf "EL_Coq"
  | Parsetree.EL_external s -> Format.fprintf ppf "EL_external@ (@[<2>%s@])" s
;;



(* ********************************************************************* *)
(* Format.formatter -> Parsetree.external_def_desc -> unit               *)
(** {b Descr} : Pretty prints a [external_def_desc] value as a Caml-like
              structure.

    {b Rem} : Not exported ouside this module.                           *)
(* ********************************************************************* *)
let rec pp_external_def_desc ppf = function
  | Parsetree.ED_type edb ->
      Format.fprintf ppf "ED_type@ (@[<2>%a@])" pp_external_type_def_body edb
  | Parsetree.ED_value edb ->
      Format.fprintf ppf "ED_value@ (@[<2>%a@])" pp_external_value_def_body edb
and pp_external_def ppf = pp_generic_ast pp_external_def_desc ppf



(* ********************************************************************* *)
(* Format.formatter -> Parsetree.external_type_def_body_desc -> unit     *)
(** {b Descr} : Pretty prints a [external_type_def_body_desc] value as a
              Caml-like structure.

    {b Rem} : Not exported ouside this module.                           *)
(* ********************************************************************* *)
and pp_external_type_def_body_desc ppf body =
  Format.fprintf ppf "@[<2>{@ %a ;@ %a @]}"
    pp_vname body.Parsetree.etd_name pp_external_expr body.Parsetree.etd_body
(* **************************************************************** *)
(* Format.formatter -> Parsetree.external_type_def_body -> unit     *)
(** {b Descr} : Pretty prints a [external_type_def_body] value as a
              Caml-like structure.

    {b Rem} : Not exported ouside this module.                      *)
(* **************************************************************** *)
and pp_external_type_def_body ppf =
  pp_generic_ast pp_external_type_def_body_desc ppf



(* ********************************************************************** *)
(* Format.formatter -> Parsetree.external_value_def_body_desc -> unit     *)
(** {b Descr} : Pretty prints a [external_value_def_body_desc] value as a
              Caml-like structure.

    {b Rem} : Not exported ouside this module.                            *)
(* ********************************************************************** *)
and pp_external_value_def_body_desc ppf body =
  Format.fprintf ppf "@[<2>{@ %a ;@ %a ;@ %a @]}"
    pp_vname body.Parsetree.evd_name
    pp_type_expr body.Parsetree.evd_type
    pp_external_expr body.Parsetree.evd_body
(* **************************************************************** *)
(* Format.formatter -> Parsetree.external_value_def_body -> unit    *)
(** {b Descr} : Pretty prints a [external_value_def_body] value as a
              Caml-like structure.

    {b Rem} : Not exported ouside this module.                      *)
(* **************************************************************** *)
and pp_external_value_def_body ppf =
  pp_generic_ast pp_external_value_def_body_desc ppf



(* ************************************************************ *)
(* Format.formatter -> Parsetree.external_expr_desc -> unit     *)
(** {b Descr} : Pretty prints a [external_expr_desc] value as a
              Caml-like structure.

    {b Rem} : Not exported ouside this module.                  *)
(* ************************************************************ *)
and external_expr_desc ppf lst =
  Format.fprintf ppf "[@ %a@ ]"
    (Handy.pp_generic_separated_list
       ","
       (fun local_ppf (ext_lang, ext_expr) ->
	 Format.fprintf local_ppf "(%a,@ %a)"
	   pp_external_language ext_lang pp_external_expression ext_expr))
    lst
(* ***************************************************************** *)
(* Format.formatter -> Parsetree.external_expr -> unit               *)
(** {b Descr} : Pretty prints a [external_expr] value as a Caml-like
              structure.

    {b Rem} : Not exported ouside this module.                       *)
(* ***************************************************************** *)
and pp_external_expr ppf = pp_generic_ast external_expr_desc ppf



(* ****************************************************************** *)
(* pp_external_expression :                                           *)
(*          Format.formatter -> Parsetree.external_expression -> unit *)
(** {b Descr} : Pretty prints a [external_expression] value as a
               Caml-like structure.

    {b Rem} : Not exported ouside this module.                        *)
(* ****************************************************************** *)
and pp_external_expression ppf eexpr = Format.fprintf ppf "%s" eexpr ;;



(* ********************************************************** *)
(* Format.formatter -> Parsetree.species_def_desc -> unit     *)
(** {b Descr} : Pretty prints a [species_def_desc] value as a
               Caml-like structure.

    {b Rem} : Not exported ouside this module.                *)
(* ********************************************************** *)
let rec pp_species_def_desc ppf def =
  Format.fprintf ppf
    "@[<2>{@ %a ;@ [@ %a@ ] ;@ [@ %a@ ] ; [@ %a@ ] @]}"
    pp_vname def.Parsetree.sd_name
    (Handy.pp_generic_separated_list
       ","
       (fun local_ppf (vname, species_param_type) ->
	 Format.fprintf local_ppf "(%a,@ %a)"
	   pp_vname vname pp_species_param_type species_param_type))
    def.Parsetree.sd_params
    pp_species_exprs def.Parsetree.sd_inherits.Parsetree.ast_desc
    pp_species_fields def.Parsetree.sd_fields
(* ************************************************************************* *)
(* Format.formatter -> Parsetree.species_def -> unit                         *)
(** {b Descr} : Pretty prints a [species_def] value as a Caml-like structure.

    {b Rem} : Not exported ouside this module.                               *)
(* ************************************************************************* *)
and pp_species_def ppf = pp_generic_ast pp_species_def_desc ppf



(* ***************************************************************** *)
(* Format.formatter -> Parsetree.species_param_type_desc -> unit     *)
(** {b Descr} : Pretty prints a [species_param_type_desc] value as a
              Caml-like structure.

    {b Rem} : Not exported ouside this module.                        *)
(* ****************************************************************** *)
and pp_species_param_type_desc ppf = function
  | Parsetree.SPT_in ident ->
      Format.fprintf ppf "@[<2>SPT_in@ (%a@])" pp_ident ident
  | Parsetree.SPT_is species_expr ->
      Format.fprintf ppf "@[<2>SPT_is@ (%a@])" pp_species_expr species_expr
(* ************************************************************ *)
(* Format.formatter -> Parsetree.species_param_type -> unit     *)
(** {b Descr} : Pretty prints a [species_param_type] value as a
              Caml-like structure.

    {b Rem} : Not exported ouside this module.                  *)
(* ************************************************************ *)
and pp_species_param_type ppf = pp_generic_ast pp_species_param_type_desc ppf



(* **************************************************************** *)
(* pp_species_expr_desc :                                           *)
(*          Format.formatter -> Parsetree.species_expr_desc -> unit *)
(** {b Descr} : Pretty prints a [species_expr_desc] value as a
              Caml-like structure.

    {b Rem} : Not exported ouside this module.                      *)
(* **************************************************************** *)
and pp_species_expr_desc ppf sed =
  Format.fprintf ppf "@[<2>{@ %a ;@ %a @]}"
    pp_ident sed.Parsetree.se_name pp_species_params sed.Parsetree.se_params
(* **************************************************************** *)
(* pp_species_exprs :                                               *)
(*         Format.formatter -> Parsetree.species_expr list -> unit  *)
(** {b Descr} : Pretty prints a [list] of [species_expr] value as a
              Caml-like structure.

    {b Rem} : Not exported ouside this module.                      *)
(* **************************************************************** *)
and pp_species_exprs ppf =
  Handy.pp_generic_separated_list "," pp_species_expr ppf
(* *********************************************************** *)
(* pp_species_expr :                                           *)
(*          Format.formatter -> Parsetree.species_expr -> unit *)
(** {b Descr} : Pretty prints a [species_expr] value as a
              Caml-like structure.

    {b Rem} : Not exported ouside this module.                 *)
(* *********************************************************** *)
and pp_species_expr ppf = pp_generic_ast pp_species_expr_desc ppf



(* ***************************************************************** *)
(* pp_species_param_desc :                                           *)
(*          Format.formatter -> Parsetree.species_param_desc -> unit *)
(** {b Descr} : Pretty prints a [species_param_desc] value as a
              Caml-like structure.

    {b Rem} : Not exported ouside this module.                       *)
(* ***************************************************************** *)
and pp_species_param_desc ppf = function
  | Parsetree.SP expr -> Format.fprintf ppf "SP@ (@[<2>%a@])" pp_expr expr
(* ***************************************************************** *)
(* pp_species_params :                                               *)
(*          Format.formatter -> Parsetree.species_param list -> unit *)
(** {b Descr} : Pretty prints a [list] of [species_param] value as a
              Caml-like structure.

    {b Rem} : Not exported ouside this module.                       *)
(* ***************************************************************** *)
and pp_species_params ppf =
  Handy.pp_generic_separated_list "," pp_species_param ppf
(* ************************************************************ *)
(* pp_species_param :                                           *)
(*          Format.formatter -> Parsetree.species_param -> unit *)
(** {b Descr} : Pretty prints a [species_param] value as a
              Caml-like structure.

    {b Rem} : Not exported ouside this module.                  *)
(* ************************************************************ *)
and pp_species_param ppf = pp_generic_ast pp_species_param_desc ppf



and pp_sig_def_desc ppf sdd =
  Format.fprintf ppf "@[<2>{@ %a ;@ %a @]}"
    pp_vname sdd.Parsetree.sig_name pp_type_expr sdd.Parsetree.sig_type
and pp_sig_def ppf = pp_generic_ast pp_sig_def_desc ppf



and pp_proof_def_desc ppf pdd =
  Format.fprintf ppf "@[<2>{@ %a ;@ %a @]}"
    pp_vname pdd.Parsetree.pd_name pp_proof pdd.Parsetree.pd_proof
and pp_proof_def ppf = pp_generic_ast pp_proof_def_desc ppf



and pp_property_def_desc ppf pdd =
  Format.fprintf ppf "@[<2>{@ %a ;@ %a @]}"
    pp_vname pdd.Parsetree.prd_name pp_prop pdd.Parsetree.prd_prop
and pp_property_def ppf = pp_generic_ast pp_property_def_desc ppf



and pp_species_field_desc ppf = function
  | Parsetree.SF_rep rep_type_def ->
      Format.fprintf ppf "@[<2>SF_rep@ (%a@])" pp_rep_type_def rep_type_def
  | Parsetree.SF_sig sig_def ->
      Format.fprintf ppf "@[<2>SF_sig@ (%a@])" pp_sig_def sig_def
  | Parsetree.SF_let let_def ->
      Format.fprintf ppf "@[<2>SF_let@ (%a@])" pp_let_def let_def
  | Parsetree.SF_property property_def ->
      Format.fprintf ppf "@[<2>SF_property@ (%a@])" pp_property_def property_def
  | Parsetree.SF_theorem theorem_def ->
      Format.fprintf ppf "@[<2>SF_theorem@ (%a@])" pp_theorem_def theorem_def
  | Parsetree.SF_proof proof_def ->
      Format.fprintf ppf "@[<2>SF_proof@ (%a@])" pp_proof_def proof_def
and pp_species_fields ppf =
  Handy.pp_generic_separated_list "," pp_species_field ppf
and pp_species_field ppf = pp_generic_ast pp_species_field_desc ppf



and pp_let_def_desc ppf ldd =
  Format.fprintf ppf "@[<2>{@ %a ;@ %a ;@ %a ;@ [@ %a@ ] @]}"
    pp_rec_flag ldd.Parsetree.ld_rec
    pp_logical_flag ldd.Parsetree.ld_logical
    pp_local_flag ldd.Parsetree.ld_local
    pp_bindings ldd.Parsetree.ld_bindings
and pp_let_def ppf = pp_generic_ast pp_let_def_desc ppf



and pp_binding_desc ppf bd =
  Format.fprintf ppf "@[<2>{@ %a ;@ [@ %a@ ] @ %a @]}"
    pp_vname bd.Parsetree.b_name
    (Handy.pp_generic_separated_list
       ","
       (fun local_ppf (vname, ty_expr_opt) ->
	 Format.fprintf local_ppf "(%a,@ %a)"
	   pp_vname vname
	   (Handy.pp_generic_explicit_option pp_type_expr) ty_expr_opt))
    bd.Parsetree.b_params
    pp_expr bd.Parsetree.b_body
and pp_bindings ppf = Handy.pp_generic_separated_list "," pp_binding ppf
and pp_binding ppf = pp_generic_ast pp_binding_desc ppf



and pp_theorem_def_desc ppf tdd =
  Format.fprintf ppf "@[<2>{@ %a ;@ %a ;@ %a ;@ %a @]}"
    pp_vname tdd.Parsetree.th_name
    pp_local_flag tdd.Parsetree.th_local
    pp_prop tdd.Parsetree.th_stmt
    pp_proof tdd.Parsetree.th_proof
and pp_theorem_def ppf = pp_generic_ast pp_theorem_def_desc ppf



and pp_fact_desc ppf = function
  | Parsetree.F_def idents ->
      Format.fprintf ppf "F_def@ (@[<2>[@ %a@ ]@])" pp_expr_idents idents
  | Parsetree.F_property idents ->
      Format.fprintf ppf "F_property@ (@[<2>[@ %a@ ]@])" pp_expr_idents idents
  | Parsetree.F_hypothesis vnames ->
      Format.fprintf ppf "F_hypothesis@ (@[<2>[@ %a@ ]@])" pp_vnames vnames
  | Parsetree.F_node node_labels ->
      Format.fprintf ppf "F_node@ (@[<2>[@ %a@ ]@])" pp_node_labels node_labels
and pp_facts ppf = Handy.pp_generic_separated_list "," pp_fact ppf
and pp_fact ppf = pp_generic_ast pp_fact_desc ppf



and pp_proof_desc ppf = function
  | Parsetree.Pf_assumed -> Format.fprintf ppf "Pf_assumed"
  | Parsetree.Pf_auto facts ->
      Format.fprintf ppf "@[<2>Pf_auto@ ([@ %a@ ]@])" pp_facts facts
  | Parsetree.Pf_coq s -> Format.fprintf ppf "Pf_coq@ (%s)" s
  | Parsetree.Pf_node proof_nodes ->
      Format.fprintf ppf "@[<2>Pf_node@ ([@ %a@ ]@])" pp_proof_nodes proof_nodes
and pp_proof ppf = pp_generic_ast pp_proof_desc ppf



and pp_proof_node_desc ppf = function
  | Parsetree.PN_sub (node_label, stmt, proof) ->
      Format.fprintf ppf "@[<2>PN_sub@ (%a,@ %a,@ %a@])"
	pp_node_label node_label pp_statement stmt pp_proof proof
  | Parsetree.PN_qed (node_label, proof) ->
      Format.fprintf ppf "@[<2>PN_qed@ (%a,@ %a@])"
	pp_node_label node_label pp_proof proof
and pp_proof_nodes ppf = Handy.pp_generic_separated_list "," proof_node ppf
and proof_node ppf = pp_generic_ast pp_proof_node_desc ppf



and pp_statement_desc ppf stmt =
  Format.fprintf ppf "{@[<2>[@ %a@ ] ;@ %a @]}"
    pp_hyps stmt.Parsetree.s_hyps
    (Handy.pp_generic_explicit_option pp_prop) stmt.Parsetree.s_concl
and pp_statement ppf = pp_generic_ast pp_statement_desc ppf



and pp_hyp_desc ppf = function
  | Parsetree.H_var (vname, te) ->
      Format.fprintf ppf "@[<2>H_var@ (%a,@ %a@])"
	pp_vname vname pp_type_expr te
  | Parsetree.H_hyp (vname, prop) ->
      Format.fprintf ppf "@[<2>H_hyp@ (%a,@ %a@])"
	pp_vname vname pp_prop prop
  | Parsetree.H_not (vname, expr) ->
      Format.fprintf ppf "@[<2>H_not@ (%a,@ %a@])"
	pp_vname vname pp_expr expr
and pp_hyps ppf = Handy.pp_generic_separated_list "," pp_hyp ppf
and pp_hyp ppf = pp_generic_ast pp_hyp_desc ppf



and pp_prop_desc ppf = function
  | Parsetree.Pr_forall (vnames, type_expr_opt, prop) ->
      Format.fprintf ppf "@[<2>Pr_forall@ ([@ %a@ ],@ %a,@ %a@])"
	pp_vnames vnames pp_type_expr type_expr_opt pp_prop prop
  | Parsetree.Pr_exists (vnames, type_expr_opt, prop) ->
      Format.fprintf ppf "@[<2>Pr_exists@ ([@ %a@ ],@ %a,@ %a@])"
	pp_vnames vnames pp_type_expr type_expr_opt pp_prop prop
  | Parsetree.Pr_imply (p1, p2) ->
      Format.fprintf ppf "@[<2>Pr_imply@ (%a,@ %a@])" pp_prop p1 pp_prop p2
  | Parsetree.Pr_or (p1, p2) ->
      Format.fprintf ppf "@[<2>Pr_or@ (%a,@ %a@])" pp_prop p1 pp_prop p2
  | Parsetree.Pr_and (p1, p2) ->
      Format.fprintf ppf "@[<2>Pr_and@ (%a,@ %a@])" pp_prop p1 pp_prop p2
  | Parsetree.Pr_equiv (p1, p2) ->
      Format.fprintf ppf "@[<2>Pr_equiv@ (%a,@ %a@])" pp_prop p1 pp_prop p2
  | Parsetree.Pr_not p -> Format.fprintf ppf "@[<2>Pr_not@ (%a@])" pp_prop p
  | Parsetree.Pr_expr e -> Format.fprintf ppf "@[<2>Pr_expr@ (%a@])" pp_expr e
  | Parsetree.Pr_paren p -> Format.fprintf ppf "@[<2>Pr_paren@ (%a@])" pp_prop p
and pp_prop ppf = pp_generic_ast pp_prop_desc ppf



(* ************************************************************************ *)
(* Format.formatter -> Parsetree.expr_desc -> unit                          *)
(** {b Descr} : Pretty prints a [expr_desc] value as a Caml-like structure.

    {b Rem} : Not exported ouside this module.                              *)
(* ************************************************************************ *)
and pp_expr_desc ppf = function
  | Parsetree.E_self -> Format.fprintf ppf "Self"
  | Parsetree.E_const cst ->
      Format.fprintf ppf "@[<2>E_const@ (%a@])" pp_constant cst
  | Parsetree.E_fun (vnames, expr) ->
      Format.fprintf ppf "@[<2>E_fun@ ([@ %a@ ],@ %a@])"
	pp_vnames vnames pp_expr expr
  | Parsetree.E_var id ->
      Format.fprintf ppf "@[<2>E_var@ (%a@])" pp_expr_ident id
  | Parsetree.E_app (expr, exprs) ->
      Format.fprintf ppf "@[<2>E_app@ (%a,@ [@ %a@ ]@])"
	pp_expr expr pp_exprs exprs
  | Parsetree.E_constr (cstr_expr, exprs) ->
      Format.fprintf ppf "@[<2>E_constr@ (%a,@ [@ %a@ ]@])"
	pp_constructor_ident cstr_expr pp_exprs exprs
  | Parsetree.E_match (expr, pat_exprs) ->
      Format.fprintf ppf "@[<2>E_match@ (%a,@ [@ %a@ ]@])"
	pp_expr expr
	(Handy.pp_generic_separated_list
	   ","
	   (fun local_ppf (pat, e) ->
	     Format.fprintf local_ppf "(%a,@ %a)" pp_pattern pat pp_expr e))
	pat_exprs
  | Parsetree.E_if (expr1, expr2, expr3) ->
      Format.fprintf ppf "@[<2>E_if@ (%a,@ %a,@ %a@])"
	pp_expr expr1 pp_expr expr2 pp_expr expr3
  | Parsetree.E_let (let_def, expr) ->
      Format.fprintf ppf "@[<2>E_let@ (%a,@ %a@])"
	pp_let_def let_def pp_expr expr
  | Parsetree.E_record label_exprs ->
      Format.fprintf ppf "E_record@ ([@ %a@ ])"
	(Handy.pp_generic_separated_list
	   ","
	   (fun local_ppf (lab_name, e) ->
	     Format.fprintf local_ppf "(%s,@ %a)" lab_name pp_expr e))
	label_exprs
  | Parsetree.E_record_access (expr, label_name) ->
      Format.fprintf ppf "@[<2>E_record_access@ (%a,@ %s@])"
	pp_expr expr label_name
  | Parsetree.E_record_with (expr, label_exprs) ->
      Format.fprintf ppf "@[<2>E_record_with@ (%a,@ [@ %a@ ]@])"
	pp_expr expr
	(Handy.pp_generic_separated_list
	   ","
	   (fun local_ppf (lab_name, e) ->
	     Format.fprintf local_ppf "(%s,@ %a)" lab_name pp_expr e))
	label_exprs
  | Parsetree.E_tuple exprs ->
      Format.fprintf ppf "@[<2>E_tuple@ ([@ %a@ ]@])" pp_exprs exprs
  | Parsetree.E_external external_expr ->
      Format.fprintf ppf "@[<2>E_external@ (%a@])"
	pp_external_expr external_expr
  | Parsetree.E_paren expr ->
      Format.fprintf ppf "@[<2>E_paren@ (%a@])" pp_expr expr
(* ******************************************************************* *)
(* Format.formatter -> Parsetree.expr list -> unit                     *)
(** {b Descr} : Pretty prints a [list] of [expr] value as a Caml-like
              structure.

    {b Rem} : Not exported ouside this module.                         *)
(* ******************************************************************* *)
and pp_exprs ppf = Handy.pp_generic_separated_list "," pp_expr ppf
(* ******************************************************************* *)
(* Format.formatter -> Parsetree.expr -> unit                          *)
(** {b Descr} : Pretty prints a [expr] value as a Caml-like structure.

    {b Rem} : Not exported ouside this module.                         *)
(* ******************************************************************* *)
and pp_expr ppf = pp_generic_ast pp_expr_desc ppf
;;



(* ************************************************************************** *)
(* Format.formatter -> Parsetree.coll_def_desc -> unit                        *)
(** {b Descr} : Pretty prints a [coll_def_desc] value as a Caml-like structure.

    {b Rem} : Not exported ouside this module.                                *)
(* ************************************************************************** *)
let pp_coll_def_desc ppf cdd =
  Format.fprintf ppf "{@ %a ;@ %a }"
    pp_vname cdd.Parsetree.cd_name pp_species_expr cdd.Parsetree.cd_body
;;
(* *********************************************************************** *)
(* Format.formatter -> Parsetree.coll_def -> unit                          *)
(** {b Descr} : Pretty prints a [coll_def] value as a Caml-like structure.

    {b Rem} : Not exported ouside this module.                             *)
(* *********************************************************************** *)
let pp_coll_def ppf = pp_generic_ast pp_coll_def_desc ppf ;;



(* ******************************************************************** *)
(* Format.formatter ->                                                  *)
(*   (Parsetree.vname * Parsetree.type_expr list) list -> unit          *)
(** {b Descr} : Pretty prints the arguments of a [TD_union] constructor
              as a Caml-like structure. This function is mostly used to
              make the [pp_type_body_desc] function lighter.

    {b Rem} : Not exported ouside this module.                          *)
(* ******************************************************************** *)
let pp_tmp_TD_union ppf l =
  Format.fprintf ppf "@[<2>TD_union ([@ %a@ ]@])"
    (Handy.pp_generic_separated_list
       ","
       (fun local_ppf (constr_name, type_exprs) ->
	 Format.fprintf local_ppf "(%a,@ [@ %a@ ])"
	   pp_vname constr_name pp_type_exprs type_exprs))
    l
;;
	   


let rec pp_type_def_desc ppf td =
  Format.fprintf ppf "{@[<2>%a ;@ [@ %a@ ] ;@ %a @]}"
    pp_vname td.Parsetree.td_name
    (Handy.pp_generic_separated_list
       ","
       (fun local_ppf s -> Format.fprintf local_ppf "%a" pp_vname s))
    td.Parsetree.td_params
    pp_type_body td.Parsetree.td_body
and pp_type_def ppf = pp_generic_ast pp_type_def_desc ppf



and pp_type_body_desc ppf = function
  | Parsetree.TD_alias te ->
      Format.fprintf ppf "@[<2>TD_alias@ (%a@])" pp_type_expr te
  | Parsetree.TD_union l -> Format.fprintf ppf "%a" pp_tmp_TD_union l
  | Parsetree.TD_record lab_exprs ->
      Format.fprintf ppf "@[<2>TD_record@ ([@ %a@ ]@])"
	(Handy.pp_generic_separated_list
	   ","
	   (fun local_ppf (lab, e) ->
	     Format.fprintf local_ppf "(%s, %a)" lab pp_type_expr e))
	lab_exprs
and pp_type_body ppf = pp_generic_ast pp_type_body_desc ppf
;;



let pp_phrase_desc ppf = function
  | Parsetree.Ph_external external_def ->
      Format.fprintf ppf "@[<2>Ph_external@ (%a)@]@."
	pp_external_def external_def
  | Parsetree.Ph_use fname -> Format.fprintf ppf "@[<2>Ph_use@ (%s)@]@." fname
  | Parsetree.Ph_open fname -> Format.fprintf ppf "@[<2>Ph_open@ (%s)@]@." fname
  | Parsetree.Ph_species species_def ->
      Format.fprintf ppf "@[<2>Ph_species@ (%a)@]@." pp_species_def species_def
  | Parsetree.Ph_coll coll_def ->
      Format.fprintf ppf "@[<2>Ph_coll@ (%a)@]@." pp_coll_def coll_def
  | Parsetree.Ph_type type_def ->
      Format.fprintf ppf "@[<2>Ph_type@ (%a)@]@." pp_type_def type_def
  | Parsetree.Ph_let let_def ->
      Format.fprintf ppf "@[<2>Ph_let@ (%a)@]@." pp_let_def let_def
  | Parsetree.Ph_theorem theorem_def ->
      Format.fprintf ppf "@[<2>Ph_theorem@ (%a)@]@." pp_theorem_def theorem_def
  | Parsetree.Ph_expr expr ->
      Format.fprintf ppf "@[<2>Ph_expr@ (%a)@]@." pp_expr expr
;;
let pp_phrase ppf = pp_generic_ast pp_phrase_desc ppf ;;
let pp_phrases ppf = Handy.pp_generic_separated_list "," pp_phrase ppf ;;



(* ************************************************************************ *)
(* Format.formatter -> Parsetree.file_desc -> unit                          *)
(** {b Descr} : Pretty prints a [file_desc] value as a Caml-like structure.

    {b Rem} : Not exported ouside this module.                              *)
(* ************************************************************************ *)
let pp_file_desc ppf = function
  | Parsetree.File phrases ->
      Format.fprintf ppf "@[<2>File@ ([@ %a@ ]@[<2>)" pp_phrases phrases
;;
(* ******************************************************************* *)
(* Format.formatter -> Parsetree.file -> unit                          *)
(** {b Descr} : Pretty prints a [file] value as a Caml-like structure.

    {b Rem} : Exported ouside this module.                             *)
(* ******************************************************************* *)
let pp_file ppf = pp_generic_ast pp_file_desc ppf ;;
