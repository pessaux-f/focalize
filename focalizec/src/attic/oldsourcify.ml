(* $Id: oldsourcify.ml,v 1.1 2007-10-02 09:29:36 pessaux Exp $ *)

(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            Fran�ois Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)


module StringMod = struct type t = string let compare = compare end
;;
module StringSet = Set.Make(StringMod)
;;

let (cleanup_conflicting_idents_table, mk_regular_lowercase) =
  let non_renamed_idents = ref StringSet.empty in
  ((* ******************************************************************** *)
   (* cleanup_conflicting_idents_table : unit -> unit                      *)
   (** {b Descr} : Empties the set of non-modified identifier encountered.
                 This function is and must be called before parsing any
                 new file.

       {b Rem} : Not exported ouside this module.                          *)
   (* ******************************************************************** *)
   (fun () -> non_renamed_idents := StringSet.empty),
   (* *********************************************************************** *)
   (* mk_regular_lowercase : string -> string                                 *)
   (** {b Descr} : This function translates a new-syntax identifier string into
		 a legal old-syntaxe one. In effect, the old syntax requires
		 a string starting a lowercase alpha followed by only
		 alphanumerical characters (no "=", "<" of whatever operator
		 symbol or "(...)" prefix notation).
		 If the string starts by an uppercase character, then it is
		 transformed by lowerizing this character.
		 Otherwise, and for the remaining characters, we replace
		 them by a string sequence ("_...") if they are not pure
		 alphanumerical characters. As above, the use of a prefixing
		 "_" is intended to prevent conflicts due to this renaming
		 with alreading existing identifiers.
                 If the initial identifier was modified by the renaming
                 process, then we check whether if conflicts with a previous
                 non-modified identifier.

       {b Rem} : Not exported ouside this module.                             *)
   (* *********************************************************************** *)
   (fun name ->
     let name_len = String.length name in  (* Common expression sharing. *)
     if name_len <= 0 then ""
     else
       (begin
       let result_str = ref "" in
       let start_process_index =
	 (match name.[0] with
	  | 'A' .. 'Z' ->
	      (* If the first character is an uppercase *)
	      (*  alpha, then turn it to lowercase.     *)
	      result_str := " " ;
	      !result_str.[0] <- Char.lowercase name.[0] ;
	      1
	  | _ ->
	      (* Else, the first character was not an uppercase alpha,      *)
	      (* then we apply the regular transformations described below. *)
	      0) in
       (* Index where to stop characters processing. *)
       let stop_process_index = name_len - 1 in
       (* The regular transformation does not deal with uppercase because *)
       (* uppercase issue only applies to the first character and this    *)
       (* special case was handled above. Instead, we concentrate on      *)
       (* translating non-alphanumerical characters.                      *)
       let tmp_s = " " in    (* Just a 1 char buffer to build the string. *)
       for i = start_process_index to stop_process_index do
	 match name.[i] with
	  | '=' -> result_str := !result_str ^ "equal"
	  | '<' -> result_str := !result_str ^ "_lt"
	  | '>' -> result_str := !result_str ^ "_gt"
	  | '+' -> result_str := !result_str ^ "_plus"
	  | '-' -> result_str := !result_str ^ "_minus"
	  | '/' -> result_str := !result_str ^ "_slash"
	  | '*' -> result_str := !result_str ^ "_times"
	  | '&' -> result_str := !result_str ^ "_amp"
	  | '(' | ')' |'`' | '\'' ->
	      (* Discard the prefix, infix and the quote notations. *)
	      ()
          | ' ' ->
	      (* Translate spaces inside the name but delete those     *)
	      (* close to parens to discard the prefix/infix notation. *)
	      if (i > 0 && name.[i - 1] <> '(')
	         &&
		 (i < stop_process_index && name.[i + 1] <> ')') then
		result_str := !result_str ^ "_"
	  | whatever ->
	      tmp_s.[0] <- whatever ;
	      result_str := !result_str ^ tmp_s
       done ;
       (* If this ident was not modified, then it belongs *)
       (* to the set of legally defined identifiers.      *)
       if !result_str = name then
	 non_renamed_idents := StringSet.add name !non_renamed_idents
       else
	 (begin
	 (* If this ident  was modified, we check it against the set *)
	 (* non-modified idents to ensure they do not conflict.      *)
	 if StringSet.mem !result_str !non_renamed_idents then
	   Printf.eprintf
	     "Warning : renamed identifier \"%s\" \
              conflicting with existing identifier.\n" name ;
	   
	 end) ;
       !result_str
       end))
  )
;;


(* ***************************************************************** *)
(* int -> Parsetree.hyp list -> (Parsetree.vname * int) list         *)
(** {b Descr} : Records the definition of each hypothesis inside the
              proof tree by traversing a list of [hyp]s.

    {b Rem} : Not exported ouside this module.                       *)
(* ***************************************************************** *)
let rec record_hyp_level_in_hyps cur_level = function
  | [] -> []
  | h :: q ->
      let h_mapping =
	(begin
	match h.Parsetree.ast_desc with
	 | Parsetree.H_var (_, _) -> []
	 | Parsetree.H_hyp (vname, _) ->
	     (* Here an hypothesis is declared at the current level. *)
	     [(vname, cur_level)]
	 | Parsetree.H_not (_, _) -> []
	end) in
      h_mapping @ (record_hyp_level_in_hyps cur_level q)



(* ***************************************************************** *)
(* int -> Parsetree.statement -> (Parsetree.vname * int) list        *)
(** {b Descr} : Records the definition of each hypothesis inside the
              proof tree by traversing a [statement].

    {b Rem} : Not exported ouside this module.                       *)
(* ***************************************************************** *)
and record_hyp_level_in_statement cur_level stmt =
  record_hyp_level_in_hyps cur_level stmt.Parsetree.ast_desc.Parsetree.s_hyps



(* ***************************************************************** *)
(* int -> Parsetree.proof_node list -> (Parsetree.vname * int) list  *)
(** {b Descr} : Records the definition of each hypothesis inside the
              proof tree by traversing a list of [proof_node]s.

    {b Rem} : Not exported ouside this module.                       *)
(* ***************************************************************** *)
and record_hyp_level_in_proof_nodes cur_level = function
  | [] -> []
  | h :: q ->
      let h_mapping =
	(begin
	match h.Parsetree.ast_desc with
	 | Parsetree.PN_sub ((level, _), stmt, proof) ->
	     (* Can declare hypotheses via the statement. *)
	     (record_hyp_level_in_statement level stmt)
	     @ (record_hyp_level_in_proof cur_level proof)
	 | Parsetree.PN_qed ((level, _), proof) ->
	     record_hyp_level_in_proof level proof
	end) in
      h_mapping @ (record_hyp_level_in_proof_nodes cur_level q)



(* ***************************************************************** *)
(* int -> Parsetree.proof -> (Parsetree.vname * int) list            *)
(** {b Descr} : Records the definition of each hypothesis inside the
              proof tree by traversing a [proof].

    {b Rem} : Not exported ouside this module.                       *)
(* ***************************************************************** *)
and record_hyp_level_in_proof cur_level proof =
  match proof.Parsetree.ast_desc with
   | Parsetree.Pf_assumed -> []
   | Parsetree.Pf_auto _ ->
       (* Here the hyps are used. At print-time, that's here *)
       (* we will use the information currently harvested.   *)
       []
   | Parsetree.Pf_coq _ ->
       (* Verbatim stuff. Don't care and don't search anything inside ! *)
       []
   | Parsetree.Pf_node proof_nodes ->
       (* Go deeper to find hypotheses definitions... *)
       record_hyp_level_in_proof_nodes cur_level proof_nodes
;;



(* *************************************************************** *)
(* Format.formatter -> Parsetree.vname -> unit                     *)
(** {b Descr} : Pretty prints a [vname] value as old FoCal source.

    {b Rem} : Not exported ouside this module.                     *)
(* *************************************************************** *)
let pp_vname ppf = function
  | Parsetree.Vlident s | Parsetree.Vuident s | Parsetree.Vpident s
  | Parsetree.Viident s | Parsetree.Vqident s ->
      Format.fprintf ppf "%s" (mk_regular_lowercase s)
;;
(* ***************************************************************** *)
(* string -> Format.formatter -> Parsetree.vname list -> unit        *)
(** {b Descr} : Pretty prints a [list] of [vname] value as old FoCal
              source.

    {b Rem} : Not exported ouside this module.                       *)
(* ***************************************************************** *)
let pp_vnames sep ppf = Handy.pp_generic_separated_list sep pp_vname ppf
;;



let pp_vnames_n_level sep ppf =
  Handy.pp_generic_separated_list
    sep
    (fun local_ppf (vname, level) ->
      Format.fprintf local_ppf "<%d>:%a"  level pp_vname vname)
    ppf
;;



(* ******************************************************************** *)
(* Format.formatter -> int * string -> unit                             *)
(** {b Descr} : Pretty prints a [node_label] value as FoCal old source.

    {b Rem} : Not exported ouside this module.                          *)
(* ******************************************************************** *)
let pp_node_label ppf (i, s) = Format.fprintf ppf "<%d>%s" i s
;;
(* ********************************************************************** *)
(* string -> Format.formatter -> (int * string) list -> unit              *)
(** {b Descr} : Pretty prints a [list] of [node_label] value as old FoCal
              source.

    {b Rem} : Not exported ouside this module.                            *)
(* ********************************************************************** *)
let pp_node_labels sep ppf =
  Handy.pp_generic_separated_list sep pp_node_label ppf
;;



(* *********************************************************************** *)
(* (Format.formatter -> 'a -> unit) ->                                     *)
(*   Format.formatter -> ('a, string) Parsetree.generic_ast ->             *)
(*             unit                                                        *)
(** {b Descr} : Wrapper to apply pretty-printing only on the [ast_doc] and
              [ast_desc] fields of a [generic_ast]. Ignores all other
              fields.

    {b Rem} : Not exported ouside this module.                             *)
(* *********************************************************************** *)
let pp_generic_ast desc_printer_fct ppf g_ast =
  (* First, print the documentation if some exists. *)
  (match g_ast.Parsetree.ast_doc with
   | None -> ()
   | Some comment -> Format.fprintf ppf "(** %s*)@\n" comment) ;
  (* Then, print the code itself. *)
  Format.fprintf ppf "%a" desc_printer_fct g_ast.Parsetree.ast_desc
;;



(* ******************************************************************** *)
(* Format.formatter -> Parsetree.ident_desc -> unit                     *)
(** {b Descr} : Pretty prints a [ident_desc] value as old FoCal source.

    {b Rem} : Not exported ouside this module.                          *)
(* ******************************************************************** *)
let pp_ident_desc ppf = function
  | Parsetree.I_local vname -> Format.fprintf ppf "%a" pp_vname vname
  | Parsetree.I_global (fname_opt, vname) ->
      begin
      match fname_opt with
       | None -> Format.fprintf ppf "%a" pp_vname vname
       | Some fname -> Format.fprintf ppf "%s#%a" fname pp_vname vname
      end
;;
(* *************************************************************** *)
(* Format.formatter -> Parsetree.ident- > unit                     *)
(** {b Descr} : Pretty prints a [ident] value as old FoCal source.

    {b Rem} : Not exported ouside this module.                     *)
(* *************************************************************** *)
let pp_ident ppf = pp_generic_ast pp_ident_desc ppf
;;
(* ************************************************************************* *)
(* string -> Format.formatter -> Parsetree.ident list -> unit                *)
(** {b Descr} : Pretty prints a [list] of [ident] value as old FoCal source.

    {b Rem} : Not exported ouside this module.                               *)
(* ************************************************************************* *)
let pp_idents sep ppf = Handy.pp_generic_separated_list sep pp_ident ppf
;;



(* ************************************************************************ *)
(* Format.formatter -> Parsetree.expr_ident_desc -> unit                    *)
(** {b Descr} : Pretty prints a [expr_ident_desc] value as old FoCal source.

    {b Rem} : Not exported ouside this module.                              *)
(* ************************************************************************ *)
let pp_expr_ident_desc ppf = function
  | Parsetree.EI_local vname -> Format.fprintf ppf "%a" pp_vname vname
  | Parsetree.EI_global (fname_opt, meth_vname) ->
      begin
      match fname_opt with
       | None -> Format.fprintf ppf "%a" pp_vname meth_vname
       | Some fname -> Format.fprintf ppf "%s#%a" fname pp_vname meth_vname
      end
  | Parsetree.EI_method (coll_specifier_opt, meth_vname) ->
      (begin
      match coll_specifier_opt with
       | None -> Format.fprintf ppf "!%a" pp_vname meth_vname
       | Some coll_specifier ->
	   (begin
	   match coll_specifier with
	    | (None,  coll_vname) ->
		(* The species qualification doesn't show the hosting module. *)
		Format.fprintf ppf "%a!%a"
		  pp_vname coll_vname pp_vname meth_vname
	    | ((Some module_name), coll_vname) ->
		(* The species qualification shows the hosting module. *)
		Format.fprintf ppf "%s#%a!%a"
		  module_name pp_vname coll_vname pp_vname meth_vname
	   end)
      end)
;;
(* ******************************************************************** *)
(* Format.formatter -> Parsetree.expr_ident- > unit                     *)
(** {b Descr} : Pretty prints a [expr_ident] value as old FoCal source.

    {b Rem} : Not exported ouside this module.                          *)
(* ******************************************************************** *)
let pp_expr_ident ppf = pp_generic_ast pp_expr_ident_desc ppf
;;
(* ********************************************************************** *)
(* string -> Format.formatter -> Parsetree.expr_ident list -> unit        *)
(** {b Descr} : Pretty prints a [list] of [expr_ident] value as old FoCal
              source.

    {b Rem} : Not exported ouside this module.                            *)
(* ********************************************************************** *)
let pp_expr_idents sep ppf =
  Handy.pp_generic_separated_list sep pp_expr_ident ppf
;;



let pp_constructor_ident_desc ppf (Parsetree.CI (fname_opt, vname)) =
  begin
  match fname_opt with
   | None -> Format.fprintf ppf "%a" pp_vname vname
   | Some fname -> Format.fprintf ppf "%s#%a" fname pp_vname vname
  end
;;
let pp_constructor_ident ppf = pp_generic_ast pp_constructor_ident_desc ppf
;;


(* ******************************************************************* *)
(* Format.formatter -> Parsetree.rep_type_def_desc -> unit             *)
(** {b Descr} : Pretty prints a [rep_type_def_desc] value as old FoCal
              source.

    {b Rem} : Not exported ouside this module.                         *)
(* ******************************************************************* *)
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
(* ****************************************************************** *)
(* string -> Format.formatter -> Parsetree.rep_type_def list ->       *)
(*            unit                                                    *)
(** {b Descr} : Pretty prints a [list] of [rep_type_def] value as old
              FoCal source.

    {b Rem} : Not exported ouside this module.                        *)
(* ****************************************************************** *)
and pp_rep_type_defs sep ppf =
  Handy.pp_generic_separated_list sep pp_rep_type_def ppf
(* ********************************************************************** *)
(* Format.formatter -> Parsetree.rep_type_def -> unit                     *)
(** {b Descr} : Pretty prints a [rep_type_def] value as old FoCal source.

    {b Rem} : Not exported ouside this module.                            *)
(* ********************************************************************** *)
and pp_rep_type_def ppf = pp_generic_ast pp_rep_type_def_desc ppf
;;



(* **************************************************************** *)
(* Format.formatter -> Parsetree.type_expr_desc -> unit             *)
(** {b Descr} : Pretty prints a [type_expr_desc] value as old FoCal
              source.

    {b Rem} : Not exported ouside this module.                      *)
(* **************************************************************** *)
let rec pp_type_expr_desc ppf = function
  | Parsetree.TE_ident ident -> Format.fprintf ppf "%a" pp_ident ident
  | Parsetree.TE_fun (te1, te2) ->
      Format.fprintf ppf "@[<2>%a@ ->@ %a@]" pp_type_expr te1 pp_type_expr te2
  | Parsetree.TE_app (ident, tes) ->
      Format.fprintf ppf "%a@[<2>@ (%a)@]"
        pp_ident ident (pp_type_exprs ",") tes
  | Parsetree.TE_prod tes ->
      Format.fprintf ppf "@[<2>(%a)@]" (pp_type_exprs "*") tes
  | Parsetree.TE_self -> Format.fprintf ppf "self"
  | Parsetree.TE_prop -> Format.fprintf ppf "Prop"
  | Parsetree.TE_paren te -> Format.fprintf ppf "(%a)" pp_type_expr te
(* ********************************************************************* *)
(* string -> Format.formatter -> Parsetree.type_expr list -> unit        *)
(** {b Descr} : Pretty prints a [list] of [type_expr] value as old FoCal
              source.

    {b Rem} : Not exported ouside this module.                           *)
(* ********************************************************************* *)
and pp_type_exprs sep ppf = Handy.pp_generic_separated_list sep pp_type_expr ppf
(* ******************************************************************* *)
(* Format.formatter -> Parsetree.type_expr -> unit                     *)
(** {b Descr} : Pretty prints a [type_expr] value as old FoCal source.

    {b Rem} : Not exported ouside this module.                         *)
(* ******************************************************************* *)
and pp_type_expr ppf = pp_generic_ast pp_type_expr_desc ppf
;;



(* *********************************************************************** *)
(* Format.formatter -> Parsetree.constant_desc -> unit                     *)
(** {b Descr} : Pretty prints a [constant_desc] value as old FoCal source.

    {b Rem} : Not exported ouside this module.                             *)
(* *********************************************************************** *)
let pp_constant_desc ppf = function
  | Parsetree.C_int s | Parsetree.C_float s | Parsetree.C_bool s ->
      Format.fprintf ppf "%s" s
  | Parsetree.C_string s -> Format.fprintf ppf "\"%s\"" s
  | Parsetree.C_char c ->
      let tmp_s = " " in
      tmp_s.[0] <- c ;
      Format.fprintf ppf "%s" tmp_s
;;
(* ****************************************************************** *)
(* Format.formatter -> Parsetree.constant -> unit                     *)
(** {b Descr} : Pretty prints a [constant] value as old FoCal source.

    {b Rem} : Not exported ouside this module.                        *)
(* ****************************************************************** *)
let pp_constant = pp_generic_ast pp_constant_desc
;;



(* ****************************************************************** *)
(* Format.formatter -> Parsetree.loc_flag -> unit                     *)
(** {b Descr} : Pretty prints a [loc_flag] value as old FoCal source.

    {b Rem} : Not exported ouside this module.                        *)
(* ****************************************************************** *)
let pp_local_flag ppf = function
  | Parsetree.LF_no_local -> ()
  | Parsetree.LF_local -> Format.fprintf ppf "local@ "
;;



(* ********************************************************************* *)
(*  Format.formatter ->                                                  *)
(*    Parsetree.rec_flag * Parsetree.log_flag * Parsetree.loc_flag ->    *)
(*      unit                                                             *)
(** {b Descr} : Pretty prints a [let_def_desc] binding kind as old FoCal
              source. It mostly determines if the binding is a "let"
              or a "logical". Aside this, it add the possible "local"
              and "rec" flags is needed.

    {b Rem} : Not exported ouside this module.                           *)
(* ********************************************************************* *)
let pp_let_def_binding_flags ppf (ld_rec, ld_logical, ld_local) =
  Format.fprintf ppf "%a" pp_local_flag ld_local ;
  (match ld_logical with
   | Parsetree.LF_no_logical -> Format.fprintf ppf "let@ "
   | Parsetree.LF_logical -> Format.fprintf ppf "letprop@ ") ;
  (match ld_rec with
   | Parsetree.RF_no_rec -> ()
   | Parsetree.RF_rec -> Format.fprintf ppf "rec@ ")
;;


(* ****************************************************************** *)
(* Format.formatter -> Parsetree.pat_desc -> unit                     *)
(** {b Descr} : Pretty prints a [pat_desc] value as old FoCal source.

    {b Rem} : Not exported ouside this module.                        *)
(* ****************************************************************** *)
let rec pp_pat_desc ppf = function
  | Parsetree.P_const cst -> Format.fprintf ppf "%a" pp_constant cst
  | Parsetree.P_var vname -> Format.fprintf ppf "%a" pp_vname vname
  | Parsetree.P_as (pat, vname) ->
      Format.fprintf ppf "%a@ as@ %a" pp_pattern pat pp_vname vname
  | Parsetree.P_wild -> Format.fprintf ppf "_"
  | Parsetree.P_constr (ident, pats) ->
      Format.fprintf ppf "@[<2>%a@ (%a)@])"
	pp_constructor_ident ident (pp_patterns ",") pats
  | Parsetree.P_record lab_pat_lst ->
      Format.fprintf ppf "@[<2>{@ %a@ }@])"
	(Handy.pp_generic_separated_list
	   ";"
	   (fun local_ppf (label, pat) ->
	     Format.fprintf local_ppf "%s@ =@ %a" label pp_pattern pat))
	lab_pat_lst
  | Parsetree.P_tuple pats ->
      Format.fprintf ppf "@[<2>(%a)@]" (pp_patterns ",") pats
  | Parsetree.P_paren pat -> Format.fprintf ppf "@[<1>(%a)@]" pp_pattern pat
(* ********************************************************************* *)
(* pp_patterns :                                                  *)
(*          string -> Format.formatter -> Parsetree.pattern list -> unit *)
(** {b Descr} : Pretty prints a [list] of [pattern] value as old FoCal
              source.

    {b Rem} : Not exported ouside this module.                             *)
(* ********************************************************************* *)
and pp_patterns sep ppf = Handy.pp_generic_separated_list sep pp_pattern ppf
(* ***************************************************************** *)
(* pp_pattern : Format.formatter -> Parsetree.pattern -> unit *)
(** {b Descr} : Pretty prints a [pattern] value as FoCal old source.

    {b Rem} : Not exported ouside this module.                         *)
(* ***************************************************************** *)
and pp_pattern ppf = pp_generic_ast pp_pat_desc ppf
;;



(* ***************************************************************** *)
(* pp_external_language :                                     *)
(*          Format.formatter -> Parsetree.external_language -> unit  *)
(** {b Descr} : Pretty prints a [external_language] value as old FoCal
              source.

    {b Rem} : Not exported ouside this module.                         *)
(* ***************************************************************** *)
let pp_external_language ppf = function
  | Parsetree.EL_Caml -> Format.fprintf ppf "caml"
  | Parsetree.EL_Coq -> Format.fprintf ppf "coqdef"
  | Parsetree.EL_external s -> Format.fprintf ppf "%s" s
;;



(* ****************************************************************** *)
(* Format.formatter -> Parsetree.external_def_desc -> unit            *)
(** {b Descr} : Pretty prints a [external_def_desc] value as old FoCal
               source.

    {b Rem} : Not exported ouside this module.                        *)
(* ****************************************************************** *)
let rec pp_external_def_desc ppf = function
  | Parsetree.ED_type edb ->
      Format.fprintf ppf "@[<2>type@ %a@]" pp_external_type_def_body edb
  | Parsetree.ED_value edb ->
      Format.fprintf ppf "@[<2>value@ %a@]" pp_external_value_def_body edb
(* ************************************************************************ *)
(* pp_external_def :                                                 *)
(*          Format.formatter ->                                             *)
(*           (Parsetree.external_def_desc, string) Parsetree.generic_ast -> *)
(*             unit                                                         *)
(** {b Descr} : Pretty prints a [external_def] value as old FoCal source.

    {b Rem} : Not exported ouside this module.                                *)
(* ************************************************************************ *)
and pp_external_def ppf = pp_generic_ast pp_external_def_desc ppf



(* ******************************************************************* *)
(* Format.formatter -> Parsetree.external_type_def_body_desc -> unit   *)
(** {b Descr} : Pretty prints a [external_type_def_body_desc] value as
              old FoCal source.

    {b Rem} : Not exported ouside this module.                         *)
(* ******************************************************************* *)
and pp_external_type_def_body_desc ppf body =
  Format.fprintf ppf "%a@ =@ %a"
    pp_vname body.Parsetree.etd_name
    pp_external_expr body.Parsetree.etd_body
(* ****************************************************************** *)
(* Format.formatter -> Parsetree.external_type_def_body -> unit       *)
(** {b Descr} : Pretty prints a [external_type_def_body] value as old
              FoCal source.

    {b Rem} : Not exported ouside this module.                        *)
(* ****************************************************************** *)
and pp_external_type_def_body ppf =
  pp_generic_ast pp_external_type_def_body_desc ppf



(* ******************************************************************** *)
(* Format.formatter -> Parsetree.external_value_def_body_desc -> unit   *)
(** {b Descr} : Pretty prints a [external_value_def_body_desc] value as
              old FoCal source.

    {b Rem} : Not exported ouside this module.                          *)
(* ******************************************************************** *)
and pp_external_value_def_body_desc ppf body =
  Format.fprintf ppf "%a@ :@ %a@ =@ %a"
    pp_vname body.Parsetree.evd_name
    pp_type_expr body.Parsetree.evd_type
    pp_external_expr body.Parsetree.evd_body
(* *************************************************************** *)
(* Format.formatter -> Parsetree.external_value_def_body -> unit   *)
(** {b Descr} : Pretty prints a [external_value_def_body] value as
              old FoCal source.

    {b Rem} : Not exported ouside this module.                     *)
(* *************************************************************** *)
and pp_external_value_def_body ppf =
  pp_generic_ast pp_external_value_def_body_desc ppf



(* ******************************************************************** *)
(* Format.formatter -> Parsetree.external_expr_desc -> unit             *)
(** {b Descr} : Pretty prints a [external_expr_desc] value as old FoCal
              source.

    {b Rem} : Not exported ouside this module.                          *)
(* ******************************************************************** *)
and pp_external_expr_desc ppf lst =
  Format.fprintf ppf "@[<2>%a@ @]"
    (Handy.pp_generic_separated_list
       "with"
       (fun local_ppf (ext_lang, ext_expr) ->
	 Format.fprintf local_ppf "%a@ %a@ "
	   pp_external_language ext_lang pp_external_expression ext_expr))
    lst
(* *********************************************************************** *)
(* Format.formatter -> Parsetree.external_expr -> unit                     *)
(** {b Descr} : Pretty prints a [external_expr] value as old FoCal source.

    {b Rem} : Not exported ouside this module.                             *)
(* *********************************************************************** *)
and pp_external_expr ppf = pp_generic_ast pp_external_expr_desc ppf



(* ********************************************************************* *)
(* Format.formatter -> Parsetree.external_expression -> unit             *)
(** {b Descr} : Pretty prints a [external_expression] value as old FoCal
              source.

    {b Rem} : Not exported ouside this module.                           *)
(* ********************************************************************* *)
and pp_external_expression ppf eexpr = Format.fprintf ppf "%s" eexpr
;;



(* ************************************************************************** *)
(* Format.formatter -> Parsetree.species_def_desc -> unit                     *)
(** {b Descr} : Pretty prints a [species_def_desc] value as old FoCal source.

    {b Rem} : Not exported ouside this module.                                *)
(* ************************************************************************** *)
let rec pp_species_def_desc ppf def =
  Format.fprintf ppf "@[<2>species %a " pp_vname def.Parsetree.sd_name ;
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
    end ;
  (* Prints the ancestors only if some. *)
  if def.Parsetree.sd_inherits.Parsetree.ast_desc <> [] then
    begin
    Format.fprintf ppf "inherits %a =@\n"
      (pp_species_exprs ",") def.Parsetree.sd_inherits.Parsetree.ast_desc
    end ;
  Format.fprintf ppf "%a@\nend@]@\n"
    pp_species_fields def.Parsetree.sd_fields
(* ********************************************************************* *)
(* Format.formatter -> Parsetree.species_def -> unit                     *)
(** {b Descr} : Pretty prints a [species_def] value as old FoCal source.

    {b Rem} : Not exported ouside this module.                           *)
(* ********************************************************************* *)
and pp_species_def ppf = pp_generic_ast pp_species_def_desc ppf



(* ******************************************************************* *)
(* Format.formatter -> Parsetree.species_param_type_desc -> unit       *)
(** {b Descr} : Pretty prints a [species_param_type_desc] value as old
              FoCal source.

    {b Rem} : Not exported ouside this module.                         *)
(* ******************************************************************* *)
and pp_species_param_type_desc ppf = function
  | Parsetree.SPT_in ident -> Format.fprintf ppf "in@ %a" pp_ident ident
  | Parsetree.SPT_is species_expr ->
      Format.fprintf ppf "is@ %a" pp_species_expr species_expr
(* ******************************************************************* *)
(* Format.formatter -> Parsetree.species_param_type -> unit            *)
(** {b Descr} : Pretty prints a [species_param_type_desc] value as old
              FoCal source.

    {b Rem} : Not exported ouside this module.                         *)
(* ******************************************************************* *)
and pp_species_param_type ppf = pp_generic_ast pp_species_param_type_desc ppf



and pp_species_expr_desc ppf sed =
  Format.fprintf ppf "%a" pp_ident sed.Parsetree.se_name ;
  if sed.Parsetree.se_params <> [] then
    begin
    Format.fprintf ppf "(@[<1>%a@])"
      (pp_species_params ",") sed.Parsetree.se_params
    end
and pp_species_exprs sep ppf =
  Handy.pp_generic_separated_list sep pp_species_expr ppf
and pp_species_expr ppf = pp_generic_ast pp_species_expr_desc ppf



and pp_species_param_desc ppf = function
  | Parsetree.SP expr -> Format.fprintf ppf "%a" pp_expr expr
and pp_species_params sep ppf =
  Handy.pp_generic_separated_list sep pp_species_param ppf
and pp_species_param ppf = pp_generic_ast pp_species_param_desc ppf



and pp_sig_def_desc ppf sdd =
  Format.fprintf ppf "@[<2>sig@ %a in@ %a"
    pp_vname sdd.Parsetree.sig_name pp_type_expr sdd.Parsetree.sig_type
and pp_sig_def ppf = pp_generic_ast pp_sig_def_desc ppf



and pp_proof_def_desc ppf pdd =
  let hyps_levels = record_hyp_level_in_proof 0 pdd.Parsetree.pd_proof in
  Format.fprintf ppf "@[<2>proof of@ %a@ =@ %a @]"
    pp_vname pdd.Parsetree.pd_name
   (pp_proof hyps_levels) pdd.Parsetree.pd_proof
and pp_proof_def ppf = pp_generic_ast pp_proof_def_desc ppf



and pp_property_def_desc ppf pdd =
  Format.fprintf ppf "@[<2>property@ %a :@ %a"
    pp_vname pdd.Parsetree.prd_name pp_prop pdd.Parsetree.prd_prop
and pp_property_def ppf = pp_generic_ast pp_property_def_desc ppf



and pp_species_field_desc ppf = function
  | Parsetree.SF_rep rep_type_def ->
      Format.fprintf ppf "@[<2>rep@ =@ %a@ ;@]" pp_rep_type_def rep_type_def
  | Parsetree.SF_sig sig_def ->
      Format.fprintf ppf "%a@ ;@]" pp_sig_def sig_def
  | Parsetree.SF_let let_def ->
      Format.fprintf ppf "%a@ ;" pp_let_def let_def
  | Parsetree.SF_property property_def ->
      Format.fprintf ppf "%a@ ;@]" pp_property_def property_def
  | Parsetree.SF_theorem theorem_def ->
      Format.fprintf ppf "%a@ ;@]" pp_theorem_def theorem_def
  | Parsetree.SF_proof proof_def ->
      Format.fprintf ppf "%a@ ;" pp_proof_def proof_def
and pp_species_fields ppf = Handy.pp_generic_newlined_list pp_species_field ppf
and pp_species_field ppf = pp_generic_ast pp_species_field_desc ppf



(* ********************************************************************** *)
(* Format.formatter -> Parsetree.let_def_desc -> unit                     *)
(** {b Descr} : Pretty prints a [let_def_desc] value as old FoCal source.

    {b Rem} : Not exported ouside this module.                            *)
(* ********************************************************************** *)
and pp_let_def_desc ppf ldd =
  Format.fprintf ppf "@[<2>%a"
    pp_let_def_binding_flags
    (ldd.Parsetree.ld_rec, ldd.Parsetree.ld_logical, ldd.Parsetree.ld_local) ;
  (* Now print the bindings. This is especially handled because bindings *)
  (* after the first one ar separated by "and" instead of "let".         *)
  match ldd.Parsetree.ld_bindings with
   | [] ->
       (* The let construct should always at least bind one identifier ! *)
       assert false
   | [one] -> Format.fprintf ppf "%a@]" pp_binding one
   | first :: nexts ->
       Format.fprintf ppf "%a" pp_binding first ;
       List.iter
	 (fun b -> Format.fprintf ppf "@]@\n@[<2>and %a" pp_binding b)
	 nexts ;
       Format.fprintf ppf "@]"
(* ***************************************************************** *)
(* Format.formatter -> Parsetree.let_def -> unit                     *)
(** {b Descr} : Pretty prints a [let_def] value as old FoCal source.

    {b Rem} : Not exported ouside this module.                       *)
(* ***************************************************************** *)
and pp_let_def ppf = pp_generic_ast pp_let_def_desc ppf



(* ********************************************************************** *)
(* Format.formatter -> Parsetree.binding_desc -> unit                     *)
(** {b Descr} : Pretty prints a [binding_desc] value as old FoCal source.

    {b Rem} : Not exported ouside this module.                            *)
(* ********************************************************************** *)
and pp_binding_desc ppf bd =
  Format.fprintf ppf "%a" pp_vname bd.Parsetree.b_name ;
  (* Prints the parameters only if some. *)
  if bd.Parsetree.b_params <> [] then
    begin
    Format.fprintf ppf "@ (%a)"
      (Handy.pp_generic_separated_list
	 ","
	 (fun local_ppf (vname, ty_expr_opt) ->
	   (* Prints the type only if it is provided. *)
	   Format.fprintf local_ppf "%a%a"
	     pp_vname vname
	     (Handy.pp_generic_option " in " pp_type_expr) ty_expr_opt))
      bd.Parsetree.b_params
    end ;
    Format.fprintf ppf "%a@ =@ %a"
      (Handy.pp_generic_option " in " pp_type_expr) bd.Parsetree.b_type
      pp_expr bd.Parsetree.b_body
(* ************************************************************* *)
(* Format.formatter -> Parsetree.binding -> unit                 *)
(** {b Descr} : Pretty prints a [binding] value as FoCal source.

    {b Rem} : Not exported ouside this module.                   *)
(* ************************************************************* *)
and pp_binding ppf = pp_generic_ast pp_binding_desc ppf



(* ************************************************************************** *)
(* Format.formatter -> Parsetree.theorem_def_desc -> unit                     *)
(** {b Descr} : Pretty prints a [theorem_def_desc] value as old FoCal source.

    {b Rem} : Not exported ouside this module.                                *)
(* ************************************************************************** *)
and pp_theorem_def_desc ppf tdd =
 let hyps_levels = record_hyp_level_in_proof 0 tdd.Parsetree.th_proof in
  Format.fprintf ppf "@[<2>theorem %a :@ %a@ %a@]@\n@[<2>proof:@ %a@ "
    pp_vname tdd.Parsetree.th_name
    pp_local_flag tdd.Parsetree.th_local
    pp_prop tdd.Parsetree.th_stmt
    (pp_proof hyps_levels) tdd.Parsetree.th_proof
(* ********************************************************************* *)
(* Format.formatter -> Parsetree.theorem_def -> unit                     *)
(** {b Descr} : Pretty prints a [theorem_def] value as old FoCal source.

    {b Rem} : Not exported ouside this module.                           *)
(* ********************************************************************* *)
and pp_theorem_def ppf = pp_generic_ast pp_theorem_def_desc ppf



(* *********************************************************************** *)
(* (Parsetree.vname * int) list -> Format.formatter ->                     *)
(*   Parsetree.fact_desc -> unit                                           *)
(** {b Descr} : Pretty prints a list of [fact_desc] values as old FoCal
              source. It tries to extract from the mapping between
              hypotheses and definition levels, the level to print before
              any hypothesis usage in [F_hypothesis] facts.
    {b Rem} : Not exported ouside this module.                             *)
(* *********************************************************************** *)
and pp_fact_desc hyps_levels ppf = function
  | Parsetree.F_def idents ->
      Format.fprintf ppf "def %a" (pp_expr_idents ",") idents
  | Parsetree.F_property idents ->
      Format.fprintf ppf "%a" (pp_expr_idents ",") idents
  | Parsetree.F_hypothesis vnames ->
      (* No "hypothesis" keyword in the old syntax. Although one must   *)
      (* add the level in the proof where this hypothesis was declared. *)
      let full_vnames =
	List.map
	  (fun vname ->
	    let level =
	      (try List.assoc vname hyps_levels with
	      | Not_found ->
		  Printf.eprintf
                    "Warning : no definition level found for hypothesis.\n" ;
		  Format.fprintf ppf
                    "(* Warning : no definition level found for hypothesis \
                     \"%a\". *)"
                    pp_vname vname ;
		  -1) in
	    (vname, level))
	  vnames in
      Format.fprintf ppf "%a" (pp_vnames_n_level ",") full_vnames
  | Parsetree.F_node node_labels ->
      (* No "step" keyword in the old syntax. *)
      Format.fprintf ppf "%a" (pp_node_labels ",") node_labels



(* *********************************************************************** *)
(* (Parsetree.vname * int) list ->                                         *)
(*   Format.formatter -> Parsetree.fact list -> unit                       *)
(** {b Descr} : Pretty prints a list of [fact_desc] values as old FoCal
              source.
              Be carreful : in the old syntax only 2 catagories existed :
              "def" and others. Hence, because no comma was required
              between others and the "def" keyword and because commas were
              required between each element of "def" section and of the
              other section, one must manually merge [F_property],
              [F_hypothesis] and [F_node] in order to generate one unique
              list COMMA-SEPARATED !

    {b Rem} : Not exported ouside this module.                              *)
(* ************************************************************************ *)
and pp_and_merge_facts hyps_levels ppf facts =
  let (facts_def, facts_other) =
    List.partition
      (function  { Parsetree.ast_desc = fact_desc } ->
	match fact_desc with Parsetree.F_def _ -> true | _ -> false)
      facts in
  Handy.pp_generic_separated_list "," (pp_fact hyps_levels) ppf facts_other ;
  Format.fprintf ppf "@ " ;
  Handy.pp_generic_separated_list "" (pp_fact hyps_levels) ppf facts_def
and pp_fact hyps_levels ppf = pp_generic_ast (pp_fact_desc hyps_levels) ppf



and pp_proof_desc hyps_levels ppf = function
  | Parsetree.Pf_assumed -> Format.fprintf ppf "assumed"
  | Parsetree.Pf_auto facts ->
      (* Empty facts list means end-of-proof. In the old *)
      (* syntax, no "." is required. So, just ignore.    *)
      (* Unde rthis points, hypotheses may be used. So we must transmit  *)
      (* the mapping between hypotheses-names and levels to the [facts]. *)
      if facts <> [] then
	Format.fprintf ppf "@[<2>by %a@]" (pp_and_merge_facts hyps_levels) facts
  | Parsetree.Pf_coq s -> Format.fprintf ppf "@[<2>coq proof@ {*%s*}@]" s
  | Parsetree.Pf_node proof_nodes ->
      Format.fprintf ppf "%a" (pp_proof_nodes "" hyps_levels) proof_nodes
and pp_proof hyps_levels ppf = pp_generic_ast (pp_proof_desc hyps_levels) ppf



and pp_proof_node_desc hyps_levels ppf = function
  | Parsetree.PN_sub (node_label, stmt, proof) ->
      Format.fprintf ppf "%a %a@\n%a"
	pp_node_label node_label pp_statement stmt
        (pp_proof hyps_levels) proof
  | Parsetree.PN_qed (node_label, proof) ->
      Format.fprintf ppf "%a qed@\n%a"
        pp_node_label node_label (pp_proof hyps_levels) proof
and pp_proof_nodes sep hyps_levels ppf =
  Handy.pp_generic_separated_list sep (pp_proof_node hyps_levels) ppf
and pp_proof_node hyps_levels ppf =
  pp_generic_ast (pp_proof_node_desc hyps_levels) ppf



and pp_statement_desc ppf stmt =
  if stmt.Parsetree.s_hyps <> [] then
    Format.fprintf ppf "assume %a@ "
      pp_hyps stmt.Parsetree.s_hyps ;
  Format.fprintf ppf "%a"
    (Handy.pp_generic_option "prove " pp_prop) stmt.Parsetree.s_concl
and pp_statement ppf = pp_generic_ast pp_statement_desc ppf



and pp_hyp_desc ppf = function
  | Parsetree.H_var (vname, te) ->
      Format.fprintf ppf "@[<2>%a in@ %a@]"
	pp_vname vname pp_type_expr te
  | Parsetree.H_hyp (vname, prop) ->
      Format.fprintf ppf "@[<2>%a :@ %a@]" pp_vname vname pp_prop prop
  | Parsetree.H_not (vname, expr) ->
      Printf.eprintf "Warning : ignored notation directive.\n" ;
      Format.fprintf ppf
	"@[<2>(* Warning : ignored notation directive %a =@ %a *)@ @]"
	pp_vname vname pp_expr expr

and pp_hyps ppf =
  (* No comma between hypotheses in the old syntax ! *)
  Handy.pp_generic_separated_list "" pp_hyp ppf
and pp_hyp ppf = pp_generic_ast pp_hyp_desc ppf



and pp_prop_desc ppf = function
  | Parsetree.Pr_forall (vnames, type_expr_opt, prop) ->
      Format.fprintf ppf "@[<2>all@ %a@ in@ %a,@ %a@]"
	(pp_vnames "") vnames pp_type_expr type_expr_opt pp_prop prop
  | Parsetree.Pr_exists (vnames, type_expr_opt, prop) ->
      Format.fprintf ppf "@[<2>ex@ %a@ in@ %a,@ %a@]"
	(pp_vnames "") vnames pp_type_expr type_expr_opt pp_prop prop
  | Parsetree.Pr_imply (p1, p2) ->
      Format.fprintf ppf "@[<2>%a@ ->@ %a@]" pp_prop p1 pp_prop p2
  | Parsetree.Pr_or (p1, p2) ->
      Format.fprintf ppf "@[<2>%a@ or@ %a@]" pp_prop p1 pp_prop p2
  | Parsetree.Pr_and (p1, p2) ->
      Format.fprintf ppf "@[<2>%a@ and@ %a@]" pp_prop p1 pp_prop p2
  | Parsetree.Pr_equiv (p1, p2) ->
      Format.fprintf ppf "@[<2>%a@ <->@ %a@]" pp_prop p1 pp_prop p2
  | Parsetree.Pr_not p -> Format.fprintf ppf "@[<2>not@ %a@]" pp_prop p
  | Parsetree.Pr_expr e -> Format.fprintf ppf "%a" pp_expr e
  | Parsetree.Pr_paren p -> Format.fprintf ppf "@[<1>(%a)@]" pp_prop p
and pp_prop ppf = pp_generic_ast pp_prop_desc ppf



and pp_expr_desc ppf = function
  | Parsetree.E_self -> Format.fprintf ppf "Self"
  | Parsetree.E_const cst -> Format.fprintf ppf "%a" pp_constant cst
  | Parsetree.E_fun (vnames, expr) ->
      Format.fprintf ppf "@[<2>fun %a ->@ %a@]"
	(pp_vnames "") vnames pp_expr expr
  | Parsetree.E_var id -> Format.fprintf ppf "%a" pp_expr_ident id
  | Parsetree.E_app (expr, exprs) ->
      (* Because old syntax didn't allow operator definitions with the *)
      (* infix and postfix positions, there is no need to plan special *)
      (* cases during application : everything is applies like a       *)
      (* regular function. Moreover, any "operator" identifier being   *)
      (* renamed, there is no risk to get something like "= (x, y)".   *)
      Format.fprintf ppf "@[<2>%a@ (%a)@]" pp_expr expr (pp_exprs ",") exprs
  | Parsetree.E_constr (cstr_expr, exprs) ->
      Format.fprintf ppf "@[<2>%a@ (%a)@]"
	pp_constructor_ident cstr_expr (pp_exprs ",") exprs
  | Parsetree.E_match (expr, pat_exprs) ->
      (begin
      Format.fprintf ppf "@[<2>match@ %a@ with@ " pp_expr expr ;
      List.iter
	(fun (pat, e) ->
	  Format.fprintf ppf "@[<2>| %a ->@ %a@]" pp_pattern pat pp_expr e)
	pat_exprs ;
      Format.fprintf ppf "@]"
      end)
  | Parsetree.E_if (expr1, expr2, expr3) ->
      Format.fprintf ppf "@[<2>if@ %a@ then@ %a@ else@ %a@]"
	pp_expr expr1 pp_expr expr2 pp_expr expr3
  | Parsetree.E_let (let_def, expr) ->
      Format.fprintf ppf "%a@ in@ %a" pp_let_def let_def pp_expr expr
  | Parsetree.E_record label_exprs ->
      Format.fprintf ppf "{@[<2>@ %a@ @]}"
	(Handy.pp_generic_separated_list
	   ";"
	   (fun local_ppf (lab_name, e) ->
	     Format.fprintf local_ppf "%s@ = @ %a" lab_name pp_expr e))
	label_exprs
  | Parsetree.E_record_access (expr, label_name) ->
      Format.fprintf ppf "%a.%s" pp_expr expr label_name
  | Parsetree.E_record_with (expr, label_exprs) ->
      Format.fprintf ppf "{@[<2>@ %a@ with@ %a@ @]}"
	pp_expr expr
	(Handy.pp_generic_separated_list
	   ";"
	   (fun local_ppf (lab_name, e) ->
	     Format.fprintf local_ppf "%s@ =@ %a" lab_name pp_expr e))
	label_exprs
  | Parsetree.E_tuple exprs ->
      Format.fprintf ppf "@[<1>(%a)@]" (pp_exprs ",") exprs
  | Parsetree.E_external external_expr ->
      Format.fprintf ppf "%a" pp_external_expr external_expr
  | Parsetree.E_paren expr ->
      Format.fprintf ppf "@[<1>(%a)@]" pp_expr expr
and pp_exprs sep ppf = Handy.pp_generic_separated_list sep pp_expr ppf
and pp_expr ppf = pp_generic_ast pp_expr_desc ppf
;;



let pp_coll_def_desc ppf cdd =
  Format.fprintf ppf "@[<2>collection@ %a@ implements@ %a@\nend@@,;;@]@\n"
    pp_vname cdd.Parsetree.cd_name pp_species_expr cdd.Parsetree.cd_body
;;
let pp_coll_def ppf = pp_generic_ast pp_coll_def_desc ppf
;;



let pp_tmp_TD_union ppf l =
  Format.fprintf ppf "@[<2>%a@]"
    (Handy.pp_generic_separated_list
       "|"
       (fun local_ppf (constr_name, type_exprs) ->
	 Format.fprintf local_ppf "%a" pp_vname constr_name ;
	 (* Print constructor's arguments if some. *)
	 if type_exprs <> [] then
	   Format.fprintf ppf " of@ %a" (pp_type_exprs "*") type_exprs))
    l
;;
	   


let rec pp_type_def_desc ppf td =
  Format.fprintf ppf "@[<2>type " ;
  (* Print type's parameters if some. *)
  if td.Parsetree.td_params <> [] then
    Format.fprintf ppf "(%a) "
      (Handy.pp_generic_separated_list
	 ","
	 (fun local_ppf s -> Format.fprintf local_ppf "%a" pp_vname s))
      td.Parsetree.td_params ;
  Format.fprintf ppf "%a =@ %a@]"
    pp_vname td.Parsetree.td_name pp_type_body td.Parsetree.td_body
and pp_type_def ppf = pp_generic_ast pp_type_def_desc ppf



and pp_type_body_desc ppf = function
  | Parsetree.TD_alias te ->
      Format.fprintf ppf "@[<2>alias@ %a@]" pp_type_expr te
  | Parsetree.TD_union l -> Format.fprintf ppf "%a" pp_tmp_TD_union l
  | Parsetree.TD_record lab_exprs ->
      Format.fprintf ppf "@[<2>{@ %a@ }@]"
	(Handy.pp_generic_separated_list
	   ";"
	   (fun local_ppf (lab, e) ->
	     Format.fprintf local_ppf "%s@ =@ %a" lab pp_type_expr e))
	lab_exprs
and pp_type_body ppf = pp_generic_ast pp_type_body_desc ppf
;;



let pp_phrase_desc ppf = function
  | Parsetree.Ph_external ext_def ->
      Format.fprintf ppf "%a" pp_external_def ext_def
  | Parsetree.Ph_use fname ->
      (* It seems that old syntax didn't accept file paths. *)
      if (Filename.dirname fname) <> "." then
	(begin
	Printf.eprintf "Warning : use directive using relative path \"%s\".\n"
	  fname ;
	Format.fprintf ppf
	  "@[<2>(* Warning :  use directive using relative path \"%s\". *)@]@\n"
	  fname
	end) ;
      Format.fprintf ppf "@[<2>uses@ %s@@,;;@]@\n" (Filename.basename fname)
  | Parsetree.Ph_open fname ->
      (* It seems that old syntax didn't accept file paths. *)
      if (Filename.dirname fname) <> "." then
	(begin
	Printf.eprintf "Warning : open directive using relative path \"%s\".\n"
	  fname ;
	Format.fprintf ppf
	  "@[<2>(* Warning : open directive using relative path \"%s\". *)@]@\n"
	  fname
	end) ;
      Format.fprintf ppf "@[<2>open@ %s@@,;;@]@\n" (Filename.basename fname)
  | Parsetree.Ph_species s_def -> Format.fprintf ppf "%a" pp_species_def s_def
  | Parsetree.Ph_coll coll_def -> Format.fprintf ppf "%a" pp_coll_def coll_def
  | Parsetree.Ph_type type_def -> Format.fprintf ppf "%a" pp_type_def type_def
  | Parsetree.Ph_let let_def -> Format.fprintf ppf "%a" pp_let_def let_def
  | Parsetree.Ph_theorem t_def -> Format.fprintf ppf "%a@]" pp_theorem_def t_def
  | Parsetree.Ph_expr expr -> Format.fprintf ppf "%a" pp_expr expr
;;
let pp_phrase ppf = pp_generic_ast pp_phrase_desc ppf
;;
let pp_phrases ppf = Handy.pp_generic_newlined_list pp_phrase ppf
;;



(* ********************************************************************* *)
(* pp_file_desc : Format.formatter -> Parsetree.file_desc -> unit *)
(** {b Descr} : Pretty prints a [file_desc] value as old FoCal source.

    {b Rem} : Not exported ouside this module.                             *)
(* ********************************************************************* *)
let pp_file_desc ppf = function
  | Parsetree.File phrases -> Format.fprintf ppf "%a" pp_phrases phrases
;;
(* ******************************************************************** *)
(* pp_file : Format.formatter -> Parsetree.file -> unit          *)
(** {b Descr} : Pretty prints a [file] value as old FoCal source. It also
              cleans-up the table of encountered identifiers that were
              not modified by the renaming processing.

    {b Rem} : Exported ouside this module.                                *)
(* ******************************************************************** *)
let pp_file ppf file =
  cleanup_conflicting_idents_table () ;
  pp_generic_ast pp_file_desc ppf file
;;
