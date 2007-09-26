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

(* $Id: parsetree.mli,v 1.15 2007-09-26 16:11:22 weis Exp $ *)

(** The parse tree, or shallow abstract syntax.
   Disambiguation has not yet been done.
   This is the input type of the disambiguation pass.
   The disambiguation pass has to :
   - resolve global/local/method classification for idents.
*)

(** Types of various identifiers in the abstract syntax tree. *)
type vname =
   | Vlident of string  (** Lowercase ident. *)
   | Vuident of string  (** Capitalized ident. *)
   | Vpident of string  (** Prefix operator ident. *)
   | Viident of string  (** Infix operator ident. *)
   | Vqident of string  (** "Quote" ident for type variables. *)
     (** Variable names are classified with respect to their lexical class,
	 which can be regular. infix or prefix. *)
;;

type constructor_name = vname
     (** Constructor name. *)
;;

type node_label = int * string
     (** Node label in proof. *)
;;

type external_name = string * string
     (** External name from Ocaml or Coq: module name and identifier. *)
;;

type ('a, 'b) generic_ast = {
   ast_loc : Location.t; (** The location in the source of the AST node. *)
   ast_desc : 'a;        (** The description of the node. *)
   ast_doc : 'b option;  (** The support for documentation in many formats. *)
   mutable ast_type : Types.type_simple option;
                         (** The type of the node. *)
}
;;

type 'a ast = ('a, string) generic_ast;;
type 'a ast_doc = ('a, string) generic_ast;;

type expr_ident = expr_ident_desc ast
and expr_ident_desc =
  | EI_local of vname
  | EI_global of Types.fname option * vname
  | EI_method of Types.collection_name option * vname
;;

type ident = ident_desc ast
and ident_desc =
  | I_local of vname
  | I_global of Types.fname option * vname
;;

(* ********************************************************************** *)
(** [Descr] : Structure of the constructor part of an expression. Because
              type constructors are always toplevel idents, neither local
              nor a method, we extracted from [ident_desc] the only
              relevant information. Having such a separate type prevents
              from having a general expression as the constructor part of
              the [expr_desc]'s [E_constr] case.

    [Rem] : Exported outside this module.                                 *)
(* ********************************************************************** *)
type constructor_ident = constructor_ident_desc ast
and constructor_ident_desc =
  | CE of Types.fname option * vname
;;

type rep_type_def = rep_type_def_desc ast_doc
and rep_type_def_desc =
  | RTE_ident of ident
  | RTE_fun of rep_type_def * rep_type_def
  | RTE_app of ident * rep_type_def list
  | RTE_prod of rep_type_def list
  | RTE_paren of rep_type_def
;;

type type_expr = type_expr_desc ast
and type_expr_desc =
  | TE_ident of ident
  | TE_fun of type_expr * type_expr
  | TE_app of ident * type_expr list
  | TE_prod of type_expr list
  | TE_self
  | TE_prop
  | TE_paren of type_expr
;;

type constant = constant_desc ast
and constant_desc =
  | C_int of string
  | C_float of string
  | C_bool of string
  | C_string of string
  | C_char of char
;;

type rec_flag = | RF_no_rec | RF_rec
;;

type logical_flag = | LF_no_logical | LF_logical
;;

type local_flag = | LF_no_local | LF_local
;;

type pattern = pat_desc ast
and pat_desc =
  | P_const of constant
  | P_var of vname
  | P_as of pattern * vname
  | P_wild
  | P_app of constructor_ident * pattern list
  | P_record of (Types.label_name * pattern) list
  | P_tuple of pattern list
  | P_paren of pattern
;;

(** External languages definitions. *)
type external_language =
  | EL_Caml
  | EL_Coq
  | EL_external of string
(** The external languages known to the compiler are [Caml], [Coq], and any
    other mentioned as such language name which is an uninterpreted string. *)
;;

type external_def = external_def_desc ast
and external_def_desc =
  | ED_type of external_type_def_body
  | ED_value of external_value_def_body
(** An external definitions can define a new type or a new value. *)

and external_type_def_body = external_type_def_body_desc ast
and external_type_def_body_desc = {
  etd_name : vname;
  etd_params : vname list;    (** For types to know their arity. *)
  etd_body : external_expr;
}

and external_value_def_body = external_value_def_body_desc ast
and external_value_def_body_desc = {
  evd_name : vname;
  evd_type : type_expr;
  evd_body : external_expr;
}

(** The body of an external definitions contains the name defined
    and its definition in some external language. *)

and external_expr = external_expr_desc ast
and external_expr_desc =
    (external_language * external_expression) list

and external_expression = string
    (** External expressions are not parsed: they are just considered
        as strings of bytes. *)
;;

type species_def = species_def_desc ast_doc
and species_def_desc = {
  sd_name : Types.species_name;
  sd_params : (vname * species_param_type) list;
  sd_inherits : (species_expr list) ast_doc;
  sd_fields : species_field list;
}

and species_param_type = species_param_type_desc ast
and species_param_type_desc =
  | SPT_in of ident
  | SPT_is of species_expr

and species_expr = species_expr_desc ast
and species_expr_desc = {
  se_name : ident;
  se_params : species_param list;
}

and species_param = species_param_desc ast
and species_param_desc =
  | SP of expr

and sig_def = sig_def_desc ast_doc
and sig_def_desc = {
  sig_name : vname;
  sig_type : type_expr;
}

and proof_def = proof_def_desc ast_doc
and proof_def_desc = {
  pd_name : vname;
  pd_proof : proof;
}

and property_def = property_def_desc ast_doc
and property_def_desc = {
  prd_name : vname;
  prd_prop : prop;
}

and species_field = species_field_desc ast
and species_field_desc =
  | SF_rep of rep_type_def
  | SF_sig of sig_def
  | SF_let of let_def
  | SF_property of property_def
  | SF_theorem of theorem_def
  | SF_proof of proof_def

and let_def = let_def_desc ast_doc
and let_def_desc = {
  ld_rec : rec_flag;
  ld_logical : logical_flag;
  ld_local : local_flag;
  ld_bindings : binding list;
}
and binding = binding_desc ast
and binding_desc = {
  b_name : vname;
  b_params : (vname * type_expr option) list;
  b_type : type_expr option;
  b_body : expr;
}

and theorem_def = theorem_def_desc ast_doc
and theorem_def_desc = {
  th_name : vname;
  th_local : local_flag;
  th_stmt : prop;
  th_proof : proof;
}

and fact = fact_desc ast
and fact_desc =
  | F_def of ident list
  | F_property of ident list
  | F_hypothesis of vname list
  | F_node of node_label list

and proof = proof_desc ast
and proof_desc =
  | Pf_assumed
  | Pf_auto of fact list
  | Pf_coq of string
  | Pf_node of proof_node list

and proof_node = proof_node_desc ast
and proof_node_desc =
  | PN_sub of node_label * statement * proof
  | PN_qed of node_label * proof

and statement = statement_desc ast
and statement_desc = {
  s_hyps : hyp list;
  s_concl : prop option;
}

and hyp = hyp_desc ast
and hyp_desc =
  | H_var of vname * type_expr
  | H_hyp of vname * prop
  | H_not of vname * expr

and prop = prop_desc ast
and prop_desc =
  | Pr_forall of vname list * type_expr * prop
  | Pr_exists of vname list * type_expr * prop
  | Pr_imply of prop * prop
  | Pr_or of prop * prop
  | Pr_and of prop * prop
  | Pr_equiv of prop * prop
  | Pr_not of prop
  | Pr_expr of expr
  | Pr_paren of prop

and expr = expr_desc ast
and expr_desc =
  | E_self
  | E_const of constant
  | E_fun of vname list * expr
  | E_var of expr_ident
  | E_app of expr * expr list
  | E_constr of constructor_ident * expr list
  | E_match of expr * (pattern * expr) list
  | E_if of expr * expr * expr
  | E_let of let_def * expr
  | E_record of (Types.label_name * expr) list
  | E_record_access of expr * Types.label_name
  | E_record_with of expr * (Types.label_name * expr) list
  | E_tuple of expr list
  | E_external of external_expr
  | E_paren of expr
;;

type coll_def = coll_def_desc ast_doc
and coll_def_desc = {
  cd_name : Types.collection_name;
  cd_body : species_expr;
};;

type type_def = type_def_desc ast
and type_def_desc = {
  td_name : vname;
  td_params : vname list;
  td_body : type_body;
}

and type_body = type_body_desc ast
and type_body_desc =
  | TD_alias of type_expr
  | TD_union of (constructor_name * type_expr list) list
  | TD_record of (Types.label_name * type_expr) list
;;

(** Toplevel expressions. *)
type expr_def = expr
;;

type phrase = phrase_desc ast
and phrase_desc =
  | Ph_external of external_def
  | Ph_use of Types.fname
  | Ph_open of Types.fname
  | Ph_species of species_def
  | Ph_coll of coll_def
  | Ph_type of type_def
  | Ph_let of let_def
  | Ph_theorem of theorem_def
  | Ph_expr of expr_def
;;

type file = file_desc ast_doc
and file_desc =
  | File of phrase list
;;
