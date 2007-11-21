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

(* $Id: parsetree.mli,v 1.29 2007-11-21 16:34:15 pessaux Exp $ *)

(** {2 The Focalize abstract syntax tree.} *)

(** The parse tree, or shallow abstract syntax.
   Disambiguation has not yet been done.
   This is the input type of the disambiguation pass.
   The disambiguation pass has to resolve global/local/method
   classification for idents. *)

(** {3 The generic polymorphic type of AST nodes.} *)

(** AST documentation support:
    documentation elements are special lexems that can appear in specified
    nodes according to the grammar of the language.
    A documentation is a list of such documentation elements.
    The contents of documentation elements is free style, provided that the
    documentation element is a legal language comment (hence, the usual end of
    comment marker of the language cannot appear in a documentation element).
    Note that an empty list of documentation elements stands for no
    documentation at all. *)
type documentation = doc_elem list
and doc_elem = {
  de_loc : Location.t;
  de_desc : string;
}
;;

type ast_node_type_information =
  | ANTI_none
  | ANTI_non_relevant
  | ANTI_type of Types.type_simple
  | ANTI_scheme of Types.type_scheme
;;

type 'a ast = {
   (** The location in the source of the AST node. *)
   ast_loc : Location.t;
   (** The description of the node. *)
   ast_desc : 'a;
   (** The support for documentation. *)
   ast_doc : documentation;
   (** The type of the node. *)
   mutable ast_type : ast_node_type_information
}
;;

(** {3 Names of the various entities.} *)

(** {6 General names.} *)

type modname = Types.fname
(** The type of ``module'' names.
  Since there are no modules in Focalize yet, modules are just files and
  module names are just file names. *)
;;

type vname =
   | Vlident of string  (** Lowercase ident. *)
   | Vuident of string  (** Capitalized ident. *)
   | Vpident of string  (** Prefix operator ident. *)
   | Viident of string  (** Infix operator ident. *)
   | Vqident of string  (** "Quote" ident for type variables. *)
(** The type of variables classified by their respective lexical category,
  which can be regular identifiers (lowercase or capitalized), infix, or
  prefix identifiers. *)
;;

type qualified_species = modname * vname
(** The name of a species qualified with its defining module. *)
;;

type qualified_vname =
   | Vname of vname
   | Qualified of modname * vname
(** A [vname] with possibly a modulename qualification. *)
;;

type constructor_name = vname
(** A constructor name as mentioned in type definitions. *)
;;
type label_name = vname
(** A label name as mentioned in type definitions. *)
;;

type node_label = int * string
(** A node label in a proof as a intger level and a string label. *)
;;

(** {6 Identifiers specific to expressions and patterns.} *)

type expr_ident = expr_ident_desc ast
and expr_ident_desc =
  | EI_local of vname
  | EI_global of qualified_vname
  | EI_method of qualified_vname option * vname
    (** The optional collection name before the "!" sign,
        and the name of the method. *)
(** The identifiers that appear in expressions: they could be globally or
    locally bound identifiers or method names. *)
;;

type constructor_ident = constructor_ident_desc ast
and constructor_ident_desc =
  | CI of qualified_vname
(** The constructor names that can appear in an expression or a pattern.
    This is always a global uppercase qualified identifier. *)
;;

type label_ident = label_ident_desc ast
and label_ident_desc =
  | LI of qualified_vname
(** The label names that can appear in an expression or a pattern.
    This is always a global lowercase qualified identifier. *)
;;

(** {6 Other identifiers.} *)

type ident = ident_desc ast
and ident_desc =
  | I_local of vname
  | I_global of qualified_vname
(** Unclassified identifiers: identifiers that can appear anywhere
    in the parse trees. *)
;;

(** {3 Type expressions.} *)

(** Types for representations of collections. *)

type rep_type_def = rep_type_def_desc ast
and rep_type_def_desc =
  | RTE_ident of ident
  | RTE_fun of rep_type_def * rep_type_def
  | RTE_app of ident * rep_type_def list
  | RTE_prod of rep_type_def list
  | RTE_paren of rep_type_def
;;

(** Types for all other entites: values, constructors, ... *)

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

(** {3 External definitions for values and constructors.} *)

(** {6 External languages name definitions.} *)

(** The external languages known to the compiler are [Caml] and [Coq].
    Any other language can be mentioned as external using its name
    which is uninterpreted. *)

type external_language =
  | EL_Caml
  | EL_Coq
  | EL_external of string
;;

(** {6 External expressions.} *)

(** An external expression is a list that binds an external language name
    to an expression in this language.*)

type external_expr = external_expr_desc ast
and external_expr_desc =
    (external_language * external_code) list

and external_code = string
(** Foreign expressions are not parsed: they are just considered
    as unstructured strings of bytes. *)
;;

(** {3 Type definitions.} *)

(** Type definitions can be either external type definitions,
    or simple type definitions of the language. *)

type type_def = type_def_desc ast
and type_def_desc = {
  td_name : vname;
  td_params : vname list;
  td_body : type_def_body;
}

and type_def_body = type_def_body_desc ast
and type_def_body_desc =
    (** Regular type definitions (unions, records, and aliases). *)
  | TDB_simple of simple_type_def_body
    (** External type definitions. *)
  | TDB_external of external_type_def_body

and external_type_def_body = external_type_def_body_desc ast
and external_type_def_body_desc = {
  (** The internal view of the externally defined type. *)
  etdb_internal : simple_type_def_body option;
  (** The external view of the externally defined type. *)
  etdb_external : external_expr;
  (** The external mapping of constructors of labels of the externally
      defined type. *)
  etdb_bindings : external_bindings;
 }

and simple_type_def_body = simple_type_def_body_desc ast
and simple_type_def_body_desc =
  | STDB_alias of type_expr
  | STDB_union of (constructor_name * type_expr list) list
  | STDB_record of (label_name * type_expr) list

and external_bindings = external_bindings_desc ast
and external_bindings_desc = external_binding list

and external_binding = external_binding_desc ast
and external_binding_desc = (vname * external_expr)
;;

(** {3 Patterns.} *)

type constant = constant_desc ast
and constant_desc =
  | C_int of string
  | C_float of string
  | C_bool of string
  | C_string of string
  | C_char of char
;;

type pattern = pat_desc ast
and pat_desc =
  | P_const of constant
  | P_var of vname
  | P_as of pattern * vname
  | P_wild
  | P_constr of constructor_ident * pattern list
  | P_record of (label_ident * pattern) list
  | P_tuple of pattern list
  | P_paren of pattern
;;

(** {3 Expressions.} *)

(** {6 Various flags for let definitions.} *)

type rec_flag = | RF_no_rec | RF_rec;;

type logical_flag = | LF_no_logical | LF_logical;;

type local_flag = | LF_no_local | LF_local;;

(** {6 Expressions and let definitions.} *)

type expr = expr_desc ast
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
  | E_record of (label_ident * expr) list
  | E_record_access of expr * label_ident
  | E_record_with of expr * (label_ident * expr) list
  | E_tuple of expr list
  | E_external of external_expr
  | E_paren of expr

and let_def = let_def_desc ast
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
;;

(** {3 Proofs.} *)

(** {6 Propositions.} *)

type prop = prop_desc ast
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
;;

(** {6 Hypotheses, statements and facts.} *)

type hyp = hyp_desc ast
and hyp_desc =
  | H_var of vname * type_expr
  | H_hyp of vname * prop
  | H_not of vname * expr
;;

type statement = statement_desc ast
and statement_desc = {
  s_hyps : hyp list;
  s_concl : prop option;
}
;;

type fact = fact_desc ast
and fact_desc =
  | F_def of expr_ident list
  | F_property of expr_ident list
  | F_hypothesis of vname list
  | F_node of node_label list
;;

(** Proofs. *)

type proof = proof_desc ast
and proof_desc =
  | Pf_assumed
  | Pf_auto of fact list
  | Pf_coq of string
  | Pf_node of proof_node list

and proof_node = proof_node_desc ast
and proof_node_desc =
  | PN_sub of node_label * statement * proof
  | PN_qed of node_label * proof
;;

(** {3 Species definitions.} *)

(** {6 Intra species definitions.} *)

(** Signatures, proofs, theorems, and property definitions that
    may appear inside species.} *)

type sig_def = sig_def_desc ast
and sig_def_desc = {
  sig_name : vname;
  sig_type : type_expr;
}
;;

type proof_def = proof_def_desc ast
and proof_def_desc = {
  pd_name : vname;
  pd_proof : proof;
}
;;

type property_def = property_def_desc ast
and property_def_desc = {
  prd_name : vname;
  prd_prop : prop;
}
;;

type theorem_def = theorem_def_desc ast
and theorem_def_desc = {
  th_name : vname;
  th_local : local_flag;
  th_stmt : prop;
  th_proof : proof;
}
;;

(** {6 Species expressions.} *)

type species_expr = species_expr_desc ast
and species_expr_desc = {
  se_name : ident;
  se_params : species_param list;
}

and species_param = species_param_desc ast
and species_param_desc =
  | SP of expr
;;

(** {6 Species definitions.} *)

type species_def = species_def_desc ast
and species_def_desc = {
  sd_name : vname;
  sd_params : (vname * species_param_type) list;
  sd_inherits : (species_expr list) ast;
  sd_fields : species_field list;
}

and species_param_type = species_param_type_desc ast
and species_param_type_desc =
  | SPT_in of ident
  | SPT_is of species_expr

and species_field = species_field_desc ast
and species_field_desc =
  | SF_rep of rep_type_def
  | SF_sig of sig_def
  | SF_let of let_def
  | SF_property of property_def
  | SF_theorem of theorem_def
  | SF_proof of proof_def
;;

(** {3 Collection definitions.} *)

type coll_def = coll_def_desc ast
and coll_def_desc = {
  cd_name : vname;
  cd_body : species_expr;
}
;;

(** {3 Toplevel entities.} *)

type expr_def = expr
;;

type phrase = phrase_desc ast
and phrase_desc =
  | Ph_use of Types.fname
  | Ph_open of Types.fname
  | Ph_species of species_def
  | Ph_coll of coll_def
  | Ph_type of type_def
  | Ph_let of let_def
  | Ph_theorem of theorem_def
  | Ph_expr of expr_def
;;

type file = file_desc ast
and file_desc =
  | File of phrase list
;;
