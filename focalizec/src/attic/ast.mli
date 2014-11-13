(* $Id: ast.mli,v 1.1 2007-06-29 05:43:53 pessaux Exp $ *)

(* Abstract syntax (deep version, i.e. disambiguation has been done).
   This is the input type of the type checker.
   This is the output type of the disambiguation pass.
   The disambiguation pass has to :
   - resolve global/local/method classification for idents
*)

type location = {
   l_beg : Lexing.position;
   l_end : Lexing.position;
}
(** Location of an AST node,
    beginning and ending position of its corresponding source text. *);;

(** Types of various identifiers in the abstract syntax tree. *)
type file_name = string
     (** File name. *);;
type collection_name = string
     (** Collection name. *);;
type species_name = string
     (** Species name. *);;
type type_name = string
     (** Type name. *);;
type variable_name = string
     (** Variable name *);;
type label_name = string
     (** Label name. *);;
type constructor_name = string
     (** Constructor name. *);;
type node_label = int * string
     (** Node label in proof. *);;
type external_name = string
     (** External name from Ocaml or Coq. *);;

type 'a ast = {
   ast_loc : location; (** The location in the source of the AST node. *)
   ast_desc : 'a;      (** Description of the node. *)
};;

type ident = ident_desc ast
     (** Identifiers. *)
and ident_desc =
  | I_local of variable_name
  | I_global of file_name option * variable_name
  | I_method of collection_name option * variable_name
;;

type constructor = constructor_desc ast
     (** Constructors. *)
and constructor_desc =
  | C_global of file_name option * variable_name
;;

type rep_type_expr = rep_type_expr_desc ast
and rep_type_expr_desc =
  | RTE_ident of ident
  | RTE_fun of rep_type_expr * rep_type_expr
  | RTE_apply of ident * rep_type_expr list
  | RTE_prod of rep_type_expr * rep_type_expr
;;

type type_expr = type_expr_desc ast
and type_expr_desc =
  | TE_ident of ident
  | TE_fun of type_expr * type_expr
  | TE_apply of ident * type_expr list
  | TE_prod of type_expr * type_expr
  | TE_self
  | TE_prop
;;

(** Expressions. *)
type constant = constant_desc ast
and constant_desc =
  | C_int of string
  | C_bool of string
  | C_string of string
;;

type rec_flag = | RF_no_rec | RF_rec
;;

type pattern = pat_desc ast
and pat_desc =
  | P_constant of constant
  | P_tuple of pattern list
  | P_wild
  | P_var of variable_name
  | P_apply of ident * pattern list
  | P_constructor of constructor * pattern list
  | P_record of (label_name * pattern) list
;;

type expr = expr_desc ast
and expr_desc =
  | E_constant of constant
  | E_tuple of expr list
  | E_fun of variable_name list * expr
  | E_var of ident
  | E_apply of expr * expr list
  | E_constructor of constructor * expr list
  | E_record of (label_name * expr) list
  | E_match of expr * (pattern * expr) list
  | E_if of expr * expr * expr
  | E_let of rec_flag * (ident * expr) list * expr
;;

type species_def = species_def_desc ast
and species_def_desc = {
  sd_name : species_name;
  sd_params : (string * species_param_type) list;
  sd_inherits : species_expr list;
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
  | SP_coll of ident
  | SP_entity of expr

and species_field = species_field_desc ast
and species_field_desc =
  | SF_rep of rep_type_expr
  | SF_sig of ident * type_expr
  | SF_let of let_def
  | SF_letprop of let_def
  | SF_property of ident * prop
  | SF_theorem of theorem_def
  | SF_proof of ident * proof

and let_def = let_def_desc ast
and let_def_desc = {
  ld_rec : rec_flag;
  ld_bindings : binding list;
}
and binding = binding_desc ast
and binding_desc = {
  b_name : ident;
  b_params : (ident * type_expr option) list;
  b_type : type_expr option;
  b_body : expr;
}

and theorem_def = theorem_def_desc ast
and theorem_def_desc = {
  td_name : ident;
  td_stmt : prop;
  td_proof : proof;
}

and fact = fact_desc ast
and fact_desc =
  | F_def of ident
  | F_property of ident
  | F_node of node_label

and proof = proof_desc ast
and proof_desc =
  | Pf_assumed
  | Pf_auto of fact list
  | Pf_coq of string
  | Pf_node of proof_node list

and proof_node = proof_node_desc ast
and proof_node_desc =
  | PN_sub of node_label * statement * proof
  | PN_let of ident * expr
  | PN_qed of node_label * proof

and statement = statement_desc ast
and statement_desc = {
  s_hyps : hyp list;
  s_concl : prop option;
}

and hyp = hyp_desc ast
and hyp_desc =
  | H_var of string * type_expr
  | H_hyp of string * prop

and prop = prop_desc ast
and prop_desc =
  | P_forall of variable_name list * type_expr option * prop
  | P_exists of variable_name list * type_expr option * prop
  | P_imply of prop * prop
  | P_or of prop * prop
  | P_and of prop * prop
  | P_equiv of prop * prop
  | P_not of prop
  | P_expr of expr
;;

type coll_def = coll_def_desc ast
and coll_def_desc = {
  cd_name : collection_name;
  cd_body : species_expr;
};;

type type_def = type_def_desc ast
and type_def_desc = {
  td_name : type_name;
  td_params : string list;
  td_body : type_body;
}

and type_body = type_body_desc ast
and type_body_desc =
  | TD_alias of type_expr
  | TD_union of (constructor_name * type_expr) list 
  | TD_record of (label_name * type_expr) list
;;

type external_def = external_def_desc ast
and external_def_desc = {
  ed_name : external_name;
  ed_body : external_def_body;
}

and external_language =
  | EL_Caml
  | EL_Coq
  | EL_external of string

and external_def_body = external_def_body_desc ast
and external_def_body_desc =
  | ED_type of type_name * ((external_language * external_name) list)
  | ED_value of value_name * ((external_language * external_name) list)
;;

type phrase = phrase_desc ast
and phrase_desc =
  (* a voir: ajouter les expressions a toplevel ? *)
  | Ph_use of file_name
  | Ph_open of file_name
  | Ph_species of species_def
  | Ph_coll of coll_def
  | Ph_type of type_def
  | Ph_let of let_def
  | Ph_external of external_def
  | Ph_letprop of let_def
  | Ph_theorem of theorem_def
;;
