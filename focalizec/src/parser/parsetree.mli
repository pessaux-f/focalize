(* The parse tree, or shallow abstract syntax.
   Disambiguation has not been done.
   This is the input type of the disambiguation pass.
   The disambiguation pass has to :
   - resolve global/local/method classification for idents
*)
type position = Lexing.position = {
  pos_fname : string ;
  pos_lnum : int ;
  pos_bol : int ;
  pos_cnum : int
} ;;

type location = {
 l_beg : position ;
 l_end : position
}
(** Location of an AST node,
    beginning and ending position of its corresponding source text. *);;

(** Types of various identifiers in the abstract syntax tree. *)
type fname = string
     (** File name. *) ;;
type cname = string
     (** Collection name. *) ;;
type sname = string
     (** Species name. *) ;;
type tname = string
     (** Type name. *) ;;
type vname =
   | Vlident of string  (* Lowercase ident. *)
   | Vuident of string  (* Capitalized ident. *)
   | Vpident of string  (* Prefix operator ident. *)
   | Viident of string  (* Infix operator ident. *)
   | Vqident of string
     (** Variable names are classified with respect of their lexical class,
       which can be regular. infix or prefix. *);;
type label_name = string
     (** Label name. *) ;;
type constr_name = vname
     (** Constructor name. *) ;;
type node_label = int * string
     (** Node label in proof. *) ;;
type external_name = string * string
     (** External name from Ocaml or Coq: module name and identifier. *);;

type ('a, 'b) generic_ast = {
   ast_loc : location ; (** The location in the source of the AST node. *)
   ast_desc : 'a ;      (** The description of the node. *)
   ast_doc : 'b option  (** The support for documentation in many formats. *)
};;

type 'a ast = ('a, string) generic_ast;;
type 'a ast_doc = ('a, string) generic_ast;;

type ident = ident_desc ast
and ident_desc =
  | I_local of vname
  | I_global of fname option * vname
  | I_method of cname option * vname (* If vname is self, then the real name  *)
	                             (* should be considered as only [cname]. *)
                                     (* If [cname] is None and [vname] is     *)
                                     (* self, then it's bugged !              *)
;;

type rep_type_def = rep_type_def_desc ast_doc
and rep_type_def_desc =
  | RTE_ident of ident
  | RTE_fun of rep_type_def * rep_type_def
  | RTE_app of ident * rep_type_def list
  | RTE_prod of rep_type_def * rep_type_def
  | RTE_paren of rep_type_def
;;

type type_expr = type_expr_desc ast
and type_expr_desc =
  | TE_ident of ident
  | TE_fun of type_expr * type_expr
  | TE_app of ident * type_expr list
  | TE_prod of type_expr * type_expr
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

type log_flag = | LF_no_log | LF_log
;;

type loc_flag = | LF_no_loc | LF_loc
;;

type pattern = pat_desc ast
and pat_desc =
  | P_const of constant
  | P_var of vname
  | P_as of pattern * vname
  | P_wild
  | P_app of ident * pattern list
  | P_record of (label_name * pattern) list
  | P_tuple of pattern list
  | P_paren of pattern
;;

type external_language =
  | EL_Caml
  | EL_Coq
  | EL_external of string
;;

type external_def = external_def_desc ast
and external_def_desc =
  | ED_type of external_def_body
  | ED_value of external_def_body

and external_def_body = external_def_body_desc ast
and external_def_body_desc = {
  ed_name : vname ;
  ed_body : external_expr
}

and external_expr = external_expr_desc ast
and external_expr_desc =
    (external_language * external_expression) list

and external_expression = string ;;

type species_def = species_def_desc ast_doc
and species_def_desc = {
  sd_name : sname ;
  sd_params : (vname * species_param_type) list ;
  sd_inherits : (species_expr list) ast_doc ;
  sd_fields : species_field list
}

and species_param_type = species_param_type_desc ast
and species_param_type_desc =
  | SPT_in of ident
  | SPT_is of species_expr

and species_expr = species_expr_desc ast
and species_expr_desc = {
  se_name : ident ;
  se_params : species_param list
}

and species_param = species_param_desc ast
and species_param_desc =
  | SP of expr

and sig_def = sig_def_desc ast_doc
and sig_def_desc = {
  sig_name : ident ;
  sig_type: type_expr
}

and proof_def = proof_def_desc ast_doc
and proof_def_desc = {
  pd_name : ident;
  pd_proof: proof
}

and property_def = property_def_desc ast_doc
and property_def_desc = {
  prd_name : ident ;
  prd_prop: prop
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
  ld_rec : rec_flag ;
  ld_log : log_flag ;
  ld_loc : loc_flag ;
  ld_bindings : binding list
}
and binding = binding_desc ast
and binding_desc = {
  b_name : ident ;
  b_params : (ident * type_expr option) list ;
  b_type : type_expr option ;
  b_body : expr
}

and theorem_def = theorem_def_desc ast_doc
and theorem_def_desc = {
  th_name : ident ;
  th_loc : loc_flag ;
  th_stmt : prop ;
  th_proof : proof
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
  s_hyps : hyp list ;
  s_concl : prop option
}

and hyp = hyp_desc ast
and hyp_desc =
  | H_var of vname * type_expr
  | H_hyp of vname * prop
  | H_not of vname * expr

and prop = prop_desc ast
and prop_desc =
  | Pr_forall of vname list * type_expr option * prop
  | Pr_exists of vname list * type_expr option * prop
  | Pr_imply of prop * prop
  | Pr_or of prop * prop
  | Pr_and of prop * prop
  | Pr_equiv of prop * prop
  | Pr_not of prop
  | Pr_expr of expr
  | Pr_paren of prop

and expr = expr_desc ast
and expr_desc =
  | E_const of constant
  | E_fun of vname list * expr
  | E_var of ident
  | E_app of expr * expr list
  | E_constr of expr * expr list
  | E_match of expr * (pattern * expr) list
  | E_if of expr * expr * expr
  | E_let of let_def * expr
  | E_record of (label_name * expr) list
  | E_record_access of expr * label_name
  | E_record_with of expr * (label_name * expr) list
  | E_tuple of expr list
  | E_external of external_expr
  | E_paren of expr
;;

type coll_def = coll_def_desc ast_doc
and coll_def_desc = {
  cd_name : cname ;
  cd_body : species_expr
} ;;

type type_def = type_def_desc ast
and type_def_desc = {
  td_name : tname ;
  td_params : string list ;
  td_body : type_body
}

and type_body = type_body_desc ast
and type_body_desc =
  | TD_alias of type_expr
  | TD_union of (constr_name * (type_expr list)) list 
  | TD_record of (label_name * type_expr) list
;;

(** Toplevel expressions. *)
type expr_def = expr ;;

type phrase = phrase_desc ast
and phrase_desc =
  | Ph_external of external_def
  | Ph_use of fname
  | Ph_open of fname
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
