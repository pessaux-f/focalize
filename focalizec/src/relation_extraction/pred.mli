(******************)
(* Specifications *)
(******************)

(* Identifiers for everything *)
type ident = string

(* Pre-extraction environment *)
type 'a info_dic = (ident * 'a) list


(* Constructors' list of an inductive type *)
type s_ind_cstr_list =
  | CLList of ident list
  | CLNone

(* Inductive types *)
type s_ind_env = (ident * s_ind_cstr_list) list

(* Mode of an inductive predicate *)
type s_mode_option =
  | MInput
  | MOutput
  | MSkip
type s_mode = s_mode_option list

(* Predicate signature *)
type s_pred = {
  pred_name : ident; 
  pred_mode : s_mode;
  pred_fun_name : ident;
}

(* Term in specification *)
type s_term =
  | TVar of ident
  | TTuple of s_term list
  | TRecord of ident list * s_term list
  | TConstr of ident * s_term list * s_ind_cstr_list
  | TConst of ident
  | TFun of ident * s_term list
  | TFunNot of ident * s_term list

(* Predicate term: p(...) *)
type s_pred_term = {
  pdt_pred : s_pred;
  pdt_args : s_term list;
  pdt_not_flag : bool;
}

(* Premisse term: either function or predicate *)
type s_prem_term =
  | PMTFun of s_term
  | PMTPred of s_pred_term

(* Premisse *)
type s_prem =
  | PMTerm of s_prem_term
  | PMOr of s_prem list
  | PMChoice of s_prem list
  | PMAnd of s_prem list

(* Property (or constructor of an inductive predicate) *)
type s_prop = {
  p_forall : ident list;
  p_prems : s_prem list;
  p_concl : s_pred_term;
}


(* Specification of an inductive predicate *)
type ('p, 'c, 'r, 'co, 'f) s_pred_spec = {
  ps_props : s_prop list;
  ps_pred_info : 'p info_dic;
  ps_constr_info : 'c info_dic;
  ps_record_info : 'r info_dic;
  ps_const_info : 'co info_dic;
  ps_fun_info : 'f info_dic;
  ps_ind_env : s_ind_env;
  ps_pred : s_pred;
}
(* 'p is Libnames.global_reference in Coq *)
(* 'c is Term.constr in Coq *)

(*********)
(* Trees *)
(*********)

type pred_tree

(******************)
(* Target langage *)
(******************)

type l_term =
  | LVar of ident
  | LTuple of l_term list
  | LRecord of ident list * l_term list
  | LConstr of ident * l_term list
  | LConst of ident
  | LFun of ident * l_term list
  | LFunNot of ident * l_term list
  | LMatch of l_term * (l_pat * l_term) list
  | LLin of (ident * ident) list
  | LTrue | LFalse | LDefault

and l_pat =
  | LPVar of ident
  | LPTuple of l_pat list
  | LPRecord of ident list * l_pat list
  | LPConstr of ident * l_pat list
  | LPConst of ident
  | LPWild
  | LPTrue | LPFalse

type l_fun = ident * ident list * l_term

(************)
(* Printers *)
(************)

val pp_s_pred_spec : ('p, 'c, 'r, 'co, 'f) s_pred_spec -> string
val pp_tree : pred_tree -> string
val pp_l_fun : l_fun -> string

(**************)
(* Algorithms *)
(**************)

val tree_from_spec_with_quick_try : bool -> ('p, 'c, 'r, 'co, 'f) s_pred_spec -> pred_tree

val code_from_tree : ('p, 'c, 'r, 'co, 'f) s_pred_spec -> pred_tree -> l_fun

