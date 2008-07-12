Require Export ast.
Require Export pcm.
Require Import ilnl.
Require Import List.
Require Export Bool.

Set Implicit Arguments.
Open Local Scope error_monad_scope.

Module PCM2ILNL.
 
(*   Inductive result (A : Set) : Set :=  *)
(*   | Ok : A -> result A *)
(*   | Error : result A. *)


(*   Record env : Set := mk_env { *)
(*     self_is_prop : bool *)
(*   }. *)

(*   Parameter is_rep : pcm.sig_field_info -> bool. *)
(*   Parameter is_in_prop : env -> pcm.type -> bool. *)
(*   Parameter is_ident_prop : string -> bool. *)


(* (*   Parameter type_ident : env -> pcm.type_ident -> ilnl.type_ident. *) *)
(*   Definition type_ident (e : env) (id : pcm.type_ident) : ilnl.type_ident := *)
(*     (ilnl.mk_type_ident *)
(*       (pcm.ti_file id) *)
(*       (pcm.ti_name id)). *)


(* (*   Parameter type : env -> pcm.type -> ilnl.type. *) *)
(*   Fixpoint type (e : env) (t : pcm.type) {struct t} : ilnl.type := *)
(*     match t with *)
(*       | pcm.T_var v => ilnl.T_var v *)
(*       | pcm.T_arrow a b => *)
(*         if is_in_prop e a *)
(*           then type e b *)
(*           else ilnl.T_arrow (type e a) (type e b) *)
(*       | pcm.T_tuple l => *)
(*   (* because we already know it's not a full prop tuple *) *)
(*         ilnl.T_tuple (list_type e l) *)
(*       | pcm.T_constr id args => (* idem *) *)
(*         ilnl.T_constr (type_ident e id) (list_type e args) *)
(*       | pcm.T_self => ilnl.T_self *)
(*       | pcm.T_species id => ilnl.T_species (type_ident e id) *)
(*     end *)

(*   with list_type (e : env) (l : pcm.list_type) {struct l} : ilnl.list_type := *)
(*     match l with *)
(*       | pcm.lt_nil => ilnl.lt_nil *)
(*       | pcm.lt_cons h l' =>  *)
(*         if is_in_prop e h *)
(*           then list_type e l' *)
(*           else ilnl.lt_cons (type e h) (list_type e l') *)
(*     end. *)

(*  (*  Parameter constant : env -> pcm.constant -> ilnl.constant. *) *)
(*   Definition constant (e : env) (c : pcm.constant) : ilnl.constant := *)
(*     match c with *)
(*       | pcm.Constant (pcm.C_int s) i => ilnl.Constant (ilnl.C_int s) i *)
(*       | pcm.Constant (pcm.C_float s) i => ilnl.Constant (ilnl.C_float s) i *)
(*       | pcm.Constant (pcm.C_bool s) i => ilnl.Constant (ilnl.C_bool s) i *)
(*       | pcm.Constant (pcm.C_string s) i => ilnl.Constant (ilnl.C_string s) i *)
(*       | pcm.Constant (pcm.C_char c) i => ilnl.Constant (ilnl.C_char c) i *)
(*     end. *)
        

(* (*   Parameter ident : env -> pcm.ident -> ilnl.ident. *) *)
(*   Definition ident (e : env) (id : pcm.ident) : ilnl.ident := *)
(*     (ilnl.mk_ident *)
(*       (pcm.i_info id) *)
(*       (type e (pcm.i_type id)) *)
(*       (pcm.i_file id) *)
(*       (pcm.i_coll id) *)
(*       (pcm.i_name id)). *)

(* (*   Parameter list_ident : env -> list pcm.ident -> list ilnl.ident. *) *)
(*   Fixpoint list_ident (e : env) (l : list pcm.ident) {struct l}  *)
(*     : list ilnl.ident := *)
(*     match l with *)
(*       | nil => nil *)
(*       | h :: t => *)
(*         if is_in_prop e (pcm.i_type h) *)
(*           then list_ident e t *)
(*           else (ident e h) :: (list_ident e t) *)
(*     end. *)

(*   Fixpoint type_expr_is_in_prop (e : env) (t : pcm.type_expr) {struct t} *)
(*     : bool := *)
(*     match t with *)
(*       | pcm.Type_expr pcm.Te_prop _ => true *)
(*       | pcm.Type_expr (pcm.Te_fun _ b) _ => type_expr_is_in_prop e b *)
(*       | pcm.Type_expr (pcm.Te_paren ty) _ => type_expr_is_in_prop e ty *)
(*       | _ => false *)
(*     end. *)

(*   Fixpoint type_expr (e : env) (t : pcm.type_expr) {struct t} *)
(*     : ilnl.type_expr := *)
(*     match t with *)
(*       | pcm.Type_expr (pcm.Te_ident id) i => *)
(*         ilnl.Type_expr (ilnl.Te_ident (ident e id)) i *)
(*       | pcm.Type_expr (pcm.Te_fun a b) i =>  *)
(*         if type_expr_is_in_prop e b  *)
(*           then type_expr e a *)
(*           else ilnl.Type_expr (ilnl.Te_fun (type_expr e a) (type_expr e b)) i *)
(*       | pcm.Type_expr (pcm.Te_app id args) i => *)
(*         ilnl.Type_expr (ilnl.Te_app (ident e id) (list_type_expr e args)) i *)
(*       | pcm.Type_expr (pcm.Te_prod l) i => *)
(*         ilnl.Type_expr (ilnl.Te_prod (list_type_expr e l)) i *)
(*       | pcm.Type_expr pcm.Te_self i => *)
(*         ilnl.Type_expr ilnl.Te_self i *)
(*       | pcm.Type_expr pcm.Te_prop i =>  *)
(*         (* should not happen *) *)
(*         ilnl.Type_expr ilnl.Te_self i *)
(*       | pcm.Type_expr (pcm.Te_paren a) i => *)
(*         ilnl.Type_expr (ilnl.Te_paren (type_expr e a)) i *)
(*     end *)
    
(*   with list_type_expr (e : env) (l : pcm.list_type_expr) {struct l} *)
(*     : ilnl.list_type_expr := *)
(*     match l with *)
(*       | pcm.lte_nil => ilnl.lte_nil *)
(*       | pcm.lte_cons h t => ilnl.lte_cons (type_expr e h) (list_type_expr e t) *)
(*     end. *)

(*   Fixpoint list_type_constr (e : env) (l : list (prod pcm.ident pcm.list_type_expr)) *)
(*     {struct l} : list (prod ilnl.ident ilnl.list_type_expr) := *)
(*     match l with *)
(*       | nil => nil *)
(*       | (a, b) :: l' => *)
(*         (ident e a, list_type_expr e b) :: (list_type_constr e l') *)
(*     end. *)

(*   Fixpoint list_type_field (e : env) (l : list (prod string pcm.type_expr)) *)
(*     {struct l} : list (prod string ilnl.type_expr) := *)
(*     match l with *)
(*       | nil => nil *)
(*       | (a, b) :: l' => *)
(*         (a, type_expr e b) :: (list_type_field e l') *)
(*     end. *)

(*   Definition simple_type_def_body (e : env) (t : pcm.simple_type_def_body) *)
(*     : ilnl.simple_type_def_body := *)
(*     match t with *)
(*       | pcm.Simple_type_def_body (pcm.Stdb_alias te) i => *)
(*         ilnl.Simple_type_def_body (ilnl.Stdb_alias (type_expr e te)) i *)
(*       | pcm.Simple_type_def_body (pcm.Stdb_union l) i => *)
(*         ilnl.Simple_type_def_body (ilnl.Stdb_union (list_type_constr e l)) i *)
(*       | pcm.Simple_type_def_body (pcm.Stdb_record l) i => *)
(*         ilnl.Simple_type_def_body (ilnl.Stdb_record (list_type_field e l)) i *)
(*     end. *)

(*   Definition external_expr (e : env) (ee : pcm.external_expr)  *)
(*     : ilnl.external_expr := *)
(*     (ilnl.mk_external_expr *)
(*       (pcm.ee_lang ee) *)
(*       (pcm.ee_code ee)). *)

(*   Definition external_type_def_body (e : env) (et : pcm.external_type_def_body) *)
(*     : ilnl.external_type_def_body := *)
(*     (ilnl.mk_external_type_def_body *)
(*       (pcm.etdb_info et) *)
(*       (match pcm.etdb_internal et with *)
(*          | None => None *)
(*          | Some ty => Some (simple_type_def_body e ty) *)
(*        end) *)
(*       (external_expr e (pcm.etdb_external et)) *)
(*       (map *)
(*         (fun x => match x with (a, b) => (ident e a, external_expr e b) end) *)
(*         (pcm.etdb_bindings et))). *)

(*   Definition type_def_body (e : env) (t : pcm.type_def_body)  *)
(*     : ilnl.type_def_body := *)
(*     match t with *)
(*       | pcm.Type_def_body (pcm.Tdb_simple b) i => *)
(*         ilnl.Type_def_body (ilnl.Tdb_simple (simple_type_def_body e b)) i *)
(*       | pcm.Type_def_body (pcm.Tdb_external ext) i => *)
(*         ilnl.Type_def_body (ilnl.Tdb_external (external_type_def_body e ext)) i *)
(*     end. *)

(* (*   Parameter type_def : env -> pcm.type_def -> ilnl.type_def. *) *)
(*   Definition type_def (e : env) (td : pcm.type_def) : ilnl.type_def := *)
(*     (ilnl.mk_type_def *)
(*       (pcm.td_info td) *)
(*       (ident e (pcm.td_name td)) *)
(*       (list_ident e (pcm.td_params td)) *)
(*       (type_def_body e (pcm.td_body td))). *)


(*   Fixpoint pattern (e : env) (p : pcm.pattern) {struct p} : ilnl.pattern := *)
(*     match p with *)
(*       | pcm.Pattern (pcm.P_const c) i t => *)
(*         ilnl.Pattern (ilnl.P_const (constant e c)) i (type e t) *)

(*       | pcm.Pattern (pcm.P_var id) i t => *)
(*         ilnl.Pattern (ilnl.P_var (ident e id)) i (type e t) *)

(*       | pcm.Pattern (pcm.P_as p' id) i t => *)
(*         ilnl.Pattern (ilnl.P_as (pattern e p') (ident e id)) i (type e t) *)

(*       | pcm.Pattern pcm.P_wild i t => *)
(*         ilnl.Pattern ilnl.P_wild i (type e t) *)

(*       | pcm.Pattern (pcm.P_constr id l) i t => *)
(*         (ilnl.Pattern *)
(*           (ilnl.P_constr (ident e id) (list_pattern e l)) i (type e t)) *)

(*       | pcm.Pattern (pcm.P_record l) i t => *)
(*         (ilnl.Pattern (ilnl.P_record (list_field_pattern e l)) i (type e t)) *)

(*       | pcm.Pattern (pcm.P_tuple l) i t => *)
(*         (ilnl.Pattern (ilnl.P_tuple (list_pattern e l)) i (type e t)) *)

(*       | pcm.Pattern (pcm.P_paren p') i t => *)
(*         ilnl.Pattern (ilnl.P_paren (pattern e p')) i (type e t) *)
(*     end *)

(*   with list_pattern (e : env) (l : pcm.list_pattern) {struct l} *)
(*     : ilnl.list_pattern := *)
(*     match l with *)
(*       | pcm.p_nil => ilnl.p_nil *)
(*       | pcm.p_cons p l' => *)
(*         ilnl.p_cons (pattern e p) (list_pattern e l') *)
(*     end *)

(*   with list_field_pattern (e : env) (l : pcm.list_field_pattern) {struct l} *)
(*     : ilnl.list_field_pattern := *)
(*     match l with *)
(*       | pcm.pf_nil => ilnl.pf_nil *)
(*       | pcm.pf_cons str p l' => *)
(*         ilnl.pf_cons str (pattern e p) (list_field_pattern e l') *)
(*     end. *)

(* (*   Parameter expr : env -> pcm.expr -> ilnl.expr. *) *)
(*   Fixpoint expr (e : env) (exp : pcm.expr) {struct exp} : ilnl.expr := *)
(*     match exp with *)
(*       | pcm.Expr pcm.E_self i t => *)
(*         ilnl.Expr ilnl.E_self i (type e t) *)

(*       | pcm.Expr (pcm.E_constant c) i t => *)
(*         ilnl.Expr (ilnl.E_constant (constant e c)) i (type e t) *)

(*       | pcm.Expr (pcm.E_fun args body) i t => *)
(*         ilnl.Expr (ilnl.E_fun (list_ident e args) (expr e body)) i (type e t) *)

(*       | pcm.Expr (pcm.E_var id) i t => *)
(*         ilnl.Expr (ilnl.E_var (ident e id)) i (type e t) *)

(*       | pcm.Expr (pcm.E_app f args) i t => *)
(*         (ilnl.Expr  *)
(*           (ilnl.E_app (expr e f) (list_expr e args)) *)
(*           i (type e t)) *)

(*       | pcm.Expr (pcm.E_constr id args) i t => *)
(*         (ilnl.Expr *)
(*           (ilnl.E_constr (ident e id) (list_expr e args)) *)
(*           i (type e t)) *)

(*       | pcm.Expr (pcm.E_match exp l) i t => *)
(*         (ilnl.Expr *)
(*           (ilnl.E_match (expr e exp) (list_clause e l)) *)
(*           i (type e t)) *)

(*       | pcm.Expr (pcm.E_if a b c) i t => *)
(*         (ilnl.Expr *)
(*           (ilnl.E_if (expr e a) (expr e b) (expr e c)) *)
(*           i (type e t)) *)
      
(*       | pcm.Expr (pcm.E_let le a) i t => *)
(*         (ilnl.Expr *)
(*           (ilnl.E_let (let_expr e le) (expr e a)) *)
(*           i (type e t)) *)

(*       | pcm.Expr (pcm.E_record l) i t => *)
(*         (ilnl.Expr *)
(*           (ilnl.E_record (list_field e l)) *)
(*           i (type e t)) *)

(*       | pcm.Expr (pcm.E_record_access exp str) i t => *)
(*         (ilnl.Expr (ilnl.E_record_access (expr e exp) str) i (type e t)) *)

(*       | pcm.Expr (pcm.E_record_with exp l) i t => *)
(*         (ilnl.Expr *)
(*           (ilnl.E_record_with (expr e exp) (list_field e l)) i (type e t)) *)

(*       | pcm.Expr (pcm.E_tuple l) i t => *)
(*         (ilnl.Expr (ilnl.E_tuple (list_expr e l)) i (type e t)) *)

(*       | pcm.Expr (pcm.E_external ee) i t => *)
(*         (ilnl.Expr (ilnl.E_external (external_expr e ee)) i (type e t)) *)

(*       | pcm.Expr (pcm.E_paren ee) i t => *)
(*         (ilnl.Expr (ilnl.E_paren (expr e ee)) i (type e t)) *)
(*     end *)

(*   with list_expr (e : env) (l : pcm.list_expr) {struct l} *)
(*     : ilnl.list_expr := *)
(*     match l with *)
(*       | pcm.le_nil => ilnl.le_nil *)
(*       | pcm.le_cons h t => *)
(*         if is_in_prop e (pcm.typeof h) *)
(*           then list_expr e t *)
(*           else ilnl.le_cons (expr e h) (list_expr e t) *)
(*     end *)
    
(*   with list_clause (e : env) (l : pcm.list_clause) {struct l} *)
(*     : ilnl.list_clause := *)
(*     match l with *)
(*       | pcm.lc_nil => ilnl.lc_nil *)
(*       | pcm.lc_cons p exp l' => *)
(*         ilnl.lc_cons (pattern e p) (expr e exp) (list_clause e l') *)
(*     end *)

(*   with let_expr (e : env) (le : pcm.let_expr) {struct le} : ilnl.let_expr := *)
(*     match le with *)
(*       pcm.Let_expr rec loc binds i => *)
(*       ilnl.Let_expr rec loc (list_binding e binds) i *)
(*     end *)

(*   with list_binding (e : env) (l : pcm.list_binding) {struct l}  *)
(*     : ilnl.list_binding := *)
(*     match l with *)
(*       | pcm.b_nil => ilnl.b_nil *)
(*       | pcm.b_cons v li body i l'=>  *)
(*         (ilnl.b_cons *)
(*           (ident e v) *)
(*           (list_ident e li) *)
(*           (expr e body) i *)
(*           (list_binding e l')) *)
(*     end *)

(*   with list_field (e : env) (l : pcm.list_field) {struct l} : ilnl.list_field := *)
(*     match l with *)
(*       | pcm.lf_nil => ilnl.lf_nil *)
(*       | pcm.lf_cons str exp l' => *)
(*         ilnl.lf_cons str (expr e exp) (list_field e l') *)
(*     end. *)

(* (*   Definition history (e : env) (h : history) : history := h. *) *)
  
(*   Fixpoint list_let_field (env : env) (l : list pcm.let_field_info) {struct l} *)
(*     : list ilnl.let_field_info := *)
(*     match l with *)
(*       | nil => nil *)
(*       | x :: l' => *)
(*         match pcm.lfi_binding_body x with *)
(*           | pcm.Bb_logical _ => list_let_field env l' *)
(*           | pcm.Bb_expr exp => *)
(*             (ilnl.mk_let_field_info *)
(*               (pcm.lfi_info x) *)
(*               (pcm.lfi_from x) *)
(*               (ident env (pcm.lfi_name x)) *)
(*               (list_ident env (pcm.lfi_params x)) *)
(*               (expr env exp)) :: (list_let_field env l') *)
(*         end *)
(*     end. *)

(*   Fixpoint species_field (env : env) (l : list pcm.species_field) {struct l} *)
(*     : list ilnl.species_field := *)
(*     match l with *)
(*       | nil => nil *)

(*       | (pcm.Sf_sig x) :: l' => *)
(*         if is_in_prop env (pcm.sfi_type x) *)
(*           then *)
(*             if is_rep x *)
(*               then species_field (mk_env true) l' *)
(*               else species_field env l' *)
(*           else *)
(*             (ilnl.Sf_sig *)
(*               (ilnl.mk_sig_field_info *)
(*                 (pcm.sfi_info x) *)
(*                 (pcm.sfi_from x) *)
(*                 (ident env (pcm.sfi_name x)) *)
(*                 (type env (pcm.sfi_type x)))) *)
(*             :: (species_field env l') *)

(*       | (pcm.Sf_let x) :: l' => *)
(*         match list_let_field env (x::nil) with *)
(*           | nil => species_field env l' *)
(*           | h :: t => (ilnl.Sf_let h) :: (species_field env l') *)
(*         end *)

(*       | (pcm.Sf_let_rec x) :: l' => *)
(*         (ilnl.Sf_let_rec (list_let_field env x)) :: (species_field env l') *)

(*       | (pcm.Sf_theorem _) :: l' => species_field env l' *)

(*       | (pcm.Sf_property _) :: l' => species_field env l' *)
(*     end. *)

(*   Definition collection (e : env) (col : pcm.collection)  *)
(*     : ilnl.collection := *)
(*     (ilnl.mk_collection  *)
(*       (ident e (pcm.coll_name col)) *)
(*       (species_field e (pcm.coll_body col))). *)
  
(*   Fixpoint bindings (e : env) (l : list pcm.binding) {struct l} *)
(*     : list ilnl.binding := *)
(*     match l with *)
(*       | nil => nil *)
(*       | b :: l' => *)
(*         match pcm.b_body b with *)
(*           | pcm.Bb_logical _ => bindings e l' *)
(*           | pcm.Bb_expr exp => *)
(*             (ilnl.mk_binding  *)
(*               (pcm.b_info b) *)
(*               (ident e (pcm.b_name b)) *)
(*               (list_ident e (pcm.b_params b)) *)
(*               (expr e exp)) :: (bindings e l') *)
(*         end *)
(*     end. *)
        

(*  (*  Parameter let_def : env -> pcm.let_def -> ilnl.let_def. *) *)
(*   Definition let_def (e : env) (ld : pcm.let_def) : ilnl.let_def := *)
(*     (ilnl.mk_let_def *)
(*       (pcm.ld_info ld) *)
(*       (pcm.ld_rec ld) *)
(*       (pcm.ld_local ld) *)
(*       (bindings e (pcm.ld_bindings ld))). *)

(*   Fixpoint phrases (e : env) (l : list pcm.phrase) {struct l}  *)
(*     : list ilnl.phrase := *)
(*     match l with *)
(*       | nil => nil *)
        
(*       | (pcm.Phrase (pcm.Ph_open str) i) :: l' => *)
(*         (ilnl.Phrase (ilnl.Ph_open str) i) :: (phrases e l') *)
      
(*       | (pcm.Phrase (pcm.Ph_use str) i) :: l' => *)
(*         (ilnl.Phrase (ilnl.Ph_use str) i) :: (phrases e l') *)

(*       | (pcm.Phrase (pcm.Ph_species _) _) :: l' => *)
(*         (phrases e l') *)

(*       | (pcm.Phrase (pcm.Ph_collection c) i) :: l' => *)
(*         (ilnl.Phrase (ilnl.Ph_collection (collection e c)) i) :: (phrases e l') *)
        
(*       | (pcm.Phrase (pcm.Ph_type t) i) :: l' => *)
(*         match pcm.td_body t with *)
(*           | pcm.Type_def_body (pcm.Tdb_simple (pcm.Simple_type_def_body (pcm.Stdb_alias te) _)) _ => *)
(*             if type_expr_is_in_prop e te *)
(*               then phrases e l' *)
(*               else  *)
(*                 (ilnl.Phrase (ilnl.Ph_type (type_def e t)) i) :: (phrases e l') *)
(*           | pcm.Type_def_body (pcm.Tdb_external et) _ => *)
(*             match pcm.etdb_internal et with *)
(*               | None => *)
(*                 (ilnl.Phrase (ilnl.Ph_type (type_def e t)) i) :: (phrases e l') *)
(*               | Some (pcm.Simple_type_def_body (pcm.Stdb_alias te) _) => *)
(*                 if type_expr_is_in_prop e te *)
(*                   then phrases e l' *)
(*                   else  *)
(*                     (ilnl.Phrase (ilnl.Ph_type (type_def e t)) i) :: (phrases e l') *)
(*               | _ => *)
(*                 (ilnl.Phrase (ilnl.Ph_type (type_def e t)) i) :: (phrases e l') *)
(*             end *)
(*           | _ => *)
(*             (ilnl.Phrase (ilnl.Ph_type (type_def e t)) i) :: (phrases e l') *)
(*         end *)
(*       | (pcm.Phrase (pcm.Ph_let_def ld) i) :: l' => *)
(*         if pcm.ld_logical ld *)
(*           then phrases e l' *)
(*           else  *)
(*             (ilnl.Phrase (ilnl.Ph_let_def (let_def e ld)) i) :: (phrases e l') *)

(*       | (pcm.Phrase (pcm.Ph_theorem _) _) :: l' => *)
(*         (phrases e l') *)

(*       | (pcm.Phrase (pcm.Ph_expr exp) i) :: l' => *)
(*         if is_in_prop e (pcm.typeof exp) *)
(*           then phrases e l' *)
(*           else  *)
(*             (ilnl.Phrase (ilnl.Ph_expr (expr e exp)) i) :: (phrases e l') *)
(*     end. *)

(*   Definition translate_ (e : env) (f : pcm.file) : ilnl.file := *)
(*     ilnl.mk_file (pcm.file_name f) (phrases e (pcm.file_body f)). *)

  Parameter env : Set.

  Parameter error_unknow_expr : string.

  Require Import Recdef.
  Require Import Arith.

  Parameter expr2nat : pcm_expr -> nat.

  Function expr (e : env) (x : pcm_expr) {measure expr2nat x} : res ilnl_expr :=
    if pcm_expr_is_self x
      then
        OK (ilnl_mk_expr
          (mk_ast ilnl_expr_self tt (pcm_expr_location x)))
      
      else if pcm_expr_is_app x
        then
          do left <- expr e (pcm_expr_app_expr x);
          do args <- mmap (expr e) (pcm_expr_app_args x);
            OK (ilnl_mk_expr (mk_ast
              (ilnl_expr_app left args)
              tt
              (pcm_expr_location x)))

        else
          Error error_unknow_expr.
        
      

  Fixpoint phrases (e : env) (l : list pcm_phrase) {struct l} 
    : res (list ilnl_phrase) :=
    match l with
      | nil => OK nil
      | h :: t =>
        if pcm_phrase_is_open h then
          do l' <- phrases e t;
            OK ((ilnl_mk_phrase
              (mk_ast
                (ilnl_phrase_open (pcm_phrase_open_name h))
                tt
                (pcm_phrase_location h))) :: l' )
          
          else if pcm_phrase_is_expr h then
            do e' <- expr e (pcm_phrase_expr_value h);
            do l' <- phrases e t;
              OK ((ilnl_mk_phrase
                (mk_ast
                  (ilnl_phrase_expr e')
                  tt
                  (pcm_phrase_location h))) :: l')
              
            else
              phrases e t
    end.
            

  Definition translate (e : env) (f : pcm_file) : res ilnl_file :=
    do l' <- phrases e (pcm_file_body f);
      OK (ilnl_mk_file (pcm_file_name f) l').

End PCM2ILNL.
