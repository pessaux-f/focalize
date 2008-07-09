Require Import pcm.
Require Import ilnl.
Require Import List.
Require Export Bool.

Set Implicit Arguments.

Module PCM2ILNL.
 
  Inductive result (A : Set) : Set := 
  | Ok : A -> result A
  | Error : result A.


  Record env : Set := mk_env {
    self_is_prop : bool
  }.

  Parameter is_rep : PCM.sig_field_info -> bool.
  Parameter is_in_prop : env -> PCM.type -> bool.
  Parameter is_ident_prop : string -> bool.


(*   Parameter type_ident : env -> PCM.type_ident -> ILNL.type_ident. *)
  Definition type_ident (e : env) (id : PCM.type_ident) : ILNL.type_ident :=
    (ILNL.mk_type_ident
      (PCM.ti_file id)
      (PCM.ti_name id)).


(*   Parameter type : env -> PCM.type -> ILNL.type. *)
  Fixpoint type (e : env) (t : PCM.type) {struct t} : ILNL.type :=
    match t with
      | PCM.T_var v => ILNL.T_var v
      | PCM.T_arrow a b =>
        if is_in_prop e a
          then type e b
          else ILNL.T_arrow (type e a) (type e b)
      | PCM.T_tuple l =>
  (* because we already know it's not a full prop tuple *)
        ILNL.T_tuple (list_type e l)
      | PCM.T_constr id args => (* idem *)
        ILNL.T_constr (type_ident e id) (list_type e args)
      | PCM.T_self => ILNL.T_self
      | PCM.T_species id => ILNL.T_species (type_ident e id)
    end

  with list_type (e : env) (l : PCM.list_type) {struct l} : ILNL.list_type :=
    match l with
      | PCM.lt_nil => ILNL.lt_nil
      | PCM.lt_cons h l' => 
        if is_in_prop e h
          then list_type e l'
          else ILNL.lt_cons (type e h) (list_type e l')
    end.

 (*  Parameter constant : env -> PCM.constant -> ILNL.constant. *)
  Definition constant (e : env) (c : PCM.constant) : ILNL.constant :=
    match c with
      | PCM.Constant (PCM.C_int s) i => ILNL.Constant (ILNL.C_int s) i
      | PCM.Constant (PCM.C_float s) i => ILNL.Constant (ILNL.C_float s) i
      | PCM.Constant (PCM.C_bool s) i => ILNL.Constant (ILNL.C_bool s) i
      | PCM.Constant (PCM.C_string s) i => ILNL.Constant (ILNL.C_string s) i
      | PCM.Constant (PCM.C_char c) i => ILNL.Constant (ILNL.C_char c) i
    end.
        

(*   Parameter ident : env -> PCM.ident -> ILNL.ident. *)
  Definition ident (e : env) (id : PCM.ident) : ILNL.ident :=
    (ILNL.mk_ident
      (PCM.i_info id)
      (type e (PCM.i_type id))
      (PCM.i_file id)
      (PCM.i_coll id)
      (PCM.i_name id)).

(*   Parameter list_ident : env -> list PCM.ident -> list ILNL.ident. *)
  Fixpoint list_ident (e : env) (l : list PCM.ident) {struct l} 
    : list ILNL.ident :=
    match l with
      | nil => nil
      | h :: t =>
        if is_in_prop e (PCM.i_type h)
          then list_ident e t
          else (ident e h) :: (list_ident e t)
    end.

  Fixpoint type_expr_is_in_prop (e : env) (t : PCM.type_expr) {struct t}
    : bool :=
    match t with
      | PCM.Type_expr PCM.Te_prop _ => true
      | PCM.Type_expr (PCM.Te_fun _ b) _ => type_expr_is_in_prop e b
      | PCM.Type_expr (PCM.Te_paren ty) _ => type_expr_is_in_prop e ty
      | _ => false
    end.

  Fixpoint type_expr (e : env) (t : PCM.type_expr) {struct t}
    : ILNL.type_expr :=
    match t with
      | PCM.Type_expr (PCM.Te_ident id) i =>
        ILNL.Type_expr (ILNL.Te_ident (ident e id)) i
      | PCM.Type_expr (PCM.Te_fun a b) i => 
        if type_expr_is_in_prop e b 
          then type_expr e a
          else ILNL.Type_expr (ILNL.Te_fun (type_expr e a) (type_expr e b)) i
      | PCM.Type_expr (PCM.Te_app id args) i =>
        ILNL.Type_expr (ILNL.Te_app (ident e id) (list_type_expr e args)) i
      | PCM.Type_expr (PCM.Te_prod l) i =>
        ILNL.Type_expr (ILNL.Te_prod (list_type_expr e l)) i
      | PCM.Type_expr PCM.Te_self i =>
        ILNL.Type_expr ILNL.Te_self i
      | PCM.Type_expr PCM.Te_prop i => 
        (* should not happen *)
        ILNL.Type_expr ILNL.Te_self i
      | PCM.Type_expr (PCM.Te_paren a) i =>
        ILNL.Type_expr (ILNL.Te_paren (type_expr e a)) i
    end
    
  with list_type_expr (e : env) (l : PCM.list_type_expr) {struct l}
    : ILNL.list_type_expr :=
    match l with
      | PCM.lte_nil => ILNL.lte_nil
      | PCM.lte_cons h t => ILNL.lte_cons (type_expr e h) (list_type_expr e t)
    end.

  Fixpoint list_type_constr (e : env) (l : list (prod PCM.ident PCM.list_type_expr))
    {struct l} : list (prod ILNL.ident ILNL.list_type_expr) :=
    match l with
      | nil => nil
      | (a, b) :: l' =>
        (ident e a, list_type_expr e b) :: (list_type_constr e l')
    end.

  Fixpoint list_type_field (e : env) (l : list (prod string PCM.type_expr))
    {struct l} : list (prod string ILNL.type_expr) :=
    match l with
      | nil => nil
      | (a, b) :: l' =>
        (a, type_expr e b) :: (list_type_field e l')
    end.

  Definition simple_type_def_body (e : env) (t : PCM.simple_type_def_body)
    : ILNL.simple_type_def_body :=
    match t with
      | PCM.Simple_type_def_body (PCM.Stdb_alias te) i =>
        ILNL.Simple_type_def_body (ILNL.Stdb_alias (type_expr e te)) i
      | PCM.Simple_type_def_body (PCM.Stdb_union l) i =>
        ILNL.Simple_type_def_body (ILNL.Stdb_union (list_type_constr e l)) i
      | PCM.Simple_type_def_body (PCM.Stdb_record l) i =>
        ILNL.Simple_type_def_body (ILNL.Stdb_record (list_type_field e l)) i
    end.

  Definition external_expr (e : env) (ee : PCM.external_expr) 
    : ILNL.external_expr :=
    (ILNL.mk_external_expr
      (PCM.ee_lang ee)
      (PCM.ee_code ee)).

  Definition external_type_def_body (e : env) (et : PCM.external_type_def_body)
    : ILNL.external_type_def_body :=
    (ILNL.mk_external_type_def_body
      (PCM.etdb_info et)
      (match PCM.etdb_internal et with
         | None => None
         | Some ty => Some (simple_type_def_body e ty)
       end)
      (external_expr e (PCM.etdb_external et))
      (map
        (fun x => match x with (a, b) => (ident e a, external_expr e b) end)
        (PCM.etdb_bindings et))).

  Definition type_def_body (e : env) (t : PCM.type_def_body) 
    : ILNL.type_def_body :=
    match t with
      | PCM.Type_def_body (PCM.Tdb_simple b) i =>
        ILNL.Type_def_body (ILNL.Tdb_simple (simple_type_def_body e b)) i
      | PCM.Type_def_body (PCM.Tdb_external ext) i =>
        ILNL.Type_def_body (ILNL.Tdb_external (external_type_def_body e ext)) i
    end.

(*   Parameter type_def : env -> PCM.type_def -> ILNL.type_def. *)
  Definition type_def (e : env) (td : PCM.type_def) : ILNL.type_def :=
    (ILNL.mk_type_def
      (PCM.td_info td)
      (ident e (PCM.td_name td))
      (list_ident e (PCM.td_params td))
      (type_def_body e (PCM.td_body td))).


  Fixpoint pattern (e : env) (p : PCM.pattern) {struct p} : ILNL.pattern :=
    match p with
      | PCM.Pattern (PCM.P_const c) i t =>
        ILNL.Pattern (ILNL.P_const (constant e c)) i (type e t)

      | PCM.Pattern (PCM.P_var id) i t =>
        ILNL.Pattern (ILNL.P_var (ident e id)) i (type e t)

      | PCM.Pattern (PCM.P_as p' id) i t =>
        ILNL.Pattern (ILNL.P_as (pattern e p') (ident e id)) i (type e t)

      | PCM.Pattern PCM.P_wild i t =>
        ILNL.Pattern ILNL.P_wild i (type e t)

      | PCM.Pattern (PCM.P_constr id l) i t =>
        (ILNL.Pattern
          (ILNL.P_constr (ident e id) (list_pattern e l)) i (type e t))

      | PCM.Pattern (PCM.P_record l) i t =>
        (ILNL.Pattern (ILNL.P_record (list_field_pattern e l)) i (type e t))

      | PCM.Pattern (PCM.P_tuple l) i t =>
        (ILNL.Pattern (ILNL.P_tuple (list_pattern e l)) i (type e t))

      | PCM.Pattern (PCM.P_paren p') i t =>
        ILNL.Pattern (ILNL.P_paren (pattern e p')) i (type e t)
    end

  with list_pattern (e : env) (l : PCM.list_pattern) {struct l}
    : ILNL.list_pattern :=
    match l with
      | PCM.p_nil => ILNL.p_nil
      | PCM.p_cons p l' =>
        ILNL.p_cons (pattern e p) (list_pattern e l')
    end

  with list_field_pattern (e : env) (l : PCM.list_field_pattern) {struct l}
    : ILNL.list_field_pattern :=
    match l with
      | PCM.pf_nil => ILNL.pf_nil
      | PCM.pf_cons str p l' =>
        ILNL.pf_cons str (pattern e p) (list_field_pattern e l')
    end.

(*   Parameter expr : env -> PCM.expr -> ILNL.expr. *)
  Fixpoint expr (e : env) (exp : PCM.expr) {struct exp} : ILNL.expr :=
    match exp with
      | PCM.Expr PCM.E_self i t =>
        ILNL.Expr ILNL.E_self i (type e t)

      | PCM.Expr (PCM.E_constant c) i t =>
        ILNL.Expr (ILNL.E_constant (constant e c)) i (type e t)

      | PCM.Expr (PCM.E_fun args body) i t =>
        ILNL.Expr (ILNL.E_fun (list_ident e args) (expr e body)) i (type e t)

      | PCM.Expr (PCM.E_var id) i t =>
        ILNL.Expr (ILNL.E_var (ident e id)) i (type e t)

      | PCM.Expr (PCM.E_app f args) i t =>
        (ILNL.Expr 
          (ILNL.E_app (expr e f) (list_expr e args))
          i (type e t))

      | PCM.Expr (PCM.E_constr id args) i t =>
        (ILNL.Expr
          (ILNL.E_constr (ident e id) (list_expr e args))
          i (type e t))

      | PCM.Expr (PCM.E_match exp l) i t =>
        (ILNL.Expr
          (ILNL.E_match (expr e exp) (list_clause e l))
          i (type e t))

      | PCM.Expr (PCM.E_if a b c) i t =>
        (ILNL.Expr
          (ILNL.E_if (expr e a) (expr e b) (expr e c))
          i (type e t))
      
      | PCM.Expr (PCM.E_let le a) i t =>
        (ILNL.Expr
          (ILNL.E_let (let_expr e le) (expr e a))
          i (type e t))

      | PCM.Expr (PCM.E_record l) i t =>
        (ILNL.Expr
          (ILNL.E_record (list_field e l))
          i (type e t))

      | PCM.Expr (PCM.E_record_access exp str) i t =>
        (ILNL.Expr (ILNL.E_record_access (expr e exp) str) i (type e t))

      | PCM.Expr (PCM.E_record_with exp l) i t =>
        (ILNL.Expr
          (ILNL.E_record_with (expr e exp) (list_field e l)) i (type e t))

      | PCM.Expr (PCM.E_tuple l) i t =>
        (ILNL.Expr (ILNL.E_tuple (list_expr e l)) i (type e t))

      | PCM.Expr (PCM.E_external ee) i t =>
        (ILNL.Expr (ILNL.E_external (external_expr e ee)) i (type e t))

      | PCM.Expr (PCM.E_paren ee) i t =>
        (ILNL.Expr (ILNL.E_paren (expr e ee)) i (type e t))
    end

  with list_expr (e : env) (l : PCM.list_expr) {struct l}
    : ILNL.list_expr :=
    match l with
      | PCM.le_nil => ILNL.le_nil
      | PCM.le_cons h t =>
        if is_in_prop e (PCM.typeof h)
          then list_expr e t
          else ILNL.le_cons (expr e h) (list_expr e t)
    end
    
  with list_clause (e : env) (l : PCM.list_clause) {struct l}
    : ILNL.list_clause :=
    match l with
      | PCM.lc_nil => ILNL.lc_nil
      | PCM.lc_cons p exp l' =>
        ILNL.lc_cons (pattern e p) (expr e exp) (list_clause e l')
    end

  with let_expr (e : env) (le : PCM.let_expr) {struct le} : ILNL.let_expr :=
    match le with
      PCM.Let_expr rec loc binds i =>
      ILNL.Let_expr rec loc (list_binding e binds) i
    end

  with list_binding (e : env) (l : PCM.list_binding) {struct l} 
    : ILNL.list_binding :=
    match l with
      | PCM.b_nil => ILNL.b_nil
      | PCM.b_cons v li body i l'=> 
        (ILNL.b_cons
          (ident e v)
          (list_ident e li)
          (expr e body) i
          (list_binding e l'))
    end

  with list_field (e : env) (l : PCM.list_field) {struct l} : ILNL.list_field :=
    match l with
      | PCM.lf_nil => ILNL.lf_nil
      | PCM.lf_cons str exp l' =>
        ILNL.lf_cons str (expr e exp) (list_field e l')
    end.

  Definition history (e : env) (h : PCM.history) : ILNL.history :=
    (ILNL.mk_history 
      (ident e (PCM.h_initial h)) 
      (list_expr e (PCM.h_inherited h))).
  
  Fixpoint list_let_field (env : env) (l : list PCM.let_field_info) {struct l}
    : list ILNL.let_field_info :=
    match l with
      | nil => nil
      | x :: l' =>
        match PCM.lfi_binding_body x with
          | PCM.Bb_logical _ => list_let_field env l'
          | PCM.Bb_expr exp =>
            (ILNL.mk_let_field_info
              (PCM.lfi_info x)
              (history env (PCM.lfi_from x))
              (ident env (PCM.lfi_name x))
              (list_ident env (PCM.lfi_params x))
              (expr env exp)) :: (list_let_field env l')
        end
    end.

  Fixpoint species_field (env : env) (l : list PCM.species_field) {struct l}
    : list ILNL.species_field :=
    match l with
      | nil => nil

      | (PCM.Sf_sig x) :: l' =>
        if is_in_prop env (PCM.sfi_type x)
          then
            if is_rep x
              then species_field (mk_env true) l'
              else species_field env l'
          else
            (ILNL.Sf_sig
              (ILNL.mk_sig_field_info
                (PCM.sfi_info x)
                (history env (PCM.sfi_from x))
                (ident env (PCM.sfi_name x))
                (type env (PCM.sfi_type x))))
            :: (species_field env l')

      | (PCM.Sf_let x) :: l' =>
        match list_let_field env (x::nil) with
          | nil => species_field env l'
          | h :: t => (ILNL.Sf_let h) :: (species_field env l')
        end

      | (PCM.Sf_let_rec x) :: l' =>
        (ILNL.Sf_let_rec (list_let_field env x)) :: (species_field env l')

      | (PCM.Sf_theorem _) :: l' => species_field env l'

      | (PCM.Sf_property _) :: l' => species_field env l'
    end.

  Definition collection (e : env) (col : PCM.collection) 
    : ILNL.collection :=
    (ILNL.mk_collection 
      (ident e (PCM.coll_name col))
      (species_field e (PCM.coll_body col))).
  
  Fixpoint bindings (e : env) (l : list PCM.binding) {struct l}
    : list ILNL.binding :=
    match l with
      | nil => nil
      | b :: l' =>
        match PCM.b_body b with
          | PCM.Bb_logical _ => bindings e l'
          | PCM.Bb_expr exp =>
            (ILNL.mk_binding 
              (PCM.b_info b)
              (ident e (PCM.b_name b))
              (list_ident e (PCM.b_params b))
              (expr e exp)) :: (bindings e l')
        end
    end.
        

 (*  Parameter let_def : env -> PCM.let_def -> ILNL.let_def. *)
  Definition let_def (e : env) (ld : PCM.let_def) : ILNL.let_def :=
    (ILNL.mk_let_def
      (PCM.ld_info ld)
      (PCM.ld_rec ld)
      (PCM.ld_local ld)
      (bindings e (PCM.ld_bindings ld))).

  Fixpoint phrases (e : env) (l : list PCM.phrase) {struct l} 
    : list ILNL.phrase :=
    match l with
      | nil => nil
        
      | (PCM.Phrase (PCM.Ph_open str) i) :: l' =>
        (ILNL.Phrase (ILNL.Ph_open str) i) :: (phrases e l')
      
      | (PCM.Phrase (PCM.Ph_use str) i) :: l' =>
        (ILNL.Phrase (ILNL.Ph_use str) i) :: (phrases e l')

      | (PCM.Phrase (PCM.Ph_species _) _) :: l' =>
        (phrases e l')

      | (PCM.Phrase (PCM.Ph_collection c) i) :: l' =>
        (ILNL.Phrase (ILNL.Ph_collection (collection e c)) i) :: (phrases e l')
        
      | (PCM.Phrase (PCM.Ph_type t) i) :: l' =>
        match PCM.td_body t with
          | PCM.Type_def_body (PCM.Tdb_simple (PCM.Simple_type_def_body (PCM.Stdb_alias te) _)) _ =>
            if type_expr_is_in_prop e te
              then phrases e l'
              else 
                (ILNL.Phrase (ILNL.Ph_type (type_def e t)) i) :: (phrases e l')
          | PCM.Type_def_body (PCM.Tdb_external et) _ =>
            match PCM.etdb_internal et with
              | None =>
                (ILNL.Phrase (ILNL.Ph_type (type_def e t)) i) :: (phrases e l')
              | Some (PCM.Simple_type_def_body (PCM.Stdb_alias te) _) =>
                if type_expr_is_in_prop e te
                  then phrases e l'
                  else 
                    (ILNL.Phrase (ILNL.Ph_type (type_def e t)) i) :: (phrases e l')
              | _ =>
                (ILNL.Phrase (ILNL.Ph_type (type_def e t)) i) :: (phrases e l')
            end
          | _ =>
            (ILNL.Phrase (ILNL.Ph_type (type_def e t)) i) :: (phrases e l')
        end
      | (PCM.Phrase (PCM.Ph_let_def ld) i) :: l' =>
        if PCM.ld_logical ld
          then phrases e l'
          else 
            (ILNL.Phrase (ILNL.Ph_let_def (let_def e ld)) i) :: (phrases e l')

      | (PCM.Phrase (PCM.Ph_theorem _) _) :: l' =>
        (phrases e l')

      | (PCM.Phrase (PCM.Ph_expr exp) i) :: l' =>
        if is_in_prop e (PCM.typeof exp)
          then phrases e l'
          else 
            (ILNL.Phrase (ILNL.Ph_expr (expr e exp)) i) :: (phrases e l')
    end.

  Definition translate (e : env) (f : PCM.file) : ILNL.file :=
    ILNL.mk_file (PCM.file_name f) (phrases e (PCM.file_body f)).


End PCM2ILNL.
