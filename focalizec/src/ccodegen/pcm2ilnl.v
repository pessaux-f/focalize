Require Import pcm.
Require Import ilnl.
Require Import List.
Require Export Bool.

Set Implicit Arguments.

Module PCM2ILNL.

  Record t : Set := mk_env {
    self_is_prop : bool
  }.
  
  (* checks if the ident is the built-in prop type or an alias of it. *)
  Parameter is_prop_equivalent : PCM.type_ident -> bool.

  (* Parameter is_in_prop : t -> PCM.ttype -> bool. *)
  Fixpoint is_in_prop (env : t) (t : PCM.ttype) {struct t} : bool :=
    match t with
      | PCM.T_var _ => false
      | PCM.T_arrow _ b => is_in_prop env b
      | PCM.T_tuple l => forallb (is_in_prop env) l
      | PCM.T_constr id _ => 
        (is_prop_equivalent id) || (self_is_prop env)
      | PCM.T_self => false
      | PCM.T_species _ => false
    end.

  (* Parameter sig_elim_prop : t -> PCM.ttype -> PCM.ttype. *)
  Fixpoint type_elim (env : t) (t : PCM.ttype) {struct t} : PCM.ttype :=
    match t with
      | PCM.T_var v => t
      | PCM.T_arrow a b =>
        if is_in_prop env a
          then type_elim env b
          else PCM.T_arrow (type_elim env a) (type_elim env b)
      | PCM.T_tuple l =>
  (* because we already know it's not a full prop tuple *)
        PCM.T_tuple (filter (fun x => negb (is_in_prop env x)) l)
      | PCM.T_constr id args => (* idem *) t
      | PCM.T_self => t
      | PCM.T_species _ => t
    end.


  (* Parameter let_params_check : t -> list PCM.vname -> PCM.ttype -> *)
(*     prod (list ILNL.vname) (ILNL.ttype). *)
  Fixpoint let_params_check (env : t) (l : list PCM.vname) (t : PCM.ttype)
    {struct t} : prod (list ILNL.vname) (ILNL.ttype) :=
    match t with
      | PCM.T_arrow a b =>
        if is_in_prop env a
          then 
            match l with
              | nil => (nil, (PCM.T_self)) (* unspecified *)
              | _ :: l' => let_params_check env l' b
            end
          else
            match l with
              | nil => (nil, (PCM.T_self)) (* unspecified *)
              | h :: l' =>
                match let_params_check env l' b with
                  | (l'', t') => ((h :: l''), (PCM.T_arrow a t'))
                end 
            end 
      | _ => (* can't be a prop *) (l, t)
    end.
  
  Parameter pattern_is_prop : PCM.pattern -> bool.

  Definition pattern_selector (p : PCM.pattern) : bool :=
    negb (pattern_is_prop p).

(*   Parameter pattern_elim_prop : t -> PCM.pattern -> PCM.pattern. *)
  Fixpoint pattern_elim (env : t) (p : PCM.pattern) {struct p} : PCM.pattern :=
    match p with
      | PCM.P_constr i id l =>
        (PCM.P_constr
          (PCM.mk_info (type_elim env (PCM.type i)))
          id
          (filter pattern_selector l))
      | PCM.P_record i l =>
        (PCM.P_record
          (PCM.mk_info (type_elim env (PCM.type i)))
          (filter 
            (fun x => match x with (_, r) => pattern_selector r end)
            l))
      | PCM.P_tuple i l =>
        (PCM.P_tuple
          (PCM.mk_info (type_elim env (PCM.type i)))
          (filter pattern_selector l))
      | _ => p
    end.
              

  Parameter external_expr_elim_prop :
    t -> PCM.external_expr -> ILNL.external_expr.

  Parameter binding_elim_prop : 
    PCM.ttype -> t -> PCM.binding PCM.expr -> ILNL.binding ILNL.expr.

  Parameter expr_is_prop : PCM.expr -> bool.

  Definition expr_selector (e : PCM.expr) : bool :=
    negb (expr_is_prop e).

(*   Parameter expr_elim_prop : t -> PCM.ast PCM.expr -> ILNL.ast ILNL.expr. *)
  (* at this stage, we made the hypothesis that the first expression isn't 
     a prop so we checks only composed expressions constructors *)
  Fixpoint expr_elim (env : t) (e : PCM.expr) {struct e} : ILNL.expr :=
    match e with
      | PCM.E_self i => 
        ILNL.E_self i
      
      | PCM.E_constant i c =>
        ILNL.E_constant i c

      | PCM.E_fun i l e' =>
        (ILNL.E_fun
          (PCM.mk_info (type_elim env (PCM.type i)))
          (filter (fun x => negb (is_in_prop env (PCM.type (PCM.i_info x)))) l)
          (expr_elim env e'))

      | PCM.E_var i id =>
        ILNL.E_var i id

      | PCM.E_app i e' l =>
        (ILNL.E_app 
          (PCM.mk_info (type_elim env (PCM.type i)))
          (expr_elim env e')
          (map (expr_elim env) (filter expr_selector l)))
        
      | PCM.E_constr i id l =>
        (ILNL.E_constr
          (PCM.mk_info (type_elim env (PCM.type i)))
          id
          (map (expr_elim env) (filter expr_selector l)))

      | PCM.E_match i e' l =>
        (ILNL.E_match
          (PCM.mk_info (type_elim env (PCM.type i)))
          (expr_elim env e')
          (map (fun x => match x with (l, r) => 
                           (pattern_elim env l, expr_elim env r) end) l))

      | PCM.E_if i a b c =>
        (ILNL.E_if
          (PCM.mk_info (type_elim env (PCM.type i)))
          (expr_elim env a)
          (expr_elim env b)
          (expr_elim env c))

      | PCM.E_let i ld e' =>
        if PCM.ld_logical ld
          then expr_elim env e'
          else
            (ILNL.E_let 
              (PCM.mk_info (type_elim env (PCM.type i)))
              (ILNL.mk_let_def 
                (PCM.ld_rec ld)
                (PCM.ld_local ld)
                (map (binding_elim env) (PCM.ld_bindings ld)))
              (expr_elim env e'))
    end

  with binding_elim (env : t) (b : PCM.binding PCM.expr) {struct b}
    : ILNL.binding ILNL.expr :=
    (ILNL.mk_binding
      (PCM.mk_info (type_elim (PCM.b_info b)))
      (PCM.b_name b)
      (filter ident_selector (PCM.b_params b))
      (expr_elim env (PCM.b_body b))).


(*       | PCM.E_var id => ILNL.E_var id *)
(*       | PCM.E_app e' l => *)
(*         (ILNL.E_app *)
(*           (ast_elim env expr_elim_prop e') *)
(*           (map *)
(*             (ast_elim env expr_elim_prop) *)
(*             (filter (fun x => negb (is_in_prop env (PCM.type x))) l))) *)
(*       | PCM.E_constr id l => *)
(*         (ILNL.E_constr *)
(*           id *)
(*           (map *)
(*             (ast_elim env expr_elim_prop) *)
(*             (filter (fun x => negb (is_in_prop env (PCM.type x))) l))) *)
(*       | PCM.E_match e' l => *)
(*         (ILNL.E_match *)
(*           (ast_elim env expr_elim_prop e') *)
(*           (map (fun a => *)
(*             match a with (l, r) => *)
(*               (ast_elim env pattern_elim_prop l, *)
(*                 ast_elim env expr_elim_prop r) *)
(*             end) l)) *)
(*       | PCM.E_if a b c => *)
(*         (ILNL.E_if  *)
(*           (ast_elim env expr_elim_prop a) *)
(*           (ast_elim env expr_elim_prop b) *)
(*           (ast_elim env expr_elim_prop c)) *)
(*       | PCM.E_let ld e' => *)
(*         if PCM.ld_logical (PCM.desc ld) *)
(*           then PCM.desc (ast_elim env expr_elim_prop e') *)
(*           else  *)
(*             (ILNL.E_let *)
(*               (ast_elim env let_def_elim_prop ld)  *)
(*               (ast_elim env expr_elim_prop e')) *)
(*       | PCM.E_record fields => *)
(*         (ILNL.E_record *)
(*           (map (* eliminate prop terms inside fields *) *)
(*             (fun x => *)
(*               match x with *)
(*                 (l, r) => (l, ast_elim env expr_elim_prop r) *)
(*               end) *)
(*             (filter (* eliminating prop fields *) *)
(*               (fun x => *)
(*                 match x with *)
(*                   (_, r) => negb (is_in_prop env (PCM.type r)) *)
(*                 end) *)
(*               fields))) *)
(*       | PCM.E_record_access e' str => *)
(*         ILNL.E_record_access (ast_elim env expr_elim_prop e') str *)
(*       | PCM.E_record_with e' l => *)
(*         (ILNL.E_record_with *)
(*           (ast_elim env expr_elim_prop e') *)
(*           (map (* eliminate prop terms inside fields *) *)
(*             (fun x => *)
(*               match x with *)
(*                 (l, r) => (l, ast_elim env expr_elim_prop r) *)
(*               end) *)
(*             (filter (* eliminating prop fields *) *)
(*               (fun x => *)
(*                 match x with *)
(*                   (_, r) => negb (is_in_prop env (PCM.type r)) *)
(*                 end) *)
(*               l))) *)
(*       | PCM.E_tuple l => *)
(*         (ILNL.E_tuple  *)
(*           (map *)
(*             (ast_elim env expr_elim_prop) *)
(*             (filter *)
(*               (fun x => negb (is_in_prop env (PCM.type x))) *)
(*               l))) *)
(*       | PCM.E_external e' => *)
(*         ILNL.E_external (ast_elim env external_expr_elim_prop e') *)
(*       | PCM.E_paren e' => *)
(*         ILNL.E_paren (ast_elim env expr_elim_prop e') *)
(*       | _ => ILNL.E_self *)
(*     end *)

(*   with let_def_elim_prop (env : t) (ld : PCM.let_def (PCM.ast PCM.expr)) *)
(*     {struct ld} : ILNL.let_def (ILNL.ast ILNL.expr) := *)
(*     (ILNL.mk_let_def *)
(*       (PCM.ld_rec ld) *)
(*       (PCM.ld_local ld) *)
(*       (map *)
(*         (fun (x : PCM.ast (PCM.binding (PCM.ast PCM.expr))) => *)
(*           ast_elim env (binding_elim_prop (PCM.type x)) x) *)
(*         (PCM.ld_bindings ld))). *)

(*   with binding_elim_prop (ty : PCM.ttype) (env : t) *)
(*     (b : PCM.binding (PCM.ast PCM.expr)) {struct b} *)
(*     : ILNL.binding (ILNL.ast ILNL.expr) := *)
(*     let params := map (PCM.bp_name b) in *)
(*       match let_params_check env params ty with *)
(*         (l, ty') => *)
(*         (ILNL.mk_binding  *)
(*           (ILNL.b_name b)  *)
(*           l *)
(*           ty' *)
(*           (ast_elim env expr_elim_type (PCM.b_body b))) *)
(*       end. *)
          

 (*  Parameter let_elim_prop : t -> PCM.let_field_info -> ILNL.let_field_info. *)
  Definition let_elim_prop (env : t) (ld : PCM.let_field_info)
    : ILNL.let_field_info :=
    match let_params_check env (PCM.lfi_params ld) (PCM.lfi_type ld) with
      | (l, t) =>
        match PCM.desc (PCM.lfi_binding_body ld) with
          | PCM.Bb_logical _ =>
           (* non specified since the let shoud not be in prop *)
            (ILNL.mk_let_field_info
              (PCM.lfi_from ld)
              (PCM.lfi_name ld)
              l
              t
              (PCM.mk_ast PCM.E_self PCM.T_self))
          | PCM.Bb_expr e => 
            (ILNL.mk_let_field_info
              (PCM.lfi_from ld)
              (PCM.lfi_name ld)
              l
              t
              (expr_elim_prop env e))
        end
    end.
            
 
  (** Checks if the given signature field is the rep specifier. *)
  Parameter is_rep : PCM.sig_field_info -> bool.

 (*  Parameter let_rec_elim_prop :  *)
(*     t -> list PCM.let_field_info -> list ILNL.let_field_info. *)
  Fixpoint let_rec_elim_prop (env : t) (l : list PCM.let_field_info)
    {struct l} : list ILNL.let_field_info :=
    match l with
      | nil => nil
      | h :: l' =>
        if is_in_prop env (PCM.lfi_type h)
          then let_rec_elim_prop env l'
          else (let_elim_prop env h) :: (let_rec_elim_prop env l')
    end.

  Fixpoint species_field (env : t) (l : list PCM.species_field) {struct l}
    : list ILNL.species_field :=
    match l with
      | nil => nil
      | h :: l' =>
        match h with
          | PCM.Sf_sig x => 
            if is_in_prop env (PCM.sfi_type x)
              then
                if is_rep x
                  then species_field (mk_env true) l'
                  else species_field env l'
              else 
                (ILNL.Sf_sig 
                  (PCM.mk_sig_field_info 
                    (PCM.sfi_from x)
                    (PCM.sfi_name x)
                    (sig_elim_prop env (PCM.sfi_type x)))) ::
                (species_field env l')
          | PCM.Sf_let x => 
            if is_in_prop env (PCM.lfi_type x)
              then species_field env l'
              else 
                (ILNL.Sf_let (let_elim_prop env x)) :: (species_field env l')
          | PCM.Sf_let_rec x =>
            (ILNL.Sf_let_rec (let_rec_elim_prop env x)) 
            :: (species_field env l')
          | PCM.Sf_theorem _ => species_field env l'
          | PCM.Sf_property _ => species_field env l'
        end
    end.

  Parameter let_def_elim_prop : PCM.let_def_info -> ILNL.let_def_info.

  Fixpoint phrases (env : t) (l : list PCM.phrase) {struct l}
    : list ILNL.phrase :=
    match l with
      | nil => nil
      | (PCM.Phrase_open x) :: l' =>
        (ILNL.Phrase_open x) :: (phrases env l') 
      | (PCM.Phrase_use x) :: l' =>
        (ILNL.Phrase_use x) :: (phrases env l')
      | (PCM.Phrase_species _) :: l' =>
        phrases env l'
      | (PCM.Phrase_collection x) :: l' =>
        (ILNL.Phrase_collection 
          (ILNL.mk_collection_info
            (PCM.coll_name x)
            (species_field env (PCM.coll_body x))))
        :: (phrases env l')
      | (PCM.Phrase_type x) :: l' =>
        (ILNL.Phrase_type x) :: (phrases env l')
      | (PCM.Phrase_let_def x) :: l' =>
        (ILNL.Phrase_let_def (let_def_elim_prop x)) :: (phrases env l')
      | (PCM.Phrase_theorem _) :: l' => phrases env l'
      | (PCM.Phrase_expr x) :: l' =>
        (ILNL.Phrase_expr x) :: (phrases env l')
    end.

  Definition translate (x : t) (s : PCM.file) : ILNL.file :=
    ILNL.mk_file (PCM.file_name s) (phrases x (PCM.file_body s)).

End PCM2ILNL.
