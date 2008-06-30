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
  Parameter is_prop_equivalent : PCM.ident -> bool.

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
  Fixpoint sig_elim_prop (env : t) (t : PCM.ttype) {struct t} : PCM.ttype :=
    match t with
      | PCM.T_var v => t
      | PCM.T_arrow a b =>
        if is_in_prop env a
          then sig_elim_prop env b
          else PCM.T_arrow (sig_elim_prop env a) (sig_elim_prop env b)
      | PCM.T_tuple l =>
  (* because we already know it's not a full prop tuple *)
        PCM.T_tuple (list_elim_prop env l)
      | PCM.T_constr id args => (* idem *) t
      | PCM.T_self => t
      | PCM.T_species _ => t
    end

  with list_elim_prop (env : t) (l : list PCM.ttype) {struct l} 
    : list PCM.ttype :=
    match l with
      | nil => nil
      | h :: t =>
        if is_in_prop env h
          then list_elim_prop env t
          else h :: (list_elim_prop env t)
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

(*   Parameter expr_elim_prop : t -> PCM.ast PCM.expr -> ILNL.ast ILNL.expr. *)
  Fixpoint expr_elim_prop (env : t) (e : PCM.ast PCM.expr) {struct (PCM.desc e)}
    : ILNL.ast ILNL.expr :=
    match PCM.desc e with
      | PCM.E_self => e (* unspecified *)
      | PCM.E_constant c => e
      | PCM.E_fun l e' =>
        match let_params_check env l (PCM.type e') with
          | (l', t) => PCM.E_fun l' (PCM.mk_ast (expr_elim_prop env e') t)
        end
      | PCM.E_var id -> e
      | PCM.E_app e' l ->
        
    end.

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
