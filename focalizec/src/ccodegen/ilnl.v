Require Import external.
Require Import pcm.
Require Import List.

Module ILNL.

  Definition vname : Set := PCM.vname.
  
  Definition history : Set := PCM.history.

  Definition ttype : Set := PCM.ttype.
  
  Definition sig_field_info : Set := PCM.sig_field_info.
  
  Definition info : Set := PCM.info.

  Definition ident : Set := PCM.ident.

  Record binding (A : Set) : Set := mk_binding {
    b_info : info;
    b_name : vname;
    b_params : list ident;
    b_body : A
  }.

  Record let_def (A : Set) : Set := mk_let_def {
    ld_rec : bool;
    ld_local : bool;
    ld_bindings : list (binding A)
  }.
  Implicit Arguments mk_let_def [A].

  Definition external_expr : Set := list (prod string string).

  Definition constant : Set := PCM.constant.
  
  Definition pattern : Set := PCM.pattern.

  Inductive expr : Set :=
  | E_self : info -> expr
  | E_constant : info -> constant -> expr
  | E_fun : info -> list ident -> expr -> expr
  | E_var : info -> ident -> expr
  | E_app : info -> expr -> list expr -> expr
  | E_constr : info -> ident -> list expr -> expr
  | E_match : info -> expr -> list (prod pattern expr) -> expr
  | E_if : info -> expr -> expr -> expr -> expr
  | E_let : info -> let_def expr -> expr -> expr
  | E_record : info -> list (prod string expr) -> expr
  | E_record_access : info -> expr -> string -> expr
  | E_record_with : info ->  expr -> list (prod string expr) -> expr
  | E_tuple : info -> list expr -> expr
  | E_external : info -> external_expr -> expr
  | E_paren : info -> expr -> expr.

  Record let_field_info : Set := mk_let_field_info {
    lfi_info : info;
    lfi_from : history;
    lfi_name : vname;
    lfi_params : list ident;
    lfi_binding_body : expr
  }.

  Inductive species_field : Set :=
  | Sf_sig : sig_field_info -> species_field
  | Sf_let : let_field_info -> species_field
  | Sf_let_rec : list let_field_info -> species_field.

  Record collection_info : Set := mk_collection_info {
    coll_name : vname;
    coll_body : list species_field
  }.

  Definition open_info : Set := PCM.open_info.

  Definition use_info : Set := PCM.use_info.

  Definition type_info : Set := PCM.type_info.

  Definition let_def_info : Set := let_def expr.

  Inductive phrase : Set := 
  | Phrase_open : open_info -> phrase
  | Phrase_use : use_info -> phrase
  | Phrase_collection : collection_info -> phrase
  | Phrase_type : type_info -> phrase
  | Phrase_let_def : let_def_info -> phrase
  | Phrase_expr : expr -> phrase.

  Record file : Set := mk_file {
    file_name : string;
    file_body : list phrase
  }.

End ILNL.