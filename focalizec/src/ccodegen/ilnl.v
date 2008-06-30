Require Import external.
Require Import pcm.
Require Import List.

Module ILNL.

  Definition vname : Set := PCM.vname.
  
  Definition history : Set := PCM.history.

  Definition ttype : Set := PCM.ttype.
  
  Definition sig_field_info : Set := PCM.sig_field_info.
  
  Definition ast : Set -> Set := PCM.ast.

  Definition expr : Set := PCM.expr.

  Record let_field_info : Set := mk_let_field_info {
    lfi_from : history;
    lfi_name : vname;
    lfi_params : list vname;
    lfi_type : ttype;
    lfi_binding_body : ast expr
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

  Definition let_def : Set -> Set := PCM.let_def.

  Definition let_def_info : Set := ast (let_def (ast expr)).

  Inductive phrase : Set := 
  | Phrase_open : open_info -> phrase
  | Phrase_use : use_info -> phrase
  | Phrase_collection : collection_info -> phrase
  | Phrase_type : type_info -> phrase
  | Phrase_let_def : let_def_info -> phrase
  | Phrase_expr : ast expr -> phrase.

  Record file : Set := mk_file {
    file_name : string;
    file_body : list phrase
  }.
End ILNL.