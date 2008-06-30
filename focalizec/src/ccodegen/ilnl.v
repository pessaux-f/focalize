Require Import external.
Require Import pcm.
Require Import List.

Module ILNL.

  Definition vname : Set := PCM.vname.
  
  Definition history : Set := PCM.history.

  Definition ttype : Set := PCM.ttype.
  
  Definition sig_field_info : Set := PCM.sig_field_info.
  
  Definition ast : Set -> Set := PCM.ast.

  Record binding (A : Set) : Set := mk_binding {
    b_name : vname;
    b_params : list vname;
    b_type : ttype;
    b_body : A
  }.

  Record let_def (A : Set) : Set := mk_let_def {
    ld_rec : bool;
    ld_local : bool;
    ld_bindings : list (ast (binding A))
  }.

  Definition external_expr : Set := list (prod string string).

  Definition constant : Set := PCM.constant.
  
  Definition ident : Set := PCM.ident.

  Definition pattern : Set := PCM.pattern.

  Inductive expr : Set :=
  | E_self : expr
  | E_constant : ast constant -> expr
  | E_fun : list vname -> ast expr -> expr
  | E_var : ast ident -> expr
  | E_app : ast expr -> list (ast expr) -> expr
  | E_constr : ast ident -> list (ast expr) -> expr
  | E_match : ast expr -> list (prod (ast pattern) (ast expr)) -> expr
  | E_if : ast expr -> ast expr -> ast expr -> expr
  | E_let : ast (let_def (ast expr)) -> ast expr -> expr
  | E_record : list (prod string (ast expr)) -> expr
  | E_record_access : ast expr -> string -> expr
  | E_record_with : ast expr -> list (prod string (ast expr)) -> expr
  | E_tuple : list (ast expr) -> expr
  | E_external : ast external_expr -> expr
  | E_paren : ast expr -> expr.

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