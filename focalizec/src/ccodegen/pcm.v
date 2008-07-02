Require Export List.
Require Export external.

Module PCM.

  Record type_ident : Set := {
    ci_file : option string;
    ci_name : string
  }.

  Inductive ttype : Set :=
  | T_var : string -> ttype
  | T_arrow : ttype -> ttype -> ttype
  | T_tuple : list ttype -> ttype
  | T_constr : type_ident -> list ttype -> ttype
  | T_self : ttype
  | T_species : type_ident -> ttype.

  Record info : Set := mk_info {
    type : ttype
  }.

  Record ident : Set := mk_ident {
    i_info : info;
    i_file : option string;
    i_coll : option string;
    i_name : string
  }.

  Inductive constant : Set :=
  | C_int : info -> string -> constant
  | C_float : info -> string -> constant
  | C_bool : info -> string -> constant
  | C_string : info -> string -> constant
  | C_char : info -> char -> constant.

  Parameter vname : Set.

  Inductive pattern : Set :=
  | P_const : info -> constant -> pattern
  | P_var : info -> vname -> pattern
  | P_as : info -> pattern -> vname -> pattern
  | P_wild : info -> pattern
  | P_constr : info -> ident -> list pattern -> pattern
  | P_record : info -> list (prod string pattern) -> pattern
  | P_tuple : info -> list pattern -> pattern
  | P_paren : info -> pattern -> pattern.

  Record binding (A : Set) : Set := mk_binding {
    b_info : info;
    b_name : vname;
    b_params : list ident;
    b_body : A
  }.

  Record let_def (A : Set) : Set := mk_let_def {
    ld_rec : bool;
    ld_logical : bool;
    ld_local : bool;
    ld_bindings : list (binding A)
  }.
  Implicit Arguments mk_let_def [A].
  Implicit Arguments ld_logical [A].
  Implicit Arguments ld_rec [A].
  Implicit Arguments ld_local [A].
  Implicit Arguments ld_bindings [A].

  Definition external_expr : Set := list (prod string string).

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

  Record history : Set := mk_history {
    h_initial : ident;
    h_inherited : list expr
  }.

  Record sig_field_info : Set := mk_sig_field_info {
    sfi_info : info;
    sfi_from : history;
    sfi_name : vname
  }.

  Parameter logical_expr : Set.

  Inductive binding_body : Set :=
  | Bb_logical : logical_expr -> binding_body
  | Bb_expr : expr -> binding_body.

  Record let_field_info : Set := mk_let_field_info {
    lfi_info : info;
    lfi_from : history;
    lfi_name : vname;
    lfi_params : list ident;
    lfi_binding_body : binding_body
  }.

  Parameter theorem_field_info : Set.

  Parameter property_field_info : Set.

  Inductive species_field : Set :=
  | Sf_sig : sig_field_info -> species_field
  | Sf_let : let_field_info -> species_field
  | Sf_let_rec : list let_field_info -> species_field
  | Sf_theorem : theorem_field_info -> species_field
  | Sf_property : property_field_info -> species_field.

  Record collection_info : Set := mk_collection_info {
    coll_name : vname;
    coll_body : list species_field
  }.

  Record open_info : Set := mk_open_info {
    oi_name : string
  }.
  
  Record use_info : Set := mk_use_info {
    ui_name : string
  }.

  Parameter species_info : Set.

  Inductive type_expr : Set :=
  | Te_ident : ident -> type_expr
  | Te_fun : type_expr -> type_expr -> type_expr
  | Te_app : ident -> list type_expr -> type_expr
  | Te_prod : list type_expr -> type_expr
  | Te_self : type_expr
  | Te_prop : type_expr
  | Te_paren : type_expr -> type_expr.

  Inductive simple_type_def_body : Set :=
  | Stdb_alias : type_expr -> simple_type_def_body
  | Stdb_union : list (prod vname (list type_expr)) -> simple_type_def_body
  | Stdb_record : list (prod string type_expr) -> simple_type_def_body.

  Record external_type_def_body : Set := mk_external_type_def_body {
    etdb_internal : option simple_type_def_body;
    etdb_external : external_expr;
    etdb_bindings : list (prod vname external_expr)
  }.

  Inductive type_def_body : Set :=
  | Tdb_simple : simple_type_def_body -> type_def_body
  | Tdb_external : external_type_def_body -> type_def_body.

  Record type_def : Set := mk_type_info {
    ti_name : vname;
    ti_params : list vname;
    ti_body : type_def_body
  }.
  
  Definition type_info : Set := type_def.

  Definition let_def_info : Set := let_def binding_body.

  Parameter theorem_info : Set.

  Inductive phrase : Set := 
  | Phrase_open : open_info -> phrase
  | Phrase_use : use_info -> phrase
  | Phrase_species : species_info -> phrase
  | Phrase_collection : collection_info -> phrase
  | Phrase_type : type_info -> phrase
  | Phrase_let_def : let_def_info -> phrase
  | Phrase_theorem : theorem_info -> phrase
  | Phrase_expr : expr -> phrase.

  Record file : Set := mk_file {
    file_name : string;
    file_body : list phrase
  }.

End PCM.





