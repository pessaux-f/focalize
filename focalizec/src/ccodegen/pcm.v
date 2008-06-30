Require Export List.
Require Export external.

Module PCM.

  Record ident : Set := mk_ident {
    i_file : option string;
    i_species : option string;
    i_name : string
  }.

  Inductive ttype : Set :=
  | T_var : string -> ttype
  | T_arrow : ttype -> ttype -> ttype
  | T_tuple : list ttype -> ttype
  | T_constr : ident -> list ttype -> ttype
  | T_self : ttype
  | T_species : ident -> ttype.

  Record ast (A : Set) : Set := mk_ast {
    desc : A;
    type : ttype
  }.
  Implicit Arguments mk_ast [A].
  Implicit Arguments desc [A].
  Implicit Arguments type [A].

  Inductive constant : Set :=
  | C_int : string -> constant
  | C_float : string -> constant
  | C_bool : string -> constant
  | C_string : string -> constant
  | C_char : char -> constant.

  Parameter vname : Set.

  Inductive pattern : Set :=
  | P_const : ast constant -> pattern
  | P_var : vname -> pattern
  | P_as : ast pattern -> vname -> pattern
  | P_wild : pattern
  | P_constr : ast ident -> list (ast pattern) -> pattern
  | P_record : list (prod string (ast pattern)) -> pattern
  | P_tuple : list (ast pattern) -> pattern
  | P_paren : ast pattern -> pattern.



  Record binding_param : Set := mk_binding_param {
    bp_name : vname;
    bp_type : option ttype
  }.

  Record binding (A : Set) : Set := mk_binding {
    b_name : vname;
    b_params : list binding_param;
    b_type : option ttype;
    b_body : A
  }.

  Record let_def (A : Set) : Set := mk_let_def {
    ld_rec : bool;
    ld_logical : bool;
    ld_local : bool;
    ld_bindings : list (ast (binding A))
  }.
  Implicit Arguments ld_logical [A].

  Definition external_expr : Set := list (prod string string).

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


  Inductive simple_species_expr_args : Set :=
  | Ssea_self : simple_species_expr_args
  | Ssea_species : ident -> simple_species_expr_args
  | Ssea_entity : ast expr -> simple_species_expr_args.

  Record simple_species_expr : Set := mk_simple_species_expr {
    sse_name : ident;
    sse_args : list simple_species_expr_args
  }.

  Record history : Set := mk_history {
    h_initial : ident;
    h_inherited : list (prod ident simple_species_expr)
  }.

  Record sig_field_info : Set := mk_sig_field_info {
    sfi_from : history;
    sfi_name : vname;
    sfi_type : ttype
  }.

  Parameter logical_expr : Set.

  Inductive binding_body : Set :=
  | Bb_logical : ast logical_expr -> binding_body
  | Bb_expr : ast expr -> binding_body.

  Record let_field_info : Set := mk_let_field_info {
    lfi_from : history;
    lfi_name : vname;
    lfi_params : list vname;
    lfi_type : ttype;
    lfi_binding_body : ast binding_body
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
  | Te_ident : ast ident -> type_expr
  | Te_fun : ast type_expr -> ast type_expr -> type_expr
  | Te_app : ast ident -> list (ast type_expr) -> type_expr
  | Te_prod : list (ast type_expr) -> type_expr
  | Te_self : type_expr
  | Te_prop : type_expr
  | Te_paren : ast type_expr -> type_expr.

  Inductive simple_type_def_body : Set :=
  | Stdb_alias : ast type_expr -> simple_type_def_body
  | Stdb_union : list (prod vname (list (ast type_expr))) -> simple_type_def_body
  | Stdb_record : list (prod string (ast type_expr)) -> simple_type_def_body.

  Record external_type_def_body : Set := mk_external_type_def_body {
    etdb_internal : option (ast simple_type_def_body);
    etdb_external : ast external_expr;
    etdb_bindings : ast (list (prod vname (ast external_expr)))
  }.

  Inductive type_def_body : Set :=
  | Tdb_simple : ast simple_type_def_body -> type_def_body
  | Tdb_external : ast external_type_def_body -> type_def_body.

  Record type_def : Set := mk_type_info {
    ti_name : vname;
    ti_params : list vname;
    ti_body : ast type_def_body
  }.
  
  Definition type_info : Set := ast type_def.

  Definition let_def_info : Set := ast (let_def (ast binding_body)).

  Parameter theorem_info : Set.

  Inductive phrase : Set := 
  | Phrase_open : open_info -> phrase
  | Phrase_use : use_info -> phrase
  | Phrase_species : species_info -> phrase
  | Phrase_collection : collection_info -> phrase
  | Phrase_type : type_info -> phrase
  | Phrase_let_def : let_def_info -> phrase
  | Phrase_theorem : theorem_info -> phrase
  | Phrase_expr : ast expr -> phrase.

  Record file : Set := mk_file {
    file_name : string;
    file_body : list phrase
  }.

End PCM.





