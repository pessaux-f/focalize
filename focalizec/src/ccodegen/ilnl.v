Require Import external.
Require Import pcm.
Require Import List.

Module ILNL.
  
  Record type_ident : Set := mk_type_ident {
    ti_file : string;
    ti_name : string
  }.

  Inductive type : Set :=
  | T_var : int -> type
  | T_arrow : type -> type -> type
  | T_tuple : list_type -> type
  | T_constr : type_ident -> list_type -> type
  | T_self : type
  | T_species : type_ident -> type

  with list_type : Set :=
  | lt_nil : list_type
  | lt_cons : type -> list_type -> list_type.

  Inductive constant : Set :=
    Constant : constant_desc -> info -> constant

  with constant_desc : Set :=
  | C_int : string -> constant_desc
  | C_float : string -> constant_desc
  | C_bool : string -> constant_desc
  | C_string : string -> constant_desc
  | C_char : char -> constant_desc.

  Record ident : Set := mk_ident {
    i_info : info;
    i_type : type;
    i_file : option string;
    i_coll : option string;
    i_name : string
  }.

  Inductive type_expr : Set :=
    Type_expr : type_expr_desc -> info -> type_expr

  with type_expr_desc : Set :=
  | Te_ident : ident -> type_expr_desc
  | Te_fun : type_expr -> type_expr -> type_expr_desc
  | Te_app : ident -> list_type_expr -> type_expr_desc
  | Te_prod : list_type_expr -> type_expr_desc
  | Te_self : type_expr_desc
  | Te_paren : type_expr -> type_expr_desc

  with list_type_expr : Set :=
  | lte_nil : list_type_expr
  | lte_cons : type_expr -> list_type_expr -> list_type_expr.

  Inductive simple_type_def_body : Set :=
    Simple_type_def_body : simple_type_def_body_desc -> info -> simple_type_def_body

  with simple_type_def_body_desc : Set := 
  | Stdb_alias : 
    type_expr -> simple_type_def_body_desc
  | Stdb_union :
    list (prod ident list_type_expr) -> simple_type_def_body_desc
  | Stdb_record :
    list (prod string type_expr) -> simple_type_def_body_desc.

  Record external_expr : Set := mk_external_expr {
    ee_lang : string;
    ee_code : string
  }.

  Record external_type_def_body : Set := mk_external_type_def_body {
    etdb_info : info;
    etdb_internal : option simple_type_def_body;
    etdb_external : external_expr;
    etdb_bindings : list (prod ident external_expr)
  }.

  Inductive type_def_body : Set :=
    Type_def_body : type_def_body_desc -> info -> type_def_body

  with type_def_body_desc : Set :=
  | Tdb_simple : simple_type_def_body -> type_def_body_desc
  | Tdb_external : external_type_def_body -> type_def_body_desc.

(*   Parameter type_def : Set. *)
  Record type_def : Set := mk_type_def {
    td_info : info;
    td_name : ident;
    td_params : list ident;
    td_body : type_def_body
  }.


  Inductive pattern : Set :=
    Pattern : pattern_desc -> info -> type ->pattern

  with pattern_desc : Set :=
  | P_const : constant -> pattern_desc
  | P_var : ident -> pattern_desc
  | P_as : pattern -> ident -> pattern_desc
  | P_wild : pattern_desc
  | P_constr : ident -> list_pattern -> pattern_desc
  | P_record : list_field_pattern -> pattern_desc
  | P_tuple : list_pattern -> pattern_desc
  | P_paren : pattern -> pattern_desc

  with list_pattern : Set := 
  | p_nil : list_pattern
  | p_cons : pattern -> list_pattern -> list_pattern

  with list_field_pattern : Set :=
  | pf_nil : list_field_pattern
  | pf_cons : string -> pattern -> list_field_pattern -> list_field_pattern.

  Inductive expr : Set :=
    Expr : expr_desc -> info -> type -> expr

  with expr_desc : Set :=
  | E_self : expr_desc
  | E_constant : constant -> expr_desc
  | E_fun : list ident -> expr -> expr_desc
  | E_var : ident -> expr_desc
  | E_app : expr -> list_expr -> expr_desc
  | E_constr : ident -> list_expr -> expr_desc
  | E_match : expr -> list_clause -> expr_desc
  | E_if : expr -> expr -> expr -> expr_desc
  | E_let : let_expr -> expr -> expr_desc
  | E_record : list_field -> expr_desc
  | E_record_access : expr -> string -> expr_desc
  | E_record_with : expr -> list_field -> expr_desc
  | E_tuple : list_expr -> expr_desc
  | E_external : external_expr -> expr_desc
  | E_paren : expr -> expr_desc

  with list_expr : Set :=
  | le_nil : list_expr
  | le_cons : expr -> list_expr -> list_expr

  with list_clause : Set :=
  | lc_nil : list_clause
  | lc_cons : pattern -> expr -> list_clause -> list_clause

  with let_expr : Set :=
  | Let_expr : bool -> bool -> list_binding -> info -> let_expr

  with list_binding : Set :=
  | b_nil : list_binding
  | b_cons : ident -> list ident -> expr -> info -> list_binding -> list_binding

  with list_field : Set :=
  | lf_nil : list_field
  | lf_cons : string -> expr -> list_field -> list_field.

  Record history : Set := mk_history {
    h_initial : ident;
    h_inherited : list_expr
  }.

  Record sig_field_info : Set := mk_sig_field_info {
    sfi_info : info;
    sfi_from : history;
    sfi_name : ident;
    sfi_type : type
  }.

  Record let_field_info : Set := mk_let_field_info {
    lfi_info : info;
    lfi_from : history;
    lfi_name : ident;
    lfi_params : list ident;
    lfi_binding_body : expr
  }.

  Inductive species_field : Set :=
  | Sf_sig : sig_field_info -> species_field
  | Sf_let : let_field_info -> species_field
  | Sf_let_rec : list let_field_info -> species_field.

  Record collection : Set := mk_collection {
    coll_name : ident;
    coll_body : list species_field
  }.


  Record binding : Set := mk_binding {
    b_info : info;
    b_name : ident;
    b_params : list ident;
    b_body : expr
  }.

  (* Parameter let_def : Set. *)
  Record let_def : Set := mk_let_def {
    ld_info : info;
    ld_rec : bool;
    ld_local : bool;
    ld_bindings : list binding
  }.

  Inductive phrase : Set :=
    Phrase : phrase_desc -> info -> phrase

  with phrase_desc : Set :=
  | Ph_open : string -> phrase_desc
  | Ph_use : string -> phrase_desc
  | Ph_collection : collection -> phrase_desc
  | Ph_type : type_def -> phrase_desc
  | Ph_let_def : let_def -> phrase_desc
  | Ph_expr : expr -> phrase_desc.

  Record file : Set := mk_file {
    file_name : string;
    file_body : list phrase
  }.

End ILNL.