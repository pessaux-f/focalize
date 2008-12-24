%{
(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*            François Pessaux                                         *)
(*                                                                     *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2006, 2008 LIP6 and INRIA                                *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: parser.mly,v 1.120 2008-12-24 08:13:04 weis Exp $ *)

open Parsetree;;

let mk_loc () = {
  Location.l_beg = Parsing.symbol_start_pos ();
  Location.l_end = Parsing.symbol_end_pos ();
}
;;

let mk_doc_elem de = {
  de_loc = mk_loc ();
  de_desc = de;
}
;;

let mk_doc doc desc = {
  ast_loc = mk_loc ();
  ast_desc = desc;
  ast_doc = doc;
  ast_type = Parsetree.ANTI_none;
}
;;

let mk d = mk_doc [] d;;

let mk_local_ident vname = mk (I_local vname);;

let mk_qual_vname qual vname =
  match qual with
  | None -> Vname vname
  | Some qual -> Qualified (qual, vname)
;;

let mk_global_ident qual vname =
  mk (I_global (mk_qual_vname qual vname))
;;

let mk_global_species_ident = mk_global_ident;;

let mk_label_ident qual vname =
  mk (LI (mk_qual_vname qual vname))
;;

let mk_global_constructor_ident qual vname =
  mk (CI (mk_qual_vname qual vname))
;;

let mk_local_expr_ident vname =
  mk (EI_local vname)
;;

let mk_global_expr_ident qual vname =
  mk (EI_global (mk_qual_vname qual vname))
;;

let mk_method_expr_ident opt_qualified_vname method_vname =
  mk (EI_method (opt_qualified_vname, method_vname))
;;

let mk_local_expr_var vname =
  mk (E_var (mk_local_expr_ident vname))
;;
let mk_global_expr_var qual vname =
  mk (E_var (mk_global_expr_ident qual vname))
;;

(* Infix/Prefix operators without scope MUST be parsed as LOCAL!     *)
(* A global identifier whose scope is None is reserved for           *)
(* identifiers of the form "#foo", meaning that we refer to          *)
(* identifier "foo" at toplevel of the current compilation unit.     *)
let mk_infix_application e1 s e2 =
  mk (E_app (mk_local_expr_var (Viident s), [ e1; e2 ]))
;;
let mk_prefix_application s e1 =
  mk (E_app (mk_local_expr_var (Vpident s), [ e1 ]))
;;

let mk_cons () = mk_global_constructor_ident (Some "basics") (Vuident "::");;
let mk_nil () = mk_global_constructor_ident (Some "basics") (Vuident "[]");;
let mk_unit () = mk_global_constructor_ident (Some "basics") (Vuident "()");;

let mk_proof_label (s1, s2) =
  try int_of_string s1, s2 with
  | Failure _ -> assert false
;;

%}

%token EOF

/* Identifiers */
/* In the following identifier nomenclature, */
/* R stands for regular, P for prefix, I for Infix, Q for quoted. */
/* U stands for uppercase, L for lowercase. */
%token <string> RLIDENT /* Lower case ident (e.g. x, _1, or _xY) */
%token <string> RUIDENT /* Upper case ident (e.g. A, _B, or _Ax) */
%token <string> PLIDENT /* Prefix lowercase ident (e.g. ! or ~) */
%token <string> PUIDENT /* Prefix uppercase ident (e.g. [], [!], or [~]) */
%token <string> ILIDENT /* Infix lowercase ident (e.g + or +matrix) */
%token <string> IUIDENT /* Infix uppercase ident (e.g :: or :+m:) */
%token <string> QLIDENT /* Quoted lowercase ident (e.g. 'a ) */
%token <string> QUIDENT /* Quoted uppercase ident (e.g. 'C ) */

/* Basic constants */
%token <string> INT
%token <string> FLOAT
%token <string> BOOL
%token <string> STRING
%token <char> CHAR

/* Special tokens */
%token <string> DOCUMENTATION
%token <string * string> PROOF_LABEL
%token <string> EXTERNAL_CODE

/* Arithmetic operators */
%token <string> BACKSLASH_OP
%token <string> PLUS_OP
%token <string> DASH_OP
%token <string> STAR_OP
%token <string> SLASH_OP
%token <string> PERCENT_OP
%token <string> STAR_STAR_OP

/* Nested symbols */
%token LPAREN
%token RPAREN
%token LRPARENS
%token LBRACE
%token RBRACE
%token LRBRACES
%token LBRACKET
%token RBRACKET
%token LRBRACKETS

/* General infix and prefix operators */
%token COMMA
%token <string> COMMA_OP

%token CONJUNCTION
%token DISJUNCTION
%token NEGATION
%token DASH_GT
%token <string> DASH_GT_OP
%token LT_DASH_GT
%token <string> LT_DASH_GT_OP

%token <string> LT_DASH_OP
%token SHARP
%token <string> SHARP_OP
%token BANG
%token <string> BANG_OP
%token BAR
%token <string> BAR_OP
%token <string> AMPER_OP
%token <string> TILDA_OP
%token UNDERSCORE
%token EQUAL
%token <string> EQ_OP
%token <string> LT_OP
%token <string> GT_OP
%token SEMI
%token <string> SEMI_OP
%token SEMI_SEMI
%token <string> SEMI_SEMI_OP
%token COLON
%token <string> COLON_OP
%token COLON_COLON
%token <string> COLON_COLON_OP
%token <string> BACKQUOTE_OP
%token <string> AT_OP
%token <string> HAT_OP
%token <string> QUESTION_OP
%token <string> DOLLAR_OP
%token DOT

/* Keywords */
%token ABSTRACT
%token ALL
%token ALIAS
%token AND
%token AS
%token ASSUME
%token ASSUMED
%token BEGIN
%token BY
%token CAML
%token COLLECTION
%token COQ
%token COQ_REQUIRE
%token DEFINITION
%token ELSE
%token END
%token EX
%token EXTERNAL
%token FUNCTION
%token HYPOTHESIS
%token IF
%token IN
%token INHERITS
%token INTERNAL
%token IMPLEMENTS
%token IS
%token LET
%token LEXICOGRAPHIC
%token LOCAL
%token LOGICAL
%token MATCH
%token MEASURE
%token NOT
%token NOTATION
%token OF
%token ON
%token OPEN
%token OR
%token ORDER
%token PRIVATE
%token PROOF
%token PROP
%token PROPERTY
%token PROVE
%token PUBLIC
%token QED
%token REC
%token RELATIONAL
%token REPRESENTATION
%token SELF
%token SIGNATURE
%token SPECIES
%token STEP
%token STRUCTURAL
%token TERMINATION
%token THEN
%token THEOREM
%token TYPE
%token USE
%token WITH

/* Precedences and associativities. */

/* Binary operators. */

%nonassoc IN
%nonassoc SEMI_SEMI_OP             /* expr (e OP e) with OP starting with ";;" */
/* %nonassoc below_SEMI */
/* %nonassoc SEMI                  /* below EQ ({lbl=...; lbl=...}) */
%nonassoc SEMI_OP                  /* expr (e OP e) with OP starting with ';' */
/* %nonassoc LET */                /* above SEMI ( ...; let ... in ...) */
/* %nonassoc below_WITH */
/* %nonassoc FUNCTION WITH */      /* below BAR  (match ... with ...) */
%nonassoc prec_quantifier
%nonassoc LT_DASH_GT LT_DASH_GT_OP
       /* expr (e OP e) with OP being "<->" or starting with "<->" */
%right    DISJUNCTION              /* logical_expr (le \/ le \/ le) */
%right    CONJUNCTION              /* logical_expr (le /\ le /\ le) */
%nonassoc NEGATION                 /* logical_expr (~ le) */
%right    AND                      /* let ... and ... */
/* %nonassoc THEN */               /* below ELSE (if ... then ...) */
%nonassoc ELSE                     /* (if ... then ... else ...) */
%right    BACKSLASH_OP             /* expr (e OP e OP e) with OP starting with '\\' */
%nonassoc LT_DASH_OP               /* below COLON_OP */
       /* expr (e OP e) with OP starting with "<-" */
%right    COLON_OP
       /* expr (e OP e OP e) with OP starting with ':' (e.g. ":=") */
%nonassoc AS                       /* pattern (pat as RLIDENT) */
%right    BAR                      /* Dangling match (match ... with ...) */
%left     COMMA COMMA_OP
       /* expr/expr_comma_list (e OP e OP e) with OP respectively */
       /* being ',' or starting with ',' */
%right    DASH_GT DASH_GT_OP
       /* type_expr (t -> t -> t) */
       /* expr (e OP e OP e) wih OP starting with "->" e2) */
%right    BAR_OP                   /* expr (e || e || e) */
%right    AMPER_OP                 /* expr (e && e && e) */
%nonassoc TILDA_OP                 /* expr (~| e) */
/* %nonassoc below_EQ */
%left     EQUAL EQ_OP LT_OP GT_OP
       /* expr (e OP e OP e) with OP respectively: */
       /* being '=', starting with '=', '<', or '>' */
%right    AT_OP HAT_OP
       /* expr (e OP e OP e) with OP starting with '@' or '^' */
%right    ILIDENT
       /* expr (e OP e OP e) with OP being a user defined lowercase ident. */
%right    COLON_COLON COLON_COLON_OP IUIDENT
       /* expr (e OP e OP e) with OP being "::", or starting with "::" */
%left     PLUS_OP DASH_OP
       /* expr (e OP e OP OP e) with OP starting with '+' or '-' */
%left     STAR_OP SLASH_OP PERCENT_OP
       /* expr (e OP e OP e) with OP starting with '*', '/', or '%' */
%right    STAR_STAR_OP
       /* expr (e OP e OP e) with OP starting with "**" */

/* Unary prefix operators. */
%nonassoc PLIDENT
       /* expr OP e with OP a lowercase ident enclosed with */
       /* backquote chars. */
%nonassoc BACKQUOTE_OP             /* expr OP e with OP starting with '\`' */
%nonassoc QUESTION_OP              /* expr OP e with OP starting with '?' */
%nonassoc DOLLAR_OP                /* expr OP e with OP starting with '$' */
%nonassoc BANG_OP                  /* expr OP e with OP starting with '!' */

/* Predefined precedences to resolve conflicts. */
%nonassoc prec_unary_minus         /* unary DASH_OP e.g. DASH_OP is '-' */
%nonassoc prec_constant_constructor /* cf. simple_expr (C versus C x) */
                                   /* above AS BAR COLON_COLON COMMA */
%nonassoc below_SHARP
%nonassoc SHARP_OP                 /* simple_expr lident # RUIDENT */
%nonassoc DOT                      /* simple_expr (simple_expr . label) */
%nonassoc below_RPAREN
%nonassoc RPAREN
/* Finally, the first tokens of simple_expr are above everything else. */
/* %nonassoc BEGIN INT FLOAT BOOL STRING CHAR */
/*          LBRACE LBRACKET RLIDENT RUIDENT */
%nonassoc LPAREN

%start file
%type <Parsetree.file> file
%%

file:
  | phrase_list { mk (File $1) }
  | opt_doc BEGIN phrase_list { mk_doc $1 (File $3) }
;

phrase_list:
  | EOF { [] }
  | phrase phrase_list { $1 :: $2 }
;

phrase:
  | define_let SEMI_SEMI { mk (Ph_let $1) }
  | define_logical SEMI_SEMI { mk (Ph_let $1) }
  | define_theorem SEMI_SEMI { mk (Ph_theorem $1) }
  | define_type SEMI_SEMI { mk (Ph_type $1) }
  | define_species SEMI_SEMI { mk (Ph_species $1) }
  | define_collection SEMI_SEMI { mk (Ph_collection $1) }
  | opt_doc OPEN STRING SEMI_SEMI { mk_doc $1 (Ph_open $3) }
  | opt_doc USE STRING SEMI_SEMI { mk_doc $1 (Ph_use $3) }
  | opt_doc COQ_REQUIRE STRING SEMI_SEMI { mk_doc $1 (Ph_coq_require $3) }
  | opt_doc expr SEMI_SEMI { mk_doc $1 (Ph_expr $2) }
;

external_language:
  | CAML { EL_Caml }
  | COQ { EL_Coq }
  | STRING { EL_external $1 }
;

external_expr_one:
  | BAR external_language DASH_GT EXTERNAL_CODE { ($2, $4) }
;

external_expr_desc:
  | external_expr_one { [ $1 ] }
  | external_expr_one external_expr_desc { $1 :: $2 }
;

external_expr:
  external_expr_desc { mk $1 }
;

/**** TYPE DEFINITION ****/

define_type:
  | opt_doc TYPE type_vname define_type_params EQUAL define_type_body
    { mk_doc $1 {td_name = $3; td_params = $4; td_body = $6; } }
;

define_type_params:
  | { [] }
  | LPAREN define_type_param_comma_list RPAREN { $2 }
;

define_type_param_comma_list:
  | type_param_vname { [ $1 ] }
  | type_param_vname COMMA define_type_param_comma_list { $1 :: $3 }
;

define_type_body:
  | opt_doc ABSTRACT define_type_body_private
    { mk_doc $1 (TDB_abstract $3) }
  | opt_doc PRIVATE define_type_body_private
    { mk_doc $1 (TDB_private $3) }
  | opt_doc RELATIONAL define_type_body_private
    { mk_doc $1 (TDB_relational $3) }
  | opt_doc PUBLIC define_type_body_contents
    { mk_doc $1 (TDB_public $3) }
  | define_type_body_contents
    { mk (TDB_public $1) }
;

/* To be completed: should also have bindings. */
define_type_body_private:
  | define_type_body_contents
    { $1 }
;

define_type_body_contents:
  | opt_doc define_type_body_regular { mk_doc $1 (TDBS_regular $2) }
  | opt_doc define_type_body_external { mk_doc $1 (TDBS_external $2) }
;

define_type_body_external:
  | INTERNAL define_type_body_regular_opt
    opt_doc EXTERNAL external_expr_desc following_external_binding_list
     { mk { etdb_internal = $2;
            etdb_external = mk_doc $3 $5;
            etdb_bindings = mk $6; }
     }
;

external_binding:
  | external_value_vname EQUAL external_expr { mk ($1, $3) }
;

following_external_binding_list:
  | { [] }
  | AND external_binding following_external_binding_list { $2 :: $3 }
;

define_type_body_regular_opt:
  | { None }
  | define_type_body_regular { Some $1 }
;

define_type_body_regular:
  | ALIAS type_expr { mk (RTDB_alias $2) }
  | define_sum { mk (RTDB_union $1) }
  | define_product { mk (RTDB_record $1) }
;

define_sum:
  | define_constructor_list { $1 }
;

define_constructor:
  | constructor_vname { ($1, []) }
  | constructor_vname LPAREN type_expr_comma_list RPAREN { ($1, $3) }
;
define_constructor_list:
  | BAR define_constructor { [ $2 ] }
  | BAR define_constructor define_constructor_list { $2 :: $3 }
;

define_product:
  | LBRACE define_record_field_list RBRACE { $2 }
;
define_record_field_list:
  | label_vname EQUAL type_expr opt_semi
    { [ ($1, $3) ] }
  | label_vname EQUAL type_expr SEMI define_record_field_list
    { ($1, $3) :: $5 }
;

/**** SPECIES ****/

define_species:
  | opt_doc
    SPECIES species_vname define_species_params define_species_inherits EQUAL
      species_fields
    END
    { mk_doc $1
        { sd_name = $3; sd_params = $4;
          sd_inherits = $5; sd_fields = $7; } }
;

define_species_params:
  | { [] }
  | LPAREN define_species_param_list RPAREN { $2 }
;

define_species_param_list:
  | define_species_param { [ $1 ] }
  | define_species_param COMMA define_species_param_list { $1 :: $3 }
;

define_species_param:
  | bound_vname IN carrier_ident { ($1, mk (SPT_in $3)) }
  | collection_vname IS species_expr { ($1, mk (SPT_is $3)) }
;

define_species_inherits:
  | opt_doc INHERITS species_expr_list { mk_doc $1 $3}
  | { mk [] }

species_expr_list:
  | species_expr {[ $1 ]}
  | species_expr COMMA species_expr_list { $1 :: $3 }
;

species_expr:
  | species_ident
    { mk { se_name = $1; se_params = []; } }
  | species_ident LPAREN species_param_list RPAREN
    { mk { se_name = $1; se_params = $3; } }

species_param:
  | expr { mk (SP $1) }
;

species_param_list:
  | species_param { [ $1 ] }
  | species_param COMMA species_param_list { $1 :: $3 }
;

species_fields:
  | { [] }
  | species_field SEMI species_fields { $1 :: $3 }

species_field :
  | define_representation { mk (SF_rep $1) }
  | define_signature      { mk (SF_sig $1) }
  | define_let            { mk (SF_let $1) }
  | define_logical        { mk (SF_let $1) }
  | define_property       { mk (SF_property $1) }
  | define_theorem        { mk (SF_theorem $1) }
  | define_proof          { mk (SF_proof $1) }
  | define_termination_proof
                 { mk (SF_termination_proof $1) }
;

termination_proof_profiles:
  | { [] }
  | termination_proof_profiles AND termination_proof_profile
    { $3 :: $1 }
;

termination_proof_profile:
  | bound_vname LPAREN param_list RPAREN
    { mk {tpp_name = $1; tpp_args = $3; } }
;

define_termination_proof:
  | opt_doc TERMINATION PROOF OF termination_proof_profiles EQUAL termination_proof
    { mk_doc $1 {tpd_profiles = List.rev $5; tpd_termination_proof = $7; } }
;

define_proof:
  | opt_doc PROOF OF property_vname EQUAL proof
    { mk_doc $1 { pd_name = $4; pd_proof = $6; } }
;

define_representation:
  | opt_doc REPRESENTATION EQUAL representation_type
    { mk_doc $1 $4 }
;

/**** REPRESENTATION TYPE EXPRESSIONS ****/
representation_type:
  | simple_representation_type
    { $1 }
  | representation_type_tuple
    { RTE_prod (List.map mk $1) }
  | representation_type DASH_GT representation_type
    { RTE_fun (mk $1, mk $3) }
;

simple_representation_type:
  | glob_ident
    { RTE_ident $1 }
    /* To have capitalized species names as representation types. */
  | species_vname
    { RTE_ident (mk_local_ident $1) }
    /* To have qualified species names as representation types. */
  | species_glob_ident
    { RTE_ident $1 }
  | RLIDENT { RTE_ident (mk_local_ident (Vlident $1)) }
  | glob_ident LPAREN representation_type_comma_list RPAREN
    { RTE_app ($1, $3) }
    /* To have non-qualified parameterized type constructors names. */
  | RLIDENT LPAREN representation_type_comma_list RPAREN
    { let paramd_cstr = mk_local_ident (Vlident $1) in
      RTE_app (paramd_cstr, $3) }
  | LPAREN representation_type RPAREN
    { RTE_paren (mk $2) }
;

representation_type_tuple:
  | simple_representation_type STAR_OP simple_representation_type
    { [ $1; $3 ] }
  | representation_type_tuple STAR_OP simple_representation_type
    { $1 @ [ $3 ] }
;

representation_type_comma_list:
  | representation_type { [ mk $1 ] }
  | representation_type COMMA representation_type_comma_list { mk $1 :: $3 }
;

/**** COLLECTION DEFINITION ****/

define_collection:
  | opt_doc COLLECTION collection_vname IMPLEMENTS species_expr
    { mk_doc $1 { cd_name = $3; cd_body = $5; } }
;

/**** FUNCTION & VALUES DEFINITION ****/

let_binding:
  | opt_local LET binding following_binding_list
    { mk { ld_rec = RF_no_rec; ld_logical = LF_no_logical; ld_local = $1;
           ld_bindings = $3 :: $4; ld_termination_proof = None; } }
  | opt_local LET REC binding following_binding_list opt_termination_proof
    { mk { ld_rec = RF_rec; ld_logical = LF_no_logical; ld_local = $1;
           ld_bindings = $4 :: $5; ld_termination_proof = $6; } }
;

define_let:
  | opt_doc let_binding { mk_doc $1 ($2.ast_desc) }
;

logical_binding:
  | LOGICAL let_binding
    { mk { $2.ast_desc with ld_logical = LF_logical; } }
;

define_logical:
  | opt_doc logical_binding { mk_doc $1 $2.ast_desc }
;

/** Since logical let is followed by a logical_expr, and since logical_expr
  embeds all expressions, at parsing stage, everything is temporarily
  considered to be a logical_expr. At scoping stage, a verification will be
  performed: if the logical_flag is [LF_no_logical] then we will ensure that
  the logical_expr is a [Pr_expr] and remove the constructor to get the
  effective [exp] that is the body of the real "Let". Otherwise we will keep
  the logical_expr as it is, since the binding is a logical binding.

  The only exception is for externals that are never logical bindings ! */
binding:
  | bound_vname EQUAL logical_expr
    { mk {
        b_name = $1; b_params = []; b_type = None;
        b_body = Parsetree.BB_logical $3 }
    }
  | bound_vname EQUAL INTERNAL type_expr EXTERNAL external_expr
    { mk {
        b_name = $1; b_params = []; b_type = Some $4;
        b_body = Parsetree.BB_computational (mk (E_external $6)) }
    }
  | bound_vname IN type_expr EQUAL logical_expr
    { mk {
        b_name = $1; b_params = []; b_type = Some $3;
        b_body = Parsetree.BB_logical $5 }
    }
  | bound_vname LPAREN param_list RPAREN EQUAL logical_expr
    { mk {
        b_name = $1; b_params = $3; b_type = None;
        b_body = Parsetree.BB_logical $6 }
    }
  | bound_vname LPAREN param_list RPAREN IN type_expr
    EQUAL logical_expr
    { mk {
        b_name = $1; b_params = $3; b_type = Some $6;
        b_body = Parsetree.BB_logical $8 }
    }
;

opt_termination_proof:
  | { None }
  | TERMINATION PROOF EQUAL termination_proof { Some $4 }
;

termination_proof:
  | opt_doc STRUCTURAL bound_vname { mk_doc $1 (TP_structural $3) }
  | opt_doc LEXICOGRAPHIC fact_list { mk_doc $1 (TP_lexicographic $3) }
  | opt_doc MEASURE expr ON param_list proof { mk_doc $1 (TP_measure ($3, $5, $6)) }
  | opt_doc ORDER expr ON param_list proof { mk_doc $1 (TP_order ($3, $5, $6)) }
;

param_list:
  | param { [ $1 ] }
  | param COMMA param_list { $1 :: $3 }
;

param:
  | bound_vname { ($1, None) }
  | bound_vname IN type_expr { ($1, Some $3) }
;

/**** PROPERTIES & THEOREM DEFINITION ****/

signature_binding:
  | SIGNATURE bound_vname COLON type_expr
    { { sig_name = $2; sig_type = $4; sig_logical = LF_no_logical; } }
  | LOGICAL signature_binding
    { { $2 with sig_logical = LF_logical; }; }

define_signature:
  | opt_doc signature_binding
    { mk_doc $1 $2 }
;

define_property:
  | opt_doc PROPERTY property_vname COLON logical_expr
    { mk_doc $1 { prd_name = $3; prd_logical_expr = $5; } }
;

define_theorem:
  | opt_doc opt_local THEOREM theorem_vname COLON logical_expr PROOF EQUAL proof
    { mk_doc $1
        { th_name = $4; th_local = $2;
          th_stmt = $6; th_proof = $9 } }
;

logical_expr:
  | ALL bound_vname_list in_type_expr COMMA logical_expr  %prec prec_quantifier
    { mk (Pr_forall ($2, $3, $5))}
  | EX bound_vname_list in_type_expr COMMA logical_expr   %prec prec_quantifier
    { mk (Pr_exists ($2, $3, $5))}
  | logical_expr DASH_GT logical_expr
    { mk (Pr_imply ($1, $3)) }
  | logical_expr LT_DASH_GT logical_expr
    { mk (Pr_equiv ($1, $3)) }
  | logical_expr DISJUNCTION logical_expr
    { mk (Pr_or ($1, $3)) }
  | logical_expr CONJUNCTION logical_expr
    { mk (Pr_and ($1, $3)) }
  | NEGATION logical_expr
    { mk (Pr_not $2) }
  | expr %prec below_RPAREN
    { mk (Pr_expr $1) }
  | LPAREN logical_expr RPAREN
    { mk (Pr_paren $2) }
;

in_type_expr:
  | IN type_expr
    { $2 }
;

/**** PROOFS ****/

proof:
    /* Trailing is the reason why the proof was not given. */
  | opt_doc enforced_dependency_list ASSUMED EXTERNAL_CODE
    { mk_doc $1 (Pf_assumed ($2, $4)) }
  | opt_doc QED
    { mk_doc $1 (Pf_auto []) }
  | opt_doc BY fact_list
    { mk_doc $1 (Pf_auto $3) }
  | opt_doc COQ PROOF enforced_dependency_list EXTERNAL_CODE
    { mk_doc $1 (Pf_coq ($4, $5)) }
  | proof_node_list
    { mk (Pf_node $1) }
  | opt_doc DOT
    { mk_doc $1 (Pf_auto []) }
;

proof_node_list:
  | proof_node_qed { [ $1 ] }
  | proof_node proof_node_list { $1 :: $2 }
;

proof_node:
  | opt_doc PROOF_LABEL statement proof
    { mk_doc $1 (PN_sub (mk_proof_label $2, $3, $4)) }
;

proof_node_qed:
  | opt_doc PROOF_LABEL QED proof
    { mk_doc $1 (PN_qed (mk_proof_label $2, $4)) }
;

fact_list:
  | QED
    { [] }
  | fact facts
    { $1 :: $2 }
;

facts:
  | { [] }
  | fact facts
    { $1 :: $2 }
;

fact:
  | DEFINITION OF definition_ident_comma_list { mk (F_definition $3) }
  | HYPOTHESIS proof_hypothesis_list { mk (F_hypothesis $2) }
  | PROPERTY property_ident_comma_list { mk (F_property ($2)) }
  | THEOREM property_ident_comma_list { mk (F_property ($2)) }
  | STEP proof_label_comma_list { mk (F_node (List.map mk_proof_label $2)) }
;

enforced_dependency_list:
  | { [ ] }
  | enforced_dependency enforced_dependency_list { $1 :: $2 }
;

enforced_dependency:
  | DEFINITION OF definition_ident_comma_list { mk (Ed_definition $3) }
  | PROPERTY property_ident_comma_list { mk (Ed_property ($2)) }
;

proof_hypothesis:
  | RUIDENT { Vuident $1 }
  | RLIDENT { Vlident $1 }
;

proof_hypothesis_list:
  | proof_hypothesis COMMA proof_hypothesis_list { $1 :: $3 }
  | proof_hypothesis { [ $1 ] }
;

opt_logical_expr:
  | { None }
  | PROVE logical_expr
    { Some $2 }
;

statement:
  | hypothesis_list opt_logical_expr
    { mk { s_hyps = $1; s_concl = $2; } }
;

hypothesis:
  | ASSUME bound_vname IN type_expr { mk (H_variable ($2, $4)) }
  | ASSUME proof_hypothesis COLON logical_expr { mk (H_hypothesis ($2, $4)) }
  | NOTATION proof_hypothesis EQUAL expr { mk (H_notation ($2, $4)) }
;

hypothesis_list:
  | { [] }
  | hypothesis COMMA hypothesis_list { $1 :: $3 }
;

/**** TYPE EXPRESSIONS ****/
type_expr:
  | simple_type_expr
    { $1 }
  | type_tuple
    { mk (TE_prod $1) }
  | type_expr DASH_GT type_expr
    { mk (TE_fun ($1, $3)) }
;

/* Type expressions that can appear inside a tuple. */
simple_type_expr:
  | SELF
    { mk TE_self }
  | PROP
    { mk TE_prop }
  | QLIDENT
    { mk (TE_ident (mk_local_ident (Vqident $1))) }
  | RLIDENT
    { mk (TE_ident (mk_local_ident (Vlident $1))) }
  | glob_ident
    { mk (TE_ident $1) }
    /* To have capitalized species names as types. */
  | species_vname
    { mk (TE_ident (mk_local_ident $1)) }
    /* To have qualified species names as types. */
  | species_glob_ident
    { mk (TE_ident $1) }
  | glob_ident LPAREN type_expr_comma_list RPAREN
    { mk (TE_app ($1, $3)) }
  | RLIDENT LPAREN type_expr_comma_list RPAREN
    { mk (TE_app (mk_local_ident (Vlident $1), $3)) }
  | LPAREN type_expr RPAREN
    { mk (TE_paren $2) }
;

type_tuple:
  | simple_type_expr STAR_OP simple_type_expr
    { [ $1; $3 ] }
  | type_tuple STAR_OP simple_type_expr
    { $1 @ [ $3 ] }
;

type_expr_comma_list:
  | type_expr { [ $1 ] }
  | type_expr COMMA type_expr_comma_list { $1 :: $3 }
;

constructor_ref:
  | constructor_vname
    { mk_global_constructor_ident None $1 }
  | opt_lident SHARP constructor_vname
    { mk_global_constructor_ident $1 $3 }
;

/* IDENTIFIERS */

glob_ident:
  | opt_lident SHARP bound_vname
    { mk_global_ident $1 $3 }
;

species_glob_ident:
  | opt_lident SHARP species_vname
    { mk_global_species_ident $1 $3 }
;

/* Only used to prefix global notation (i.e. with '#'). */
opt_lident:
  | { None }
  | RLIDENT
    { Some $1 }
;

/* Only used to prefix explicit method call notation (i.e. with '!'). */
opt_qualified_vname:
  | { None }
  | SELF
    { Some (mk_qual_vname None (Vuident "Self")) }
  | RUIDENT
    { Some (mk_qual_vname None (Vuident $1)) }
    /* "Module" name qualification. To allow a species name */
    /* to be "module"-scoped in a method call. */
    /* E.g. my_file#My_species!my_method. */
  | opt_lident SHARP RUIDENT
    { Some (mk_qual_vname $1 (Vuident $3)) }
;

/**** EXPRESSIONS ****/

expr_ident:
  | opt_lident SHARP bound_vname
    { mk_global_expr_ident $1 $3 }
  | opt_qualified_vname BANG method_vname
    { mk_method_expr_ident $1 $3 }
  | bound_vname
    { mk_local_expr_ident $1 }
;

simple_expr:
  | constant
    { mk (E_const $1) }
  | expr_ident
    { mk (E_var $1) }
  | simple_expr DOT label_ident
    { mk (E_record_access ($1, $3)) }
  | LBRACE record_field_list RBRACE
    { mk (E_record $2) }
  | LBRACE simple_expr WITH record_field_list RBRACE
    { mk (E_record_with ($2, $4)) }
  | LBRACKET expr_semi_list RBRACKET
    { $2 }
  | LPAREN expr COMMA expr_comma_list RPAREN
    { mk (E_tuple ($2 :: $4)) }
  | LPAREN expr RPAREN
    { mk (E_paren $2) }
;

expr:
  | SELF
    { mk E_self }
  | simple_expr %prec below_SHARP
    { $1 }
  | constructor_ref LPAREN expr_comma_list RPAREN { mk (E_constr ($1, $3)) }
  | constructor_ref { mk (E_constr ($1, [])) } %prec prec_constant_constructor
  | FUNCTION bound_vname_list DASH_GT expr
    { mk (E_fun ($2, $4)) }
  | expr LPAREN expr_comma_list RPAREN
    { mk (E_app ($1, $3)) }
  | MATCH expr WITH clause_list
    { mk (E_match ($2, $4)) }
  | IF expr THEN expr ELSE expr
    { mk (E_if ($2, $4, $6)) }
  | let_binding IN expr
    { mk (E_let ($1, $3)) }

  /* Binary operators */
  | expr ILIDENT expr
    { mk_infix_application $1 $2 $3 }

  | expr SEMI_SEMI_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr SEMI_OP expr
    { mk_infix_application $1 $2 $3 }

  | expr LT_DASH_GT_OP expr
    { mk_infix_application $1 $2 $3 }

  | expr BACKSLASH_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr LT_DASH_OP expr
    { mk_infix_application $1 $2 $3 }

  | expr COLON_OP expr
    { mk_infix_application $1 $2 $3 }

  | expr COMMA_OP expr
    { mk_infix_application $1 $2 $3 }

  | expr DASH_GT_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr BAR_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr AMPER_OP expr
    { mk_infix_application $1 $2 $3 }

  | expr EQUAL expr
    { mk_infix_application $1 "=" $3 }
    | expr EQ_OP expr
      { mk_infix_application $1 $2 $3 }
    | expr LT_OP expr
      { mk_infix_application $1 $2 $3 }
    | expr GT_OP expr
      { mk_infix_application $1 $2 $3 }

  | expr HAT_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr AT_OP expr
    { mk_infix_application $1 $2 $3 }

  | expr COLON_COLON expr
    { mk (E_constr (mk_cons (), [ $1; $3 ])) }
  | expr COLON_COLON_OP expr
    { mk_infix_application $1 $2 $3 }

  | expr PLUS_OP expr
    { mk_infix_application $1 $2 $3 }
    | expr DASH_OP expr
      { mk_infix_application $1 $2 $3 }
  | expr STAR_OP expr
    { mk_infix_application $1 $2 $3 }
    | expr SLASH_OP expr
      { mk_infix_application $1 $2 $3 }
    | expr PERCENT_OP expr
      { mk_infix_application $1 $2 $3 }
  | expr STAR_STAR_OP expr
    { mk_infix_application $1 $2 $3 }

  /* Unary operators. */
  | TILDA_OP expr
    { mk_prefix_application $1 $2 }
  | BACKQUOTE_OP expr
    { mk_prefix_application $1 $2 }
  | QUESTION_OP expr
    { mk_prefix_application $1 $2 }
  | DOLLAR_OP expr
    { mk_prefix_application $1 $2 }
  | BANG_OP expr
    { mk_prefix_application $1 $2 }
  | SHARP_OP expr
    { mk_prefix_application $1 $2 }
  | DASH_OP expr %prec prec_unary_minus
    { mk_prefix_application $1 $2 }
  | PLIDENT expr
    { mk_prefix_application $1 $2 }

  | EXTERNAL external_expr END
    { mk (E_external $2) }
;

expr_semi_list:
  | { mk (E_constr (mk_nil (), [])) }
  | expr
    { mk (E_constr (mk_cons (), [ $1; mk (E_constr (mk_nil (), [])) ])) }
  | expr SEMI expr_semi_list
    { mk (E_constr (mk_cons (), [ $1; $3 ])) }
;

expr_comma_list:
  | expr
    { [ $1 ] }
  | expr COMMA expr_comma_list
    { $1 :: $3 }
;

record_field_list:
  | label_ident EQUAL expr opt_semi
    { [ ($1, $3) ] }
  | label_ident EQUAL expr SEMI record_field_list
    { ($1, $3) :: $5 }
;

/* In a proof, "by definition" is always refering to something local      */
/* to the species. Hence, it is not allowed to say "by definition C!foo". */
/* We enforce this in the syntax.                                         */
definition_ident:
  | property_vname
    { mk_local_expr_ident $1 }
  | opt_lident SHARP property_vname
    { mk_global_expr_ident $1 $3 }
;

property_ident:
  | definition_ident
    { $1 }
  | opt_qualified_vname BANG property_vname
    { mk_method_expr_ident $1 $3 }     /* "by property C!foo" is allowed. */
;

carrier_ident:
  | species_ident { $1 }
;

species_ident:
  | species_vname
    { mk_local_ident $1 }
  | species_glob_ident
    { $1 }
;

definition_ident_comma_list:
  | definition_ident COMMA definition_ident_comma_list { $1 :: $3 }
  | definition_ident { [ $1 ] }
;

property_ident_comma_list:
  | property_ident COMMA property_ident_comma_list { $1 :: $3 }
  | property_ident { [ $1 ] }
;

proof_label_comma_list:
  | PROOF_LABEL COMMA proof_label_comma_list { $1 :: $3 }
  | PROOF_LABEL { [ $1 ] }
;

clause_list:
  | BAR clause
    { [ $2 ] }
  | BAR clause clause_list
    { $2 :: $3 }
;

clause:
  | pattern DASH_GT expr
    { ($1, $3) }
;

constant:
  | INT { mk (C_int $1) }
  | FLOAT { mk (C_float $1) }
  | BOOL { mk (C_bool $1) }
  | STRING { mk (C_string $1) }
  | CHAR { mk (C_char $1) }
;

pattern:
  | constant { mk (P_const $1) }
  | RLIDENT { mk (P_var (Vlident $1)) }
  | UNDERSCORE { mk (P_wild) }
  | constructor_ref LPAREN pattern_comma_list RPAREN { mk (P_constr ($1, $3)) }
  | constructor_ref { mk (P_constr ($1, [])) }
  | LBRACKET pattern_semi_list RBRACKET { $2 }
  | pattern COLON_COLON pattern { mk (P_constr (mk_cons (), [ $1; $3 ])) }
  | pattern IUIDENT pattern
    { mk (P_constr (mk_global_constructor_ident None (Vuident $2), [ $1; $3 ])) }
  | LBRACE pattern_record_field_list RBRACE { mk (P_record $2) }
  | pattern AS RLIDENT { mk (P_as ($1, Vlident $3)) }
  | LPAREN pattern COMMA pattern_comma_list RPAREN { mk (P_tuple ($2 :: $4)) }
  | LPAREN pattern RPAREN { mk (P_paren $2) }
;

pattern_semi_list:
  | { mk (P_constr (mk_nil (), [])) }
  | pattern
    { mk (P_constr (mk_cons (), [ $1; mk (P_constr (mk_nil (), [])) ])) }
  | pattern SEMI pattern_semi_list
    { mk (P_constr (mk_cons (), [ $1; $3 ])) }
;

pattern_comma_list:
  | pattern { [ $1 ] }
  | pattern COMMA pattern_comma_list { $1 :: $3 }
;

pattern_record_field_list:
  | label_ident EQUAL pattern opt_semi { [ ($1, $3) ] }
  | label_ident EQUAL pattern SEMI pattern_record_field_list { ($1, $3) :: $5 }
;

opt_local:
  | { LF_no_local }
  | LOCAL { LF_local }
;

opt_semi:
  | { () }
  | SEMI { () }
;

opt_doc:
  | { [] }
  | DOCUMENTATION opt_doc { mk_doc_elem $1 :: $2 }
;

following_binding_list:
  | { [] }
  | AND binding following_binding_list { $2 :: $3 }
;

/**** NAMES ****/

label_ident:
  | label_vname { mk_label_ident None $1 }
  | opt_lident SHARP label_vname { mk_label_ident $1 $3 }
;

bound_vname:
  | RLIDENT { Vlident $1 }
  | PLIDENT { Vpident $1 }
  | ILIDENT { Viident $1 }
;

bound_vname_list:
  | bound_vname bound_vname_list
    { $1 :: $2 }
  | bound_vname
    { [ $1 ] }
;

external_value_vname:
  | RLIDENT { Vlident $1 }
  | constructor_vname { $1 }
;

method_vname:
  | bound_vname { $1 }
;

constructor_vname:
  | RUIDENT { Vuident $1 }
  | PUIDENT { Vuident $1 }
  | IUIDENT { Vuident $1 }
  | LRPARENS { Vuident "()" }
  | LRBRACKETS { Vuident "[]" }
;

label_vname:
  | bound_vname { $1 }
;

species_vname:
  | RUIDENT { Vuident $1 }
;

collection_vname:
  | RUIDENT { Vuident $1 }
;

property_vname:
  | RLIDENT { Vlident $1 }
  | RUIDENT { Vuident $1 }
  | PLIDENT { Vpident $1 }
  | ILIDENT { Viident $1 }
  | PUIDENT { Vuident $1 }
  | IUIDENT { Vuident $1 }
;

theorem_vname:
  | property_vname { $1 }
;

type_vname:
  | RLIDENT { Vlident $1 }
;

type_param_vname:
  | RLIDENT { Vlident $1 }
  | QLIDENT { Vqident $1 }
;
