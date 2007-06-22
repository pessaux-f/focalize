%{
(* $Id: parser.mly,v 1.36 2007-06-21 23:27:27 weis Exp $ *)

open Parsetree;;

let mk_doc doc d = {
  ast_loc = {
    l_beg = Parsing.symbol_start_pos ();
    l_end = Parsing.symbol_end_pos ();
  };
  ast_desc = d;
  ast_doc = doc;
};;

let mk d = mk_doc None d;;
let mk_no_doc d = mk_doc None d;;

let mk_local_ident s = mk (I_local s);;
let mk_global_ident s = mk (I_global (None, s));;
let mk_global_constr s1 s2 = mk (I_global (s1, s2));;

let mk_local_var s = mk (E_var (mk_local_ident s));;
let mk_global_var s = mk (E_var (mk_global_ident s));;
let mk_global_constr_var s1 s2 = mk (E_var (mk_global_constr s1 s2));;

let mk_infix_application e1 s e2 =
  mk (E_app (mk_global_var (Viident s), [e1; e2]));;
let mk_prefix_application s e1 =
  mk (E_app (mk_local_var (Vpident s), [e1]));;

let mk_method_application fname s = mk (I_method (fname, s));;

let mk_cons_ident () = mk_global_constr (Some "basics") (Vuident "Cons");;
let mk_nil_ident () = mk_global_constr (Some "basics") (Vuident "Nil");;
let mk_void_ident () = mk_global_constr (Some "basics") (Vuident "Void");;

let mk_cons () = mk (E_var (mk_cons_ident ()));;
let mk_nil () = mk (E_var (mk_nil_ident ()));;
let mk_void () = mk (E_var (mk_void_ident ()));;

let mk_proof_label (s1, s2) =
  try int_of_string s1, s2 with
  | Failure _ -> assert false;;

%}

%token EOF

/* Identifiers */
%token <string> LIDENT /* Lower case ident (e.g. x, _1, or _xY) */
%token <string> UIDENT /* Upper case ident (e.g. A, _B, or _Ax) */
%token <string> PIDENT /* Prefix ident  (e.g. ( ! ) ) */
%token <string> IIDENT /* Infix ident  (e.g ( + ) */
%token <string> QIDENT /* Quoted lower case ident (e.g. 'a ) */

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
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET

/* General infix and prefix operators */
%token COMMA
%token <string> COMMA_OP

%token DASH_GT
%token <string> DASH_GT_OP
%token <string> LT_DASH_OP
%token LT_DASH_GT
%token <string> LT_DASH_GT_OP
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
%token IMPLEMENTS
%token IS
%token LET
%token LOCAL
%token LOGICAL
%token MATCH
%token NOT
%token NOTATION
%token OF
%token OPEN
%token OR
%token PROOF
%token PROP
%token PROPERTY
%token PROVE
%token QED
%token REC
%token REP
%token SELF
%token SIG
%token SPECIES
%token STEP
%token THEN
%token THEOREM
%token TYPE
%token USE
%token VALUE
%token WITH

/* Precedences and associativities. */

%nonassoc IN
/* %nonassoc below_SEMI */
/* %nonassoc SEMI */
%nonassoc SEMI_OP SEMI_SEMI_OP    /* below EQ ({lbl=...; lbl=...}) */
/* %nonassoc LET */                    /* above SEMI ( ...; let ... in ...) */
/* %nonassoc below_WITH */
/* %nonassoc FUNCTION WITH */          /* below BAR  (match ... with ...) */
%nonassoc LT_DASH_GT LT_DASH_GT_OP     /* <-> */
%right    OR                           /* prop or prop */
%right    AND                          /* above WITH prop and prop */
%nonassoc NOT                          /* not prop */
/* %nonassoc THEN */                   /* below ELSE (if ... then ...) */
%nonassoc ELSE                         /* (if ... then ... else ...) */
%right    BACKSLASH_OP                 /* e \ e */
%nonassoc LT_DASH_OP                   /* below COLON_OP */
%right    COLON_OP                     /* expr (e := e := e) */
%nonassoc AS
%right    BAR                          /* Dangling match (match ... with ...) */
%left     COMMA COMMA_OP               /* expr/expr_comma_list (e,e,e) */
%right    DASH_GT DASH_GT_OP           /* core_type2 (t -> t -> t) */
%right    BAR_OP                       /* expr (e || e || e) */
%right    AMPER_OP                     /* expr (e && e && e) */
/* %nonassoc below_EQ */
%left     EQUAL EQ_OP LT_OP GT_OP      /* expr (e OP e OP e) */
%right    AT_OP HAT_OP                 /* expr (e OP e OP e) */
%right    COLON_COLON COLON_COLON_OP   /* expr (e :: e :: e) */
%left     PLUS_OP DASH_OP              /* expr (e OP e OP e) */
%left     STAR_OP SLASH_OP             /* expr (e OP e OP e) */
%left     PERCENT_OP                   /* expr (e OP e OP e) */
%right    STAR_STAR_OP                 /* expr (e OP e OP e) */
%nonassoc BACKQUOTE_OP                 /* unary ` ~ ? $ ! continue_infix* */
%nonassoc TILDA_OP                     /* ~| expr */
%nonassoc QUESTION_OP
%nonassoc DOLLAR_OP
%nonassoc BANG_OP
%nonassoc prec_unary_minus             /* unary DASH_OP */
%nonassoc prec_constant_constructor    /* cf. simple_expr (C versus C x) */
                                       /* above AS BAR COLON_COLON COMMA */
%nonassoc below_SHARP
%nonassoc SHARP_OP               /* simple_expr */
%nonassoc DOT
%nonassoc below_RPAREN
%nonassoc RPAREN
/* Finally, the first tokens of simple_expr are above everything else. */
/* %nonassoc BEGIN INT FLOAT BOOL STRING CHAR */
/*          LBRACE LBRACKET LIDENT UIDENT */
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

/* a voir: ajouter les expressions a toplevel ? */
phrase:
  | def_let SEMI_SEMI { mk (Ph_let $1) }
  | def_logical SEMI_SEMI { mk (Ph_let $1) }
  | def_theorem SEMI_SEMI { mk (Ph_theorem $1) }
  | def_type SEMI_SEMI { mk (Ph_type $1) }
  | def_species SEMI_SEMI { mk (Ph_species $1) }
  | def_collection SEMI_SEMI { mk (Ph_coll $1) }
  | opt_doc EXTERNAL def_external SEMI_SEMI { mk_doc $1 (Ph_external $3) }
  | opt_doc OPEN STRING SEMI_SEMI { mk_doc $1 (Ph_open $3) }
  | opt_doc USE STRING SEMI_SEMI { mk_doc $1 (Ph_use $3) }
  | opt_doc expr SEMI_SEMI { mk_doc $1 (Ph_expr $2) }
;

def_external:
  | TYPE external_type_name EQUAL external_definition SEMI_SEMI
    { mk (ED_type (mk {ed_name = Vlident $2; ed_body = mk $4})) }
  | VALUE external_value_vname EQUAL external_definition SEMI_SEMI
    { mk (ED_value (mk {ed_name = $2; ed_body = mk $4})) }
;

external_language:
  | CAML { EL_Caml }
  | COQ { EL_Coq }
  | STRING { EL_external $1 }
;

external_code:
  | STRING { $1 }
;

external_def_one:
  | BAR external_language DASH_GT external_code { ($2, $4) }
;

external_definition:
  | external_def_one { [ $1 ] }
  | external_def_one external_definition { $1 :: $2 }
;

/**** TYPE DEFINITION ****/

def_type:
  | opt_doc TYPE type_name def_type_params EQUAL def_type_body SEMI_SEMI
    { mk_doc $1 {td_name = ($3 : string); td_params = $4; td_body = $6; } }
;

def_type_params:
  | { [] }
  | LPAREN def_type_param_comma_list RPAREN { $2 }
;

def_type_param_comma_list:
  | type_param_name { [ $1 ] }
  | type_param_name COMMA def_type_param_comma_list { $1 :: $3 }
;

def_type_body:
  | ALIAS type_expr { mk (TD_alias $2) }
  | def_sum { mk (TD_union $1) }
  | def_product { mk (TD_record $1) }
;

def_sum:
  | def_constructor_list { $1 }
/* ajouter les types a constructeurs prives ? */
/*  | PRIVATE def_constructor_list */
;

def_constructor:
  | constructor_vname { ($1, []) }
  | constructor_vname LPAREN type_expr_comma_list RPAREN { ($1, $3) }
;
def_constructor_list:
  | BAR def_constructor { [ $2 ] }
  | BAR def_constructor def_constructor_list { $2 :: $3 }
;

def_product:
  | LBRACE def_record_field_list RBRACE { $2 }
;
def_record_field_list:
  | label_name EQUAL type_expr opt_semi
    { [ ($1, $3) ] }
  | label_name EQUAL type_expr SEMI def_record_field_list
    { ($1, $3) :: $5 }
;

/**** SPECIES ****/

def_species:
  | opt_doc SPECIES species_name def_species_params
            def_species_inherits
            EQUAL species_fields END
    { mk_doc $1
        { sd_name = $3; sd_params = $4;
          sd_inherits = $5; sd_fields = $7; } }
;

def_species_params:
  | { [] }
  | LPAREN def_species_param_list RPAREN { $2 }
;

def_species_param_list:
  | def_species_param { [$1] }
  | def_species_param COMMA def_species_param_list { $1 :: $3 }
;

def_species_param:
  | bound_vname IN carrier_ident { ($1, mk (SPT_in $3)) }
  | collection_vname IS species_expr { ($1, mk (SPT_is $3)) }
;

def_species_inherits:
  | opt_doc INHERITS species_expr_list { mk_doc $1 $3}
  | { mk_no_doc [] }

species_expr_list:
  | species_expr {[ $1 ]}
  | species_expr COMMA species_expr_list { $1 :: $3 }
;

species_expr:
  | species_ident
    { mk_no_doc { se_name = $1; se_params = []; } }
  | species_ident LPAREN species_param_list RPAREN
    { mk_no_doc { se_name = $1; se_params = $3; } }

species_param:
  | expr { mk (SP $1) }
;

species_param_list:
  | species_param { [$1] }
  | species_param COMMA species_param_list { $1 :: $3 }
;

species_fields:
  | { [] }
  | species_field SEMI species_fields { $1 :: $3 }

species_field :
  | def_rep      { mk (SF_rep $1) }
  | def_sig      { mk (SF_sig $1) }
  | def_let      { mk (SF_let $1) }
  | def_logical  { mk (SF_let $1) }
  | def_property { mk (SF_property $1) }
  | def_theorem  { mk (SF_theorem $1) }
  | def_proof    { mk (SF_proof $1) }
;

def_proof:
  | opt_doc PROOF OF property_vname EQUAL proof
    { mk_doc $1 { pd_name = mk_local_ident $4; pd_proof = $6; } }
;

def_rep:
  | opt_doc REP EQUAL rep_type_def
    { mk_doc $1 $4 }
;

rep_type_def:
  | glob_ident
    { RTE_ident $1 }
  | LIDENT { RTE_ident (mk_global_ident (Vlident $1)) }
  | rep_type_def DASH_GT rep_type_def
    { RTE_fun (mk $1, mk $3) }
  | rep_type_def STAR_OP rep_type_def
    { RTE_prod (mk $1, mk $3) }
  | glob_ident LPAREN rep_type_def_comma_list RPAREN
    { RTE_app ($1, $3) }
  | LPAREN rep_type_def RPAREN
    { RTE_paren (mk $2) }
;

rep_type_def_comma_list:
  | rep_type_def { [ mk $1 ] }
  | rep_type_def COMMA rep_type_def_comma_list { mk $1 :: $3 }
;

/**** COLLECTION DEFINITION ****/

def_collection:
  | opt_doc COLLECTION collection_name IMPLEMENTS species_expr EQUAL END
    { mk_doc $1 { cd_name = $3; cd_body = $5; } }
;

/**** FUNCTION & VALUES DEFINITION ****/

let_binding:
  | opt_local LET binding
    { mk {ld_rec = RF_no_rec; ld_log = LF_no_log; ld_loc = $1;
          ld_bindings = [ $3 ]} }
  | opt_local LET REC binding_list
    { mk { ld_rec = RF_rec; ld_log = LF_no_log; ld_loc = $1;
           ld_bindings = $4; } }
;

def_let:
  | opt_doc let_binding { mk_doc $1 $2.ast_desc }
;

binding:
  | bound_ident EQUAL expr
    { mk {b_name = $1; b_params = []; b_type = None; b_body = $3} }
  | bound_ident IN type_expr EQUAL expr
    { mk {b_name = $1; b_params = []; b_type = Some $3; b_body = $5} }
  | bound_ident LPAREN param_list RPAREN EQUAL expr
    { mk {b_name = $1; b_params = $3; b_type = None; b_body = $6} }
  | bound_ident LPAREN param_list RPAREN IN type_expr EQUAL expr
    { mk {b_name = $1; b_params = $3; b_type = Some $6; b_body = $8} }
;

param_list:
  | param { [$1] }
  | param COMMA param_list { $1 :: $3 }
;

param:
  | bound_ident { ($1, None) }
  | bound_ident IN type_expr { ( $1, Some $3) }
;

/**** PROPERTIES & THEOREM DEFINITION ****/

def_sig:
  | opt_doc SIG bound_vname COLON type_expr
    { mk_doc $1 {sig_name = mk_local_ident $3; sig_type = $5; } }
;

def_logical:
  | opt_doc opt_local LOGICAL binding
    { mk_doc $1 {ld_rec = RF_no_rec; ld_log = LF_log; ld_loc = $2;
                 ld_bindings = [ $4 ]} }
  | opt_doc opt_local LOGICAL REC binding_list
    { mk_doc $1 { ld_rec = RF_rec; ld_log = LF_log; ld_loc = $2;
                  ld_bindings = $5; } }
;

def_property:
  | opt_doc PROPERTY property_vname COLON prop
    { mk_doc $1 {prd_name = mk_local_ident $3; prd_prop = $5; } }
;

def_theorem:
  | opt_doc opt_local THEOREM theorem_vname COLON prop PROOF COLON proof
    { mk_doc $1
        { th_name = mk_local_ident $4; th_loc = $2;
          th_stmt = $6; th_proof = $9 } }
;

prop:
  | ALL bound_vname_list opt_in_type_expr COMMA prop
    { mk (Pr_forall ($2, $3, $5))}
  | EX bound_vname_list opt_in_type_expr COMMA prop
    { mk (Pr_exists ($2, $3, $5))}
  | NOT prop
    { mk (Pr_not $2) }
  | LPAREN prop RPAREN
    { mk (Pr_paren $2) }
  | prop DASH_GT prop
    { mk (Pr_imply ($1, $3)) }
  | prop OR prop
    { mk (Pr_or ($1, $3)) }
  | prop AND prop
    { mk (Pr_and ($1, $3)) }
  | prop LT_DASH_GT prop
    { mk (Pr_equiv ($1, $3)) }
  | expr %prec below_RPAREN
    { mk (Pr_expr $1) }
;

opt_in_type_expr:
  | IN type_expr
    { Some $2 }
  | { None }
;

/**** PROOFS ****/

proof:
  | opt_doc ASSUMED
    { mk_doc $1 (Pf_assumed) }
  | opt_doc BY fact_list
    { mk_doc $1 (Pf_auto $3) }
  | opt_doc COQ PROOF EXTERNAL_CODE
    { mk_doc $1 (Pf_coq $4) }
  | proof_node_list
    { mk (Pf_node $1) }
  | DOT { mk (Pf_auto []) }
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
  | { [ ] }
  | fact fact_list { $1 :: $2 }
;

fact:
  | DEFINITION OF species_ident_comma_list { mk (F_def $3) }
  | HYPOTHESIS proof_hyp_list { mk (F_hypothesis $2) }
  | PROPERTY prop_ident_comma_list { mk (F_property ($2)) }
  | THEOREM prop_ident_comma_list { mk (F_property ($2)) }
  | STEP proof_label_comma_list { mk (F_node (List.map mk_proof_label $2)) }
;

proof_hyp:
  | UIDENT { Vuident $1 }
  | LIDENT { Vlident $1 }
;

proof_hyp_list:
  | proof_hyp COMMA proof_hyp_list { $1 :: $3 }
  | proof_hyp { [ $1 ] }
; 

opt_prop:
  | { None }
  | PROVE prop
    { Some $2 }
;

statement:
  | hyp_list opt_prop
    { mk { s_hyps = $1; s_concl = $2; } }
;

hyp:
  | ASSUME bound_vname IN type_expr { mk (H_var ($2, $4)) }
  | ASSUME proof_hyp COLON prop { mk (H_hyp ($2, $4)) }
  | NOTATION proof_hyp EQUAL expr { mk (H_not ($2, $4)) }
;

hyp_list:
  | { [] }
  | hyp COMMA hyp_list { $1 :: $3 }
;

/**** TYPE EXPRESSIONS ****/

type_expr:
  | SELF
    { mk TE_self }
  | PROP
    { mk TE_prop }
  | QIDENT
    { mk (TE_ident (mk_local_ident (Vqident $1))) }
  | glob_ident
    { mk (TE_ident $1) }
  | LIDENT
    { mk (TE_ident (mk_method_application (Some $1) (Vlident "self"))) }
  | type_expr DASH_GT type_expr
    { mk (TE_fun ($1, $3)) }
  | type_expr STAR_OP type_expr
    { mk (TE_prod ($1, $3)) }
  | glob_ident LPAREN type_expr_comma_list RPAREN
    { mk (TE_app ($1, $3)) }
  | LPAREN type_expr RPAREN
    { mk (TE_paren $2) }
;

type_expr_comma_list:
  | type_expr COMMA type_expr_comma_list { $1 :: $3 }
  | type_expr { [$1] }
;

constructor_ref:
  | opt_lident SHARP constructor_vname
    { mk (I_global ($1, $3)) }
;

/**** EXPRESSIONS ****/

glob_ident:
  | opt_lident SHARP bound_vname
    { mk (I_global ($1, $3)) }
;

opt_lident:
  | { None }
  | SELF
    { Some "self" }
  | LIDENT
    { Some $1 }
;

simple_expr:
  | constant
    { mk (E_const $1) }
  | expr_ident
    { mk (E_var $1) }
  | opt_lident SHARP UIDENT %prec prec_constant_constructor
    { mk (E_constr (mk_global_constr_var $1 (Vuident $3), [])) }
  | opt_lident SHARP UIDENT LPAREN expr_comma_list RPAREN
    { mk (E_constr (mk_global_constr_var $1 (Vuident $3), $5)) }
  | simple_expr DOT label_name
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
  | LPAREN RPAREN
    { mk (E_constr (mk_void (), [])) }
;

expr:
  | simple_expr %prec below_SHARP
    { $1 }
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
  | expr COLON_COLON expr
    { mk (E_app (mk_cons (), [$1; $3])) }
  | expr COMMA_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr HAT_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr AT_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr SEMI_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr SEMI_SEMI_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr COLON_COLON_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr COLON_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr PLUS_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr DASH_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr DASH_GT_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr STAR_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr SLASH_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr PERCENT_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr STAR_STAR_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr BACKSLASH_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr EQ_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr EQUAL expr
    { mk_infix_application $1 "=" $3 }
  | expr LT_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr LT_DASH_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr LT_DASH_GT_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr GT_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr BAR_OP expr
    { mk_infix_application $1 $2 $3 }
  | expr AMPER_OP expr
    { mk_infix_application $1 $2 $3 }
  | BACKQUOTE_OP expr
    { mk_prefix_application $1 $2 }
  | TILDA_OP expr
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
  | EXTERNAL external_definition END
    { mk (E_external (mk $2)) }
;

expr_semi_list:
  | { mk (E_app (mk_nil (), [])) }
  | expr
    { mk (E_app (mk_cons (), [$1; mk (E_app (mk_nil (), []))])) }
  | expr SEMI expr_semi_list
    { mk (E_app (mk_cons (), [$1; $3])) }
;

expr_comma_list:
  | expr
    { [ $1 ] }
  | expr COMMA expr_comma_list
    { $1 :: $3 }
;

record_field_list:
  | label_name EQUAL expr opt_semi
    { [ ($1, $3) ] }
  | label_name EQUAL expr SEMI record_field_list
    { ($1, $3) :: $5 }
;

expr_ident:
  | glob_ident
    { $1 }
  | opt_lident BANG method_vname
    { mk_method_application $1 $3 }
  | bound_ident
    { $1 }
;

prop_ident:
  | expr_ident
    { $1 }
;

carrier_ident :
  | species_ident { $1 }
;

species_ident:
  | species_vname
    { mk_local_ident $1 }
  | glob_ident
    { $1 }
  | opt_collection_name BANG method_vname
    { mk_method_application $1 $3 }
;

opt_collection_name:
  | { None }
  | collection_name { Some $1 }
;


species_ident_comma_list:
  | species_ident COMMA species_ident_comma_list { $1 :: $3 }
  | species_ident { [$1] }
;

prop_ident_comma_list:
  | prop_ident COMMA prop_ident_comma_list { $1 :: $3 }
  | prop_ident { [$1] }
;

proof_label_comma_list:
  | PROOF_LABEL COMMA proof_label_comma_list { $1 :: $3 }
  | PROOF_LABEL { [$1] }
;

clause_list:
  | BAR clause
    { [$2] }
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
  | LIDENT { mk (P_var (Vlident $1)) }
  | UNDERSCORE { mk (P_wild) }
  | constructor_ref LPAREN pattern_comma_list RPAREN { mk (P_app ($1, $3)) }
  | constructor_ref { mk (P_app ($1, [])) }
  | LBRACKET pattern_semi_list RBRACKET { $2 }
  | pattern COLON_COLON pattern { mk (P_app (mk_cons_ident (), [$1; $3])) }
  | LBRACE pattern_record_field_list RBRACE { mk (P_record $2) }
  | pattern AS LIDENT { mk (P_as ($1, Vlident $3)) }
  | LPAREN pattern COMMA pattern_comma_list RPAREN { mk (P_tuple ($2 :: $4)) }
  | LPAREN pattern RPAREN { mk (P_paren $2) }
  | LPAREN RPAREN { mk (P_app (mk_void_ident (), [])) }
;

pattern_semi_list:
  | { mk (P_app (mk_nil_ident (), [])) }
  | pattern
    { mk (P_app (mk_cons_ident (), [$1; mk (P_app (mk_nil_ident (), []))])) }
  | pattern SEMI pattern_semi_list
    { mk (P_app (mk_cons_ident (), [$1; $3])) }
;

pattern_comma_list:
  | pattern { [ $1 ] }
  | pattern COMMA pattern_comma_list { $1 :: $3 }
;

pattern_record_field_list:
  | label_name EQUAL pattern opt_semi { [ ($1, $3) ] }
  | label_name EQUAL pattern SEMI pattern_record_field_list { ($1, $3) :: $5 }
;

opt_local:
  | { LF_no_loc }
  | LOCAL { LF_loc }
;

opt_semi:
  | { () }
  | SEMI { () }
;

opt_doc:
  | { None }
  | DOCUMENTATION { Some $1 }
;

binding_list:
  | { [] }
  | binding AND binding_list { $1 :: $3 }
;

/**** NAMES ****/

bound_ident:
  | bound_vname { mk_local_ident $1 }
;

bound_vname_list:
  | bound_vname bound_vname_list
    { $1 :: $2 }
  | bound_vname
    { [$1] }
;

bound_vname:
  | LIDENT { Vlident $1 }
  | PIDENT { Vpident $1 }
  | IIDENT { Viident $1 }
;

external_value_vname:
  | LIDENT { Vlident $1 }
  | UIDENT { Vuident $1 }
  | PIDENT { Vpident $1 }
  | IIDENT { Viident $1 }
;

method_vname:
  | bound_vname { $1 }
;

constructor_vname:
  | UIDENT { Vuident $1 }
  | PIDENT { Vpident $1 }
  | IIDENT { Viident $1 }
;

species_name:
  | LIDENT { $1 }
  | UIDENT { $1 }
;

species_vname:
  | LIDENT { Vlident $1 }
  | UIDENT { Vuident $1 }
;

collection_name:
  | UIDENT { $1 }
  | LIDENT { $1 }
;

collection_vname:
  | LIDENT { Vlident $1 }
  | UIDENT { Vuident $1 }
;

property_vname:
  | LIDENT { Vlident $1 }
  | UIDENT { Vuident $1 }
;

theorem_vname:
  | LIDENT { Vlident $1 }
  | UIDENT { Vlident $1 }
;

label_name:
  | LIDENT { $1 }
;

type_name:
  | LIDENT { $1 }
;

external_type_name:
  | LIDENT { $1 }
;

type_param_name:
  | LIDENT { $1 }
;
