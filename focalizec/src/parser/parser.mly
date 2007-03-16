%{
(* $Id: parser.mly,v 1.21 2007-03-16 10:56:51 weis Exp $ *)

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

let mk_cons_ident () = mk (I_global (Some "basics", "Cons"));;
let mk_nil_ident () = mk (I_global (Some "basics", "Nil"));;
let mk_void_ident () = mk (I_global (Some "basics", "Void"));;

let mk_cons () = mk (E_var (mk_cons_ident ()));;
let mk_nil () = mk (E_var (mk_nil_ident ()));;
let mk_void () = mk (E_var (mk_void_ident ()));;

let mk_local_ident s = mk (I_local s);;
let mk_global_ident s = mk (I_global (None, s));;
let mk_global_constr s1 s2 = mk (I_global (s1, s2));;

let mk_local_var s = mk (E_var (mk_local_ident s));;
let mk_global_var s = mk (E_var (mk_global_ident s));;
let mk_global_constr s1 s2 = mk (E_var (mk_global_constr s1 s2));;

let mk_infix e1 s e2 = mk (E_app (mk_global_var s, [e1; e2]));;

let mk_proof_label (s1, s2) =
  try int_of_string s1, s2 with
  | Failure _ -> assert false;;

%}

%token EOF

%token <string> LIDENT
%token <string> UIDENT
%token <string> PIDENT
%token <string> IIDENT
%token <string> QIDENT
%token <string> INT
%token <string> STRING
%token <string> DOCUMENTATION
%token <string> BOOL
%token <char> CHAR

%token <string * string> PROOF_LABEL

/* Arithmetic operators */
%token <string> BACKSLASH_OP
%token <string> PERCENT_OP
%token <string> PLUS_OP
%token <string> DASH_OP
%token <string> STAR_OP
%token <string> SLASH_OP
%token <string> STAR_STAR_OP
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token COMMA
%token <string> COMMA_OP
%token QUOTE
%token DOUBLEQUOTE
%token DASH_GT
%token <string> DASH_GT_OP
%token LT_DASH_GT
%token <string> LT_DASH_GT_OP
%token SHARP
%token BANG
%token BAR
%token <string> BAR_OP
%token <string> AMPER_OP
%token TILDA_BAR
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
%token <string> AT_OP
%token <string> HAT_OP
%token <string> PREFIX_OP
%token DOT

%token ALL
%token ALIAS
%token AND
%token AS
%token ASSUME
%token ASSUMED
%token BUT
%token BY
%token CAML
%token COLLECTION
%token COQ
%token <string> COQPROOF
%token DECL
%token DEF
%token ELSE
%token END
%token EX
%token EXTERNAL
%token FUNCTION
%token IF
%token IN
%token INHERITS
%token IMPLEMENTS
%token IS
%token LET
%token LETPROP
%token LOCAL
%token MATCH
%token NOT
%token OF
%token OPEN
%token OR
%token PROOF
%token PROVE
%token PROP
%token PROPERTY
%token QED
%token REC
%token REP
%token SELF
%token SIG
%token SPECIES
%token THEN
%token THEOREM
%token TYPE
%token USES
%token VALUE
%token WITH

/* Precedences and associativities. */

%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI SEMI_OP SEMI_SEMI_OP     /* below EQ ({lbl=...; lbl=...}) */
%nonassoc LET                           /* above SEMI ( ...; let ... in ...) */
%nonassoc below_WITH
%nonassoc FUNCTION WITH                 /* below BAR  (match ... with ...) */
%nonassoc LT_DASH_GT LT_DASH_GT_OP      /* <-> */
%right    OR                            /* prop or prop */
%right    AND                           /* above WITH prop and prop */
%nonassoc NOT                           /* not prop */
%nonassoc THEN                          /* below ELSE (if ... then ...) */
%nonassoc ELSE                          /* (if ... then ... else ...) */
%right    BACKSLASH_OP                  /* e \ e */
%right    COLON_OP                      /* expr (e := e := e) */
%nonassoc AS
%right     BAR                          /* Dangling match (match ... with ...) */
%left     COMMA COMMA_OP                /* expr/expr_comma_list (e,e,e) */
%right    DASH_GT DASH_GT_OP            /* core_type2 (t -> t -> t) */
%right    BAR_OP                        /* expr (e || e || e) */
%right    AMPER_OP                      /* expr (e && e && e) */
%nonassoc below_EQ
%left     EQ_OP LT_OP GT_OP             /* expr (e OP e OP e) */
%right    AT_OP HAT_OP                  /* expr (e OP e OP e) */
%right    COLON_COLON COLON_COLON_OP    /* expr (e :: e :: e) */
%left     PLUS_OP DASH_OP               /* expr (e OP e OP e) */
%left     STAR_OP SLASH_OP              /* expr (e OP e OP e) */
%left     PERCENT_OP                    /* expr (e OP e OP e) */
%right    STAR_STAR_OP                  /* expr (e OP e OP e) */
%nonassoc TILDA_BAR PREFIX_OP           /* unary ` ~ ? $ continue_infix* */
%nonassoc prec_unary_minus              /* unary DASH_OP */
%nonassoc prec_constant_constructor     /* cf. simple_expr (C versus C x) */
                                        /* above AS BAR COLON_COLON COMMA */
%nonassoc below_SHARP
%nonassoc SHARP                         /* simple_expr/toplevel_directive */
%nonassoc DOT
%nonassoc below_RPAREN
%nonassoc RPAREN
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc BEGIN CHAR INT
          LBRACE LBRACKET LIDENT LPAREN
          STRING UIDENT

%start main
%type <Parsetree.phrase list> main
%%

main:
  | EOF { [] }
  | phrase main { $1 :: $2 }
;

/* a voir: ajouter les expressions a toplevel ? */
phrase:
  | def_let SEMI_SEMI { mk (Ph_let $1) }
  | def_letprop SEMI_SEMI { mk (Ph_letprop $1) }
  | def_theorem SEMI_SEMI { mk (Ph_theorem $1) }
  | def_type { mk (Ph_type $1) }
  | def_species { mk (Ph_species $1) }
  | def_collection { mk (Ph_coll $1) }
  | EXTERNAL def_external { mk (Ph_external $2) }
  | OPEN STRING SEMI_SEMI { mk (Ph_open $2) }
  | USES STRING SEMI_SEMI { mk (Ph_use $2) } /* USES should be USE */
;

def_external:
  | TYPE LIDENT EQUAL def_external_body SEMI_SEMI
      { mk (ED_type (mk {ed_name = $2; ed_body = $4})) }
  | VALUE value_name EQUAL def_external_body SEMI_SEMI
      { mk (ED_value (mk {ed_name = $2; ed_body = $4})) }

def_external_body:
  | BAR external_language DASH_GT external_name { [($2, $4)] }
  | BAR external_language DASH_GT external_name def_external_body
      { ($2, $4) :: $5 }
;

external_language:
  | CAML { EL_Caml }
  | COQ { EL_Coq }
  | STRING { EL_external $1 }
;

/**** TYPE DEFINITION ****/

def_type:
  | opt_doc TYPE LIDENT def_type_params EQUAL def_type_body SEMI_SEMI
      { mk_doc $1 {td_name = $3; td_params = $4; td_body = $6; } }
;

def_type_params:
  | { [] }
  | LPAREN def_type_param_comma_list RPAREN { $2 }
;

def_type_param_comma_list:
  | LIDENT { [ $1 ] }
  | LIDENT COMMA def_type_param_comma_list { $1 :: $3 }
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
  | constructor_name { ($1, []) }
  | constructor_name LPAREN type_expr_comma_list RPAREN { ($1, $3) }
;
def_constructor_list:
  | BAR def_constructor { [ $2 ] }
  | BAR def_constructor def_constructor_list { $2 :: $3 }
;

def_product:
  | LBRACE def_record_field_list RBRACE { $2 }
;
def_record_field_list:
  | label_name EQUAL type_expr opt_semi { [ ($1, $3) ] }
  | label_name EQUAL type_expr SEMI def_record_field_list { ($1, $3) :: $5 }
;

/**** SPECIES ****/

def_species:
  | opt_doc SPECIES LIDENT def_species_params INHERITS species_expr_list EQUAL species_fields END
    { mk_doc $1 { sd_name = $3; sd_params = $4; sd_inherits = $6; sd_fields = $8; } }
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
  | LIDENT IN species_ident { ($1, mk (SPT_in $3)) }
  | LIDENT IS species_expr { ($1, mk (SPT_is $3)) }
;

species_expr_list:
  | {[]}
  | species_expr species_expr_list { $1 :: $2 }
;

species_expr:
  | species_ident
    { mk_no_doc { se_name = $1; se_params = []; } }
  | species_ident LPAREN species_param_list RPAREN
    { mk_no_doc { se_name = $1; se_params = $3; } }

species_param:
  | coll_ident { mk (SP_coll $1) }
  | expr { mk (SP_entity $1) }
;

species_param_list:
  | species_param { [$1] }
  | species_param COMMA species_param_list { $1 :: $3 }
;

species_fields:
  | species_field { [$1] }
  | species_field species_fields { $1 :: $2 }

species_field :
  | def_rep      { mk (SF_rep $1) }
  | def_sig      { mk (SF_sig $1) }
  | def_let      { mk (SF_let $1) }
  | def_letprop  { mk (SF_letprop $1) }
  | def_property { mk (SF_property $1) }
  | def_theorem  { mk (SF_theorem $1) }
  | def_proof    { mk (SF_proof $1) }
;

def_proof:
  | opt_doc PROOF OF LIDENT EQUAL proof
    { mk_doc $1 { pd_name = mk_local_ident $4; pd_proof = $6; } }
;

def_rep:
  | opt_doc REP rep_type_def
    { mk_doc $1 $3 }
;

rep_type_def:
  | LIDENT { RTE_ident (mk_global_ident $1) }
  /* Fixme incomplete */
;

/**** COLLECTION DEFINITION ****/

def_collection:
  | opt_doc COLLECTION LIDENT IMPLEMENTS species_expr EQUAL END
      { mk_doc $1 { cd_name = $3; cd_body = $5; } }
;

/**** FUNCTION & VALUES DEFINITION ****/

def_let:
  | opt_doc LET binding
       { mk_doc $1 {ld_rec = RF_no_rec; ld_bindings = [$3]} }
  | opt_doc LET REC binding_list
       { mk_doc $1 {ld_rec = RF_rec; ld_bindings = $4} }
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
  | opt_doc SIG LIDENT COLON type_expr
    { mk_doc $1 {sig_name = mk_local_ident $3; sig_type = $5; } }

;

def_letprop:
  | opt_doc LETPROP binding
    { mk_doc $1 {ld_rec = RF_no_rec; ld_bindings = [$3]} }
;

def_property:
  | opt_doc PROPERTY LIDENT EQUAL prop
    { mk_doc $1 {prd_name = mk_local_ident $3; prd_prop = $5; } }
;

def_theorem:
  | opt_doc THEOREM LIDENT COLON prop PROOF COLON proof
      { mk_doc $1 { th_name = mk_local_ident $3; th_stmt = $5; th_proof = $8 } }
;

prop:
  | ALL vname_list opt_in_type_expr COMMA prop
     { mk (P_forall ($2, $3, $5))}
  | EX vname_list opt_in_type_expr COMMA prop
     { mk (P_exists ($2, $3, $5))}
  | prop DASH_GT prop
     { mk (P_imply ($1, $3)) }
  | prop OR prop
     { mk (P_or ($1, $3)) }
  | prop AND prop
     { mk (P_and ($1, $3)) }
  | prop LT_DASH_GT prop
     { mk (P_equiv ($1, $3)) }
  | NOT prop
     { mk (P_not $2) }
  | expr %prec below_RPAREN
     { mk (P_expr $1) }
  | LPAREN prop RPAREN
     { $2 }
;

opt_prop:
  | prop { Some $1 }
  |      { None }

opt_in_type_expr:
  | IN type_expr { Some $2 }
  |              { None }
;

vname_list:
  | LIDENT vname_list { $1 :: $2 }
  | LIDENT            { [$1] }
;

proof:
 | ASSUMED
     { mk (Pf_assumed) }
 | BY fact_list
     { mk (Pf_auto $2) }
 | COQPROOF
     { mk (Pf_coq $1) }
 | proof_node_list
     { mk (Pf_node $1) }
;

fact_list:
 | { [] }
 | fact COMMA fact_list { $1 :: $3 }
;

fact:
 | DEF LIDENT { mk (F_def (mk_local_ident $2)) }
 | prop_ident { mk (F_property ($1)) }
 | PROOF_LABEL { mk (F_node (mk_proof_label $1)) }
;

proof_node_list:
 | proof_node { [$1] }
 | proof_node proof_node_list { $1 :: $2 }
;

proof_node:
 | PROOF_LABEL statement proof
     { mk (PN_sub (mk_proof_label $1, $2, $3)) }
 | LET bound_ident expr
     { mk (PN_let ($2, $3)) }
 | PROOF_LABEL QED proof
     { mk (PN_qed (mk_proof_label $1, $3)) }
;

statement:
 | hyp_list opt_prop
     { mk { s_hyps = $1; s_concl = $2; } }
;

hyp:
 | ASSUME LIDENT IN type_expr
     { mk (H_var ($2, $4)) }
 | ASSUME LIDENT COLON prop
     { mk (H_hyp ($2, $4)) }
;

hyp_list:
 | { [] }
 | hyp hyp_list { $1 :: $2 }
;

/**** TYPE EXPRESSIONS ****/

type_expr:
  | SELF
     { mk TE_self }
  | PROP
     { mk TE_prop }
  | QIDENT
     { mk (TE_ident (mk_local_ident $1)) }
  | glob_ident
     { mk (TE_ident $1) }
  | LIDENT
     { mk (TE_ident (mk (I_method (Some $1, "self")))) }
  | type_expr DASH_GT type_expr
     { mk (TE_fun ($1, $3)) }
  | type_expr STAR_OP type_expr
     { mk (TE_prod ($1, $3)) }
  | glob_ident LPAREN type_expr_comma_list RPAREN
     { mk (TE_app ($1, $3)) }
  | LPAREN type_expr RPAREN
     { $2 }
;

type_expr_comma_list:
  | type_expr COMMA type_expr_comma_list { $1 :: $3 }
  | type_expr { [$1] }
;

constructor_ref:
  | opt_lident SHARP UIDENT
     { mk (I_global ($1, $3)) }
;

/**** EXPRESSIONS ****/

glob_ident:
  | opt_lident SHARP LIDENT
     { mk (I_global ($1, $3)) }
;

opt_lident:
  | LIDENT { Some $1 }
  |        { None }
;

expr:
  | constant
     { mk (E_const $1) }
  | PREFIX_OP expr
     { mk (E_app (mk_local_var $1, [ $2 ])) }
  | FUNCTION vname_list DASH_GT expr
     { mk (E_fun ($2, $4)) }
  | expr_ident
     { mk (E_var $1) }
  | expr DOT label_name
     { mk (E_record_access ($1, $3)) }
  | opt_lident SHARP UIDENT %prec prec_constant_constructor
     { mk (E_constr (mk_global_constr $1 $3, [])) }
  | opt_lident SHARP UIDENT LPAREN expr_comma_list RPAREN
     { mk (E_constr (mk_global_constr $1 $3, $5)) }
  | expr LPAREN expr_comma_list RPAREN
     { mk (E_app ($1, $3)) }
  | MATCH expr WITH clause_list
     { mk (E_match ($2, $4)) }
  | IF expr THEN expr ELSE expr
     { mk (E_if ($2, $4, $6)) }
  | def_let IN expr
     { mk (E_let ($1, $3)) }
  | LBRACE record_field_list RBRACE
     { mk (E_record $2) }
  | LBRACE expr WITH record_field_list RBRACE
     { mk (E_record_with ($2, $4)) }
  | LBRACKET expr_semi_list RBRACKET { $2 }
  | expr COLON_COLON expr { mk (E_app (mk_cons (), [$1; $3])) }
  | LPAREN expr COMMA expr_comma_list RPAREN { mk (E_tuple ($2 :: $4)) }
  | expr COMMA_OP expr
      { mk_infix $1 $2 $3 }
  | expr HAT_OP expr
      { mk_infix $1 $2 $3 }
  | expr AT_OP expr
      { mk_infix $1 $2 $3 }
  | expr SEMI_OP expr
      { mk_infix $1 $2 $3 }
  | expr SEMI_SEMI_OP expr
      { mk_infix $1 $2 $3 }
  | expr COLON_COLON_OP expr
      { mk_infix $1 $2 $3 }
  | expr COLON_OP expr
      { mk_infix $1 $2 $3 }
  | expr PLUS_OP expr
      { mk_infix $1 $2 $3 }
  | expr DASH_OP expr
      { mk_infix $1 $2 $3 }
  | expr DASH_GT_OP expr
      { mk_infix $1 $2 $3 }
  | expr STAR_OP expr
      { mk_infix $1 $2 $3 }
  | expr SLASH_OP expr
      { mk_infix $1 $2 $3 }
  | expr PERCENT_OP expr
      { mk_infix $1 $2 $3 }
  | expr STAR_STAR_OP expr
      { mk_infix $1 $2 $3 }
  | expr BACKSLASH_OP expr
      { mk_infix $1 $2 $3 }
  | expr EQ_OP expr
      { mk_infix $1 $2 $3 }
  | expr LT_OP expr
      { mk_infix $1 $2 $3 }
  | expr LT_DASH_GT_OP expr
      { mk_infix $1 $2 $3 }
  | expr GT_OP expr
      { mk_infix $1 $2 $3 }
  | expr BAR_OP expr
      { mk_infix $1 $2 $3 }
  | expr AMPER_OP expr
      { mk_infix $1 $2 $3 }
  | TILDA_BAR expr
      { mk (E_app (mk_local_var "~|", [$2])) }
  | DASH_OP expr %prec prec_unary_minus
      { mk (E_app (mk_local_var $1, [$2])) }
  | LPAREN expr RPAREN
     { $2 }
  | LPAREN RPAREN { mk (E_constr (mk_void (), [])) }
;

expr_semi_list:
  | { mk (E_app (mk_nil (), [])) }
  | expr { mk (E_app (mk_cons (), [$1; mk (E_app (mk_nil (), []))])) }
  | expr SEMI expr_semi_list { mk (E_app (mk_cons (), [$1; $3])) }
;
expr_comma_list:
  | expr { [ $1 ] }
  | expr COMMA expr_comma_list { $1 :: $3 }
;

record_field_list:
  | label_name EQUAL expr opt_semi { [ ($1, $3) ] }
  | label_name EQUAL expr SEMI record_field_list { ($1, $3) :: $5 }
;

bound_ident:
  | LIDENT { mk_local_ident $1 }
  | PIDENT { mk_local_ident $1 }
  | IIDENT { mk_local_ident $1 }
;

expr_ident:
  | glob_ident { $1 }
  | opt_lident BANG LIDENT
     { mk (I_method ($1, $3)) }
  | bound_ident { $1 }
;

prop_ident:
  | expr_ident { $1 }

species_ident:
  | glob_ident { $1 }
  | opt_lident BANG LIDENT
     { mk (I_method ($1, $3)) }
  | LIDENT { mk (I_local $1) }
;

coll_ident:
  | species_ident { $1 }
;

clause_list:
  | BAR clause { [$2] }
  | BAR clause clause_list { $2 :: $3 }
;

clause:
  | pattern DASH_GT expr { ($1, $3) }
;

constant:
  | INT { mk (C_int $1) }
  | BOOL { mk (C_bool $1) }
  | STRING { mk (C_string $1) }
  | CHAR { mk (C_char $1) }
;

pattern:
  | constant { mk (P_const $1) }
  | LIDENT { mk (P_var $1) }
  | UNDERSCORE { mk (P_wild) }
  | constructor_ref LPAREN pattern_comma_list RPAREN { mk (P_app ($1, $3)) }
  | constructor_ref { mk (P_app ($1, [])) }
  | LBRACKET pattern_semi_list RBRACKET { $2 }
  | pattern COLON_COLON pattern { mk (P_app (mk_cons_ident (), [$1; $3])) }
  | LBRACE pattern_record_field_list RBRACE { mk (P_record $2) }
  | pattern AS LIDENT { mk (P_as ($1, $3)) }
  | LPAREN pattern COMMA pattern_comma_list RPAREN { mk (P_tuple ($2 :: $4)) }
  | LPAREN pattern RPAREN { $2 }
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

label_name:
  | LIDENT { $1 }

external_name:
  | STRING { $1 }
;

value_name:
  | LIDENT { $1 }
  | UIDENT { $1 }
  | PIDENT { $1 }
  | IIDENT { $1 }
;
constructor_name:
  | UIDENT { $1 }
  | PIDENT { $1 }
  | IIDENT { $1 }
;
