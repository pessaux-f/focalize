%{
(* $Id: parser.mly,v 1.7 2007-02-01 20:51:55 weis Exp $ *)

open Parsetree;;

let mk d = {
  ast_loc = {
    l_beg = Parsing.symbol_start_pos ();
    l_end = Parsing.symbol_end_pos ();
  };
  ast_desc = d;
};;

let mk_cons () = mk (CR_global (Some "basics", "Cons"));;
let mk_nil () = mk (CR_global (Some "basics", "Nil"));;
let mk_void () = mk (CR_global (Some "basics", "Void"));;

let mk_infix e1 s e2 = E_app (mk (CR_GLOBAL (None, s), [e1; e2]));;

%}

%token EOF

%token <string> LIDENT
%token <string> UIDENT
%token <string> PIDENT
%token <string> IIDENT
%token <string> INT
%token <string> STRING
%token <string> BOOL

/* Arithmetic operators */
%token <string> BACKSLASH_OP
%token <string> PERCENT_OP
%token <string> PLUS_OP
%token DASH
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
%token QUOTE
%token DOUBLEQUOTE
%token DASH_GT
%token <string> DASH_GT_OP
%token <string> LT_DASH_GT_OP
%token SHARP
%token BANG
%token BAR
%token <string> BAR_OP
%token <string> BAR_BAR_OP
%token <string> AMPER_OP
%token UNDERSCORE
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
%token AT
%token <string> AT_OP
%token HAT
%token <string> HAT_OP
%token DOT

%token ALL
%token ALIAS
%token AND
%token AS
%token ASSUMED
%token BUT
%token BY
%token CAML
%token COLLECTION
%token COQ
%token DECL
%token DEF
%token ELSE
%token END
%token EX
%token EXTERNAL
%token FUN
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

%token COQPROOF

/* Precedences and associativities. */

%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI                          /* below EQ ({lbl=...; lbl=...}) */
%nonassoc LET                           /* above SEMI ( ...; let ... in ...) */
%nonassoc below_WITH
%nonassoc FUNCTION WITH                 /* below BAR  (match ... with ...) */
%nonassoc LT_DASH_GT LT_DASH_GT_OP      /* <-> */
%nonassoc AND                           /* above WITH */
%nonassoc THEN                          /* below ELSE (if ... then ...) */
%nonassoc ELSE                          /* (if ... then ... else ...) */
%right    COLON_EQ                      /* expr (e := e := e) */
%nonassoc AS
%left     BAR                           /* pattern (p|p|p) */
%nonassoc below_COMMA
%left     COMMA                         /* expr/expr_comma_list (e,e,e) */
%nonassoc NOT
%right    DASH_GT DASH_GT_OP            /* core_type2 (t -> t -> t) */
%right    BAR_OP                        /* expr (e || e || e) */
%right    AMPER_OP                      /* expr (e && e && e) */
%nonassoc below_EQ
%left     EQ_OP LT_OP GT_OP             /* expr (e OP e OP e) */
%right    AT_OP HAT_OP                  /* expr (e OP e OP e) */
%right    COLON_COLON_OP                /* expr (e :: e :: e) */
%left     PLUS_OP DASH_OP               /* expr (e OP e OP e) */
%left     STAR_OP SLASH_OP              /* expr (e OP e OP e) */
%right    STAR_STAR_OP                  /* expr (e OP e OP e) */
%nonassoc PREFIX_OP                     /* unary - ` ~ ? $ */
%nonassoc prec_constant_constructor     /* cf. simple_expr (C versus C x) */
%nonassoc prec_constructor_apply        /* above AS BAR COLON_COLON COMMA */
%nonassoc below_SHARP
%nonassoc SHARP                         /* simple_expr/toplevel_directive */
%nonassoc below_DOT
%nonassoc DOT
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc BEGIN CHAR FALSE INT
          LBRACE LBRACKET LIDENT LPAREN
          STRING TRUE UIDENT

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
  | species { mk (Ph_species $1) }
  | collection { mk (Ph_coll $1) }
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
  | BAR external_language DASH_GT external_name { [($2, $4)]}
  | BAR external_language DASH_GT external_name def_external_body { ($2, $4) :: $5}
;

external_language:
  | CAML { EL_Caml}
  | COQ { EL_Coq}
  | STRING { EL_external $1 }
;

/**** TYPE DEFINITION ****/
def_type:
  | TYPE LIDENT def_type_params EQUAL def_type_body SEMI_SEMI
      { mk {td_name = $2; td_params = $3; td_body = $5; } }
;

def_type_params:
  | { [] }
  | LPAREN def_type_params RPAREN { $2 }
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
  | constructor_name { ($1, []) ] }
  | constructor_name LPAREN type_expr_comma_list RPAREN { ($1, $3) }
;
def_constructor_list:
  | BAR def_constructor { [ $2 ] }
  | BAR def_constructor def_constructor_list { $2 :: $3 }
;

def_product:
  | LBRACE def_record_field_list RBRACE { mk (E_record $2) }
;
def_record_field_list:
  | label_name EQ type_expr opt_semi { [ ($1, $3) ] }
  | label_name EQ type_expr SEMI def_record_field_list { ($1, $3) :: $5 }
;

/**** SPECIES ****/

species:
  | SPECIES LIDENT species_params inherits EQUAL species_body END
      { mk { sd_name = $2; sd_params = $3; sd_inherits = $4; sd_fields = $6; } }
;

species_params:
  | { [] }
  | LPAREN species_param_list RPAREN { $2 }
;

species_param_list:
  | species_param { [$1] }
  | species_param COMMA species_param_list { $1 :: $3 }
;

species_param:
  | LIDENT IN ident { ($1, mk (SPT_in $3)) }
  | LIDENT IS species_expr { ($1, mk (SPT_is $3)) }
;

inherits:
  | { [] }
;

species_body:
  | { [] }
;

/**** COLLECTION DEFINITION ****/

collection:
  | COLLECTION LIDENT IMPLEMENTS species_expr EQUAL END
      { mk { cd_name = $2; cd_body = $4; } }
;

species_expr:
  | { [] }
;

/**** FUNCTION & VALUES DEFINITION ****/

def_let:
  | LET binding
       { mk {ld_rec = RF_no_rec; ld_bindings = [$2]} }
  | LET REC binding_list
       { mk {ld_rec = RF_rec; ld_bindings = $3} }
;

binding:
  | LIDENT EQ expr
       { mk {b_name = $1; b_params = []; b_type = None; b_body = $3} }
  | LIDENT IN type_expr EQ expr
       { mk {b_name = $1; b_params = []; b_type = Some $3; b_body = $5} }
  | LIDENT LPAREN param_list RPAREN EQ expr
       { mk {b_name = $1; b_params = $3; b_type = None; b_body = $6} }
  | LIDENT LPAREN param_list RPAREN IN type_expr EQ expr
       { mk {b_name = $1; b_params = $3; b_type = Some $6; b_body = $8} }
;

param_list:
  | param { [$1] }
  | param COMMA param_list { $1 :: $3 }
;

param:
  | LIDENT { ($1, None) }
  | LIDENT IN type_expr { ($1, Some $3) }
;

/**** PROPERTIES & THEOREM DEFINITION ****/

def_letprop:
  | LETPROP binding
      { mk {ld_rec = RF_no_rec; ld_bindings = [$2]} }
;

def_theorem:
  | THEOREM LIDENT COLON prop PROOF COLON proof
      { mk { td_name = $2; td_stmt = $4; td_proof = $7 } }
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
  | expr
     { mk (P_expr $1) }
  | LPAREN prop RPAREN
     { mk $2 }
;

opt_in_type_expr:
  | IN type_expr { Some $2 }
  |              { None }
;

vname_list:
  | LIDENT vname_list { $1 :: $2 }
  |                   { [] }
;

proof:
 | ASSUMED { [] }

/**** TYPE EXPRESSIONS ****/

type_expr:
  | SELF
     { mk TE_self }
  | PROP
     { mk TE_prop }
  | glob_ident
     { mk (TE_ident $1) }
  | LIDENT
     { mk (TE_ident (mk (I_method (Some $1, "self")))) }
  | type_expr DASH_GT type_expr
     { mk (TE_fun ($1, $3)) }
  | type_expr STAR type_expr
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
     { mk (CR_global ($1, $3)) }
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
     { mk (E_constant $1) }
  | PREFIX_OP expr
     { mk (E_app ($1, $2)) }
  | FUN vname_list DASH_GT expr
     { mk (E_fun ($2, $4)) }
  | ident
     { mk (E_var $1) }
  | opt_lident SHARP UIDENT %prec prec_constant_constructor
     { mk (E_constr (mk (I_global ($1, $3))), []) }
  | opt_lident SHARP UIDENT LPAREN expr_comma_list RPAREN
     { mk (E_constr (mk (I_global ($1, $3))), $5) }
  | expr LPAREN expr_comma_list RPAREN
     { mk (E_app ($1, $3)) }
  | MATCH expr WITH clause_list
     { mk (E_match ($2, $4)) }
  | IF expr THEN expr ELSE expr
     { mk (E_if ($2, $4, $6)) }
  | LET opt_rec binding_list IN expr
     { mk (E_let ($2, $3, $5)) }
  | LBRACE record_field_list RBRACE
     { mk (E_record $2) }
  | LBRACKET expr_semi_list RBRACKET { $2 }
  | expr COLON_COLON expr { mk (E_app (mk_cons (), [$1; $3])) }
  | LPAREN expr COMMA expr_comma_list RPAREN { mk (E_tuple ($2 :: $4)) }
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
  | expr PLUS_OP expr
      { mk_infix $1 $2 $3 }
  | expr DASH_OP expr
      { mk_infix $1 $2 $3 }
  | expr STAR_OP expr
      { mk_infix $1 $2 $3 }
  | expr SLASH_OP expr
      { mk_infix $1 $2 $3 }
  | expr PERCENT_OP expr
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
  | expr BAR_BAR_OP expr
      { mk_infix $1 $2 $3 }
  | expr AMPER_OP expr
      { mk_infix $1 $2 $3 }
  | LPAREN expr RPAREN
     { $2 }
  | LPAREN RPAREN { mk (E_constr (mk_void (), [])) }
;

expr_semi_list:
  | { mk (E_app (mk_nil (), [])) }
  | expr opt_semi { mk (E_app (mk_cons (), [$1; mk (E_app (mk_nil (), []))])) }
  | expr SEMI expr_semi_list { mk (E_app (mk_cons (), [$1; $3])) }
;
expr_comma_list:
  | expr { [ $1 ] }
  | expr COMMA expr_comma_list { $1 :: $3 }
;

record_field_list:
  | label_name EQ expr opt_semi { [ ($1, $3) ] }
  | label_name EQ expr SEMI record_field_list { ($1, $3) :: $5 }
;

ident:
  | glob_ident { $1 }
  | opt_lident BANG LIDENT
     { mk (I_method ($1, $3)) }
  | LIDENT { mk (I_local $1) }
;

clause_list:
  | { [] }
  | clause BAR clause_list { $1 :: $3 }
;

clause:
  | pattern DASH_GT expr { ($1, $3) }
;

constant:
  | INT { mk (C_int $1) }
  | BOOL { mk (C_bool $1) }
  | STRING { mk (C_string $1) }
;

pattern:
  | constant { mk (P_constant $1) }
  | LIDENT { mk (P_var $1) }
  | UNDERSCORE { mk (P_wild) }
  | constructor_ref LPAREN pattern_comma_list RPAREN { mk (P_constr ($1, $3)) }
  | constructor_ref %prec prec_constant_constructor { mk (P_constr ($1, [])) }
  | LBRACKET pattern_semi_list RBRACKET { $2 }
  | pattern COLON_COLON pattern { mk (P_constr (mk_cons (), [$1; $3])) }
  | LBRACE pattern_record_field_list RBRACE { mk (P_record $2) }
  | pattern AS LIDENT { mk (P_as ($1, $3)) }
  | LPAREN pattern COMMA pattern_comma_list RPAREN { mk (P_tuple ($2 :: $4)) }
  | LPAREN pattern RPAREN { $2 }
  | LPAREN RPAREN { mk (P_constr (mk_void (), [])) }
;

pattern_semi_list:
  | { mk (P_constr (mk_nil (), [])) }
  | pattern opt_semi
    { mk (P_constr (mk_cons (), [$1; mk (P_constr (mk_nil (), []))])) }
  | pattern SEMI pattern_semi_list
    { mk (P_constr (mk_cons (), [$1; $3])) }
;
pattern_comma_list:
  | pattern { [ $1 ] }
  | pattern COMMA pattern_comma_list { $1 :: $3 }
;

pattern_record_field_list:
  | label_name EQ pattern opt_semi { [ ($1, $3) ] }
  | label_name EQ pattern SEMI pattern_record_field_list { ($1, $3) :: $5 }
;

opt_semi:
  | { () }
  | SEMI { () }
;

opt_rec:
  | { RF_no_rec }
  | REC { RF_rec }
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
