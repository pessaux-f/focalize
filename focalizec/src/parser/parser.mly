%{
(* $Id: parser.mly,v 1.3 2006-11-16 23:11:14 weis Exp $ *)

open Parsetree;;

let mk d = {
  ast_loc = {
    l_beg = Parsing.symbol_start_pos ();
    l_beg = Parsing.symbol_end_pos ();
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
%token <string> QIDENT
%token <string> UIDENT
%token <string> INT
%token <string> STRING

/* Arithmetic operators */
%token PLUS
%token <string> PLUS_OP
%token DASH
%token <string> DASH_OP
%token STAR
%token <string> STAR_OP
%token SLASH
%token <string> SLASH_OP
%token STAR_STAR
%token <string> STAR_STAR_OP
%token LPAREN
%token RPAREN
%token COMMA
%token <string> COMMA_OP
%token QUOTE
%token DOUBLEQUOTE
%token BACKSLASH
%token DASH_GT
%token <string> DASH_GT_OP
%token LT_DASH_GT
%token <string> LT_DASH_GT_OP
%token SHARP
%token BANG
%token BAR
%token <string> BAR_OP
%token BAR_BAR
%token <string> BAR_BAR_OP
%token AMPER
%token <string> AMPER_OP
%token AMPER_AMPER
%token <string> AMPER_AMPER_OP
%token UNDERSCORE
%token EQ
%token <string> EQ_OP
%token LT
%token <string> LT_OP
%token LT_DASH
%token <string> LT_DASH_OP
%token GT
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
%token AND
%token AS
%token ASSUMED
%token BUT
%token BY
%token COLLECTION
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
%token WITH

%token COQPROOF

/* Precedences and associativities. */

%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI                          /* below EQ ({lbl=...; lbl=...}) */
%nonassoc LET                           /* above SEMI ( ...; let ... in ...) */
%nonassoc below_WITH
%nonassoc FUNCTION WITH                 /* below BAR  (match ... with ...) */
%nonassoc AND             /* above WITH (module rec A: SIG with ... and ...) */
%nonassoc THEN                          /* below ELSE (if ... then ...) */
%nonassoc ELSE                          /* (if ... then ... else ...) */
%nonassoc LT_DASH                       /* below COLON_EQ (lbl <- x := e) */
/* %right    COLON_EQ                   /* expr (e := e := e) */ */
/* %nonassoc AS */
%left     BAR BAR_OP                    /* pattern (p|p|p) */
%nonassoc below_COMMA
%left     COMMA COMMA_OP                /* expr/expr_comma_list (e,e,e) */
%nonassoc LT_DASH_GT                    /* <-> */
%nonassoc NOT
%right    DASH_GT DASH_GT_OP            /* core_type2 (t -> t -> t) */
%right    BAR_BAR BAR_BAR_OP            /* expr (e || e || e) */
%right    AMPER AMPER_OP AMPER_AMPER AMPER_AMPER_OP  /* expr (e && e && e) */
%nonassoc below_EQ
%left     EQ EQ_OP LT LT_OP GT GT_OP   /* expr (e OP e OP e) */
%right    AT AT_OP HAT HAT_OP          /* expr (e OP e OP e) */
%right    COLON_COLON COLON_COLON_OP   /* expr (e :: e :: e) */
%left     PLUS PLUS_OP DASH DASH_OP    /* expr (e OP e OP e) */
%left     STAR STAR_OP SLASH SLASH_OP  /* expr (e OP e OP e) */
%right    STAR_STAR STAR_STAR_OP       /* expr (e OP e OP e) */
%right    SLASH SLASH_OP               /* expr (e OP e OP e) */
%nonassoc prec_unary_dash              /* unary - */
%nonassoc prec_constant_constructor    /* cf. simple_expr (C versus C x) */
%nonassoc prec_constr_appl             /* above AS BAR COLON_COLON COMMA */
%nonassoc below_SHARP
%nonassoc SHARP                        /* simple_expr/toplevel_directive */
%nonassoc below_DOT
%nonassoc DOT
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc BEGIN CHAR FALSE INT
          LBRACE LBRACKET LIDENT LPAREN
          BACKQUOTE BACKQUOTE_OP TILDE TILDE_OP QUESTION QUESTION_OP DOLLAR DOLLAR_OP
          STRING TRUE UIDENT

%start main
%type <Parsetree.phrase list> main
%%

main:
  | EOF { [] }
  | phrase main { $1 :: $2 }
;
 
phrase:
  | def_let SEMI_SEMI { mk (Ph_let $1) }
  /* a voir: ajouter les expressions a toplevel ? */
  | def_letprop SEMI_SEMI { mk (Ph_letprop $1) }
  | def_theorem SEMI_SEMI { mk (Ph_theorem $1) }

/*
  | atc  { $1 }
  | spec { $1 }
  | collection { $1}
  | type_def { $1 }
  | foc_header { mk (Foc_header($1)) }
*/
;

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
     { mk (P_forall ($2, $3, $5))
  | EX vname_list opt_in_type_expr COMMA prop
     { mk (P_exists ($2, $3, $5))
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

type_expr:
  | SELF
     { mk TE_self }
  | PROP
     { mk TE_prop }
  | QIDENT
     { mk (TE_ident (mk (I_local $1))) }
  | glob_ident
     { mk (TE_ident $1) }
  | LIDENT
     { mk (TE_ident (mk (I_method (Some $1, "self")))) }
  | type_expr DASH_GT type_expr
     { mk (TE_fun ($1, $3)) }
  | type_expr STAR type_expr
     { mk (TE_prod ($1, $3)) }
  | glob_ident LPAREN type_expr_list RPAREN
     { mk (TE_app ($1, $3)) }
  | LPAREN type_expr RPAREN
     { $2 }
;

type_expr_list:
  | type_expr COMMA type_expr_list { $1 :: $3 }
  | type_expr { [$1] }
;

constr_ref:
  | opt_lident SHARP UIDENT
     { mk (CR_global ($1, $3)) }
;

glob_ident:
  | opt_lident SHARP LIDENT
     { mk (I_global ($1, $3)) }
;

opt_lident:
  | LIDENT { Some $1 }
  |        { None }
;

expr:
  | INT
     { mk (E_const (mk (C_int $1))) }
  | BOOL
     { mk (E_const (mk (C_bool $1))) }
  | STRING
     { mk (E_const (mk (C_string $1))) }
  | FUN vname_list DASH_GT expr
     { mk (E_fun ($2, $4)) }
  | ident
     { mk (E_var $1) }
  | opt_lident SHARP UIDENT %prec constant_constr
     { mk (E_constr (mk (I_global ($1, $3))), []) }
  | opt_lident SHARP UIDENT LPAREN expr_list RPAREN
     { mk (E_constr (mk (I_global ($1, $3))), $5) }
  | expr LPAREN expr_list RPAREN
     { mk (E_app ($1, $3)) }
  | MATCH expr WITH clause_list
     { mk (E_match ($2, $4)) }
  | IF expr THEN expr ELSE expr
     { mk (E_if ($2, $4, $6)) }
  | LET opt_rec binding_list IN expr
     { mk (E_let ($2, $3, $5)) }
  | LBRACE record_field_list RBRACE
     { mk (E_record $2) }
  | EXTERNAL external_name opt_external_name
     { mk (E_external ($2, $3)) }
  | LBRACKET expr_semi_list RBRACKET { $2 }
  | expr COLON_COLON expr { mk (E_app (mk_cons (), [$1; $3])) }
  | LPAREN expr COMMA expr_comma_list RPAREN { mk (E_tuple ($2 :: $4)) }
  | expr HAT expr
      { mk_infix $1 "^" $3 }
  | expr HAT_OP expr
      { mk_infix $1 $2 $3 }
  | expr AT expr
      { mk_infix $1 "@" $3 }
  | expr AT_OP expr
      { mk_infix $1 $2 $3 }
  | expr SEMI_OP expr
      { mk_infix $1 $2 $3 }
  | expr SEMI_SEMI_OP expr
      { mk_infix $1 $2 $3 }
  | expr COLON_COLON_OP expr
      { mk_infix $1 $2 $3 }
  | expr PLUS expr
      { mk_infix $1 "+" $3 }
  | expr PLUS_OP expr
      { mk_infix $1 $2 $3 }
  | expr DASH expr
      { mk_infix $1 "-" $3 }
  | expr DASH_OP expr
      { mk_infix $1 $2 $3 }
  | expr STAR expr
      { mk_infix $1 "*" $3 }
  | expr STAR_OP expr
      { mk_infix $1 $2 $3 }
  | expr SLASH expr
      { mk_infix $1 "/" $3 }
  | expr SLASH_OP expr
      { mk_infix $1 $2 $3 }
  | expr PERCENT expr
      { mk_infix $1 "/" $3 }
  | expr PERCENT_OP expr
      { mk_infix $1 $2 $3 }
  | expr BACKSLASH expr
      { mk_infix $1 "\\" $3 }
  | expr BACKSLASH_OP expr
      { mk_infix $1 $2 $3 }
  | expr EQ expr
      { mk_infix $1 "=" $3 }
  | expr EQ_OP expr
      { mk_infix $1 $2 $3 }
  | expr LT expr
      { mk_infix $1 "<" $3 }
  | expr LT_OP expr
      { mk_infix $1 $2 $3 }
  | expr LT_DASH_OP expr
      { mk_infix $1 $2 $3 }
  | expr LT_DASH_GT_OP expr
      { mk_infix $1 $2 $3 }
  | expr GT expr
      { mk_infix $1 ">" $3 }
  | expr GT_OP expr
      { mk_infix $1 $2 $3 }
  | expr BAR_BAR expr
      { mk_infix $1 "||" $3 }
  | expr BAR_BAR_OP expr
      { mk_infix $1 $2 $3 }
  | expr AMPER_AMPER expr
      { mk_infix $1 "&&" $3 }
  | expr AMPER_AMPER_OP expr
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

pattern:
  | constant { mk (P_const $1) }
  | LIDENT { mk (P_var $1) }
  | UNDERSCORE { mk (P_wild) }
  | constr_ref LPAREN pattern_list RPAREN { mk (P_constr ($1, $3)) }
  | constr_ref %prec constant_constr { mk (P_constr ($1, [])) }
  | LBRACKET pattern_semi_list RBRACKET { $2 }
  | pattern COLON_COLON pattern { mk (P_constr (mk_cons (), [$1; $3])) }
  | LBRACE label_pattern_list RBRACE { mk (P_record $2) }
  | LPAREN pattern COMMA pattern_comma_list RPAREN { mk (P_tuple ($2 :: $4)) }
  | LPAREN pattern RPAREN { $2 }
  | LPAREN RPAREN { mk (P_constr (mk_void (), [])) }
;

pattern_semi_list:
  | { mk (P_constr (mk_nil (), [])) }
  | pattern opt_semi { mk (P_constr (mk_cons (), [$1; mk (P_constr (mk_nil (), []))])) }
  | pattern SEMI pattern_semi_list { mk (P_constr (mk_cons (), [$1; $3])) } 
;
pattern_comma_list:
  | pattern { [ $1 ] }
  | pattern COMMA pattern_comma_list { $1 :: $3 } 
;

label_pattern_list:
  | LIDENT EQ pattern opt_semi { [ ($1, $3) ] }
  | LIDENT EQ pattern SEMI label_pattern_list { ($1, $3) :: $5 }
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

record_field_list:
  | label EQ expr { [ ($1, $3) ] }
  | label EQ expr SEMI record_field_list { ($1, $3) :: $5 }
;

external_name:
  | STRING { $1 }
;

opt_external_name:
  | { None }
  | STRING { Some $1 }
;
