/*  Copyright 2006 INRIA  */
/*  $Id: parser_dk.mly,v 1.4 2008-08-14 14:09:23 pessaux Exp $  */

%{
open Expr;;

let mk_dk_apply (e,l) =
  match e with
  | Eapp (s, args) ->  Eapp(s, args @ l)
  | Evar (s) -> Eapp(s, l)
  | _ -> raise Parse_error
;;

%}

/* tokens for parsing dk syntax */

%token <string> FQN
%token BANG_
%token PERCENT_
%token AMPER_
%token AMPER_AMPER_
%token LPAREN_
%token LPAREN_RPAREN_
%token RPAREN_
%token STAR_
%token PLUS_
%token PLUS_PLUS_
%token COMMA_
%token DASH_
%token DASH_GT_
%token PERIOD_
%token PERIOD_LPAREN_
%token PERIOD_PERIOD_
%token SLASH_
%token SLASH_BACKSL_
%token COLON_
%token COLON_COLON_
%token COLON_LT_
%token COLON_EQ_
%token COLON_GT_
%token SEMI_
%token LT_
%token LT_DASH_
%token LT_DASH_GT_
%token LT_COLON_
%token LT_EQ_
%token LT_GT_
%token EQ_
%token EQ_GT_
%token EQ_UNDER_D_
%token GT_
%token GT_DASH_GT_
%token GT_EQ_
%token QUEST_
%token QUEST_EQ_
%token AROBAS_
%token LBRACK_
%token BACKSL_SLASH_
%token RBRACK_
%token HAT_
%token LBRACE_
%token BAR_
%token BAR_DASH_
%token BAR_BAR_
%token RBRACE_
%token TILDE_

%token AS
%token AT
%token COFIX
%token DEFINITION
%token DEPENDS
%token ELSE
%token END
%token EXISTS
%token EXISTS2
%token FIX
%token FOR
%token FORALL
%token FUN
%token IF
%token UC_IF
%token IN
%token LET
%token MATCH
%token MOD
%token ON
%token PARAMETER
%token PROP
%token RETURN
%token SET
%token THEN
%token THEOREM
%token TYPE
%token USING
%token WHERE
%token WITH

%token BEGINPROOF
%token <string> BEGINNAME
%token BEGINHEADER
%token ENDPROOF


%token <string> IDENT
%token <string> STRING
%token EOF

/* precedences for dk syntax */

%nonassoc LPAREN_
%nonassoc let_in
%nonassoc IDENT FQN
%nonassoc FORALL EXISTS COMMA_ IF THEN ELSE
%right DASH_GT_ LT_DASH_GT_
%right BACKSL_SLASH_
%right SLASH_BACKSL_
%nonassoc EQ_ LT_GT_
%nonassoc TILDE_
%left apply

%start dkfile
%type <Expr.expr * Expr.phrase list> dkfile

%%

/* Dk Syntax */

dkfile:
  | dk_hyp_def_list THEOREM IDENT COLON_ dkexpr PERIOD_ EOF
      { ($5, $1) }

/* deprecated "Focal" format -- kept for compatibility */

  | BEGINPROOF headers BEGINNAME headers dkexpr dk_hyp_def_list ENDPROOF EOF
      { ($5, $6) }
  | dkexpr dk_hyp_def_list EOF
      { ($1, $2) }
;

headers:
  | /* empty */         { () }
  | BEGINHEADER headers { () }

dkident:
  | IDENT {  $1 }
  | FQN { $1 }

dkexpr:
  | FORALL dkbindings COMMA_ dkexpr
      { Eall ($2, $4) }
  | EXISTS dkbindings COMMA_ dkexpr
      { Eex ($2, $4) }

  | LET IDENT COLON_EQ_ dkexpr IN dkexpr %prec let_in
      {assert false}

  | dkexpr DASH_GT_ dkexpr
      { Eimply ($1, $3) }

  | dkexpr LT_DASH_GT_ dkexpr
      { Eequiv ($1, $3) }

  | dkexpr BACKSL_SLASH_ dkexpr
      { Eor ($1, $3) }

  | dkexpr SLASH_BACKSL_ dkexpr
      { Eand ($1, $3) }

  | dkexpr EQ_ dkexpr
      { Eapp ("=", [$1; $3]) }
  | dkexpr LT_GT_ dkexpr
      { Enot (Eapp ("=", [$1; $3])) }

  | TILDE_ dkexpr
      { Enot ($2) }

  | dkexpr1 dkexpr1_list  %prec apply
      { mk_dk_apply ($1, $2) }

  | dkexpr1
      { $1 }
;

dkexpr1:
  | dkident
      { Evar ($1) }
  | LPAREN_ dkexpr RPAREN_
      { $2 }
;

dkexpr1_list:
  | dkexpr1                  { [$1] }
  | dkexpr1 dkexpr1_list    { $1 :: $2 }
;

dkbindings:
  | dksimplebinding          { $1 }
  | dkbinding_list           { $1 }
;

dksimplebinding:
  | dkidlist COLON_ dktype  { $1 }
;

dkidlist:
  | /* empty */               { [] }
  | IDENT dkidlist           { $1 :: $2 }
;

dkbinding_list:
  | /* empty */
      { [] }
  | IDENT dkbinding_list
      { ($1) :: $2 }
  | LPAREN_ dksimplebinding RPAREN_ dkbinding_list
      { $2 @ $4 }
;

dktype:
  | dkexpr               { () }
;

/* normal identifier or unparsed dk expression */
id_or_dkexpr:
  | IDENT  { $1 }
  | STRING { $1 }
;

dkparam_expr:
  | dkexpr
      { ([], $1) }
  | LPAREN_ FUN LPAREN_ IDENT COLON_ dktype RPAREN_ EQ_GT_ dkparam_expr
      RPAREN_
      { let (params, expr) = $9 in (($4) :: params, expr) }
;

parameter:
  | PARAMETER id_or_dkexpr COLON_ dkexpr PERIOD_         { Hyp $4 }


definition:
  | DEFINITION id_or_dkexpr COLON_EQ_ dkparam_expr PERIOD_
      { let (params, expr) = $4 in  Def ($2, params, expr) }
/* Modif par François. A valider, Damien. */
  | DEFINITION IDENT compact_args COLON_ dktype COLON_EQ_ dkparam_expr PERIOD_
      {
       let compact_params = $3 in
       let (other_params, expr) = $7 in
       Def ($2, (compact_params @ other_params), expr)
     }
/* Fin modif par François. */
;


/* Modif par François. A valider, Damien. */
compact_args:
    /* empty */                                          { [] }
  | LPAREN_ IDENT COLON_ dktype RPAREN_ compact_args    { $2 :: $6 }
;
/* Fin modif par François. */


dk_hyp_def:
  | DEPENDS ON parameter       { $3 }
  | DEPENDS ON definition      { $3 }
  | parameter                  { $1 }
  | definition                 { $1 }
;

dk_hyp_def_list:
  | dk_hyp_def dk_hyp_def_list   { $1 :: $2 }
  | /* empty */                    { [] }
;

%%
