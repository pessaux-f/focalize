/*  Copyright 2006 INRIA  */
/*  $Id: parser_coq.mly,v 1.3 2006-06-22 17:09:40 doligez Exp $  */

%{
open Expr;;

let mk_coq_apply (e,l) =
  match e with
  | Eapp (s, args) ->  Eapp(s, args @ l)
  | Evar (s) -> Eapp(s, l)
  | _ -> raise Parse_error
;;

%}

/* tokens for parsing coq syntax */

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

/* precedences for coq syntax */

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

%start coqfile
%type <Expr.expr * Expr.phrase list> coqfile

%%

/* Coq Syntax */

coqfile:
  | coq_hyp_def_list THEOREM IDENT COLON_ coqexpr PERIOD_ EOF
      { ($5, $1) }

/* deprecated "Focal" format -- kept for compatibility */

  | BEGINPROOF headers BEGINNAME headers coqexpr coq_hyp_def_list ENDPROOF EOF
      { ($5, $6) }
  | coqexpr coq_hyp_def_list EOF
      { ($1, $2) }
;

headers:
  | /* empty */         { () }
  | BEGINHEADER headers { () }

coqident:
  | IDENT { $1 }
  | FQN { $1 }

coqexpr:
  | FORALL coqbindings COMMA_ coqexpr
      { Eall ($2, $4) }
  | EXISTS coqbindings COMMA_ coqexpr
      { Eex ($2, $4) }

  | LET IDENT COLON_EQ_ coqexpr IN coqexpr %prec let_in
      {assert false}

  | coqexpr DASH_GT_ coqexpr
      { Eimply ($1, $3) }

  | coqexpr LT_DASH_GT_ coqexpr
      { Eequiv ($1, $3) }

  | coqexpr BACKSL_SLASH_ coqexpr
      { Eor ($1, $3) }

  | coqexpr SLASH_BACKSL_ coqexpr
      { Eand ($1, $3) }

  | coqexpr EQ_ coqexpr
      { Eapp ("=", [$1; $3]) }
  | coqexpr LT_GT_ coqexpr
      { Enot (Eapp ("=", [$1; $3])) }

  | TILDE_ coqexpr
      { Enot ($2) }

  | coqexpr1 coqexpr1_list  %prec apply
      { mk_coq_apply ($1, $2) }

  | coqexpr1
      { $1 }
;

coqexpr1:
  | coqident
      { Evar ($1) }
  | LPAREN_ coqexpr RPAREN_
      { $2 }
;

coqexpr1_list:
  | coqexpr1                  { [$1] }
  | coqexpr1 coqexpr1_list    { $1 :: $2 }
;

coqbindings:
  | coqsimplebinding          { $1 }
  | coqbinding_list           { $1 }
;

coqsimplebinding:
  | coqidlist COLON_ coqtype  { $1 }
;

coqidlist:
  | /* empty */               { [] }
  | IDENT coqidlist           { $1 :: $2 }
;

coqbinding_list:
  | /* empty */
      { [] }
  | IDENT coqbinding_list
      { ($1) :: $2 }
  | LPAREN_ coqsimplebinding RPAREN_ coqbinding_list
      { $2 @ $4 }
;

coqtype:
  | coqexpr               { () }
;

/* normal identifier or unparsed coq expression */
id_or_coqexpr:
  | IDENT  { $1 }
  | STRING { $1 }
;

coqparam_expr:
  | coqexpr
      { ([], $1) }
  | LPAREN_ FUN LPAREN_ IDENT COLON_ coqtype RPAREN_ EQ_GT_ coqparam_expr
      RPAREN_
      { let (params, expr) = $9 in (($4) :: params, expr) }
;

parameter:
  | PARAMETER id_or_coqexpr COLON_ coqexpr PERIOD_
      { Hyp $4 }

definition:
  | DEFINITION id_or_coqexpr COLON_EQ_ coqparam_expr PERIOD_
      { let (params, expr) = $4 in  Def($2, params, expr) }

coq_hyp_def:
  | DEPENDS ON parameter
      {
        $3

      }
  | DEPENDS ON definition
      {
        $3
      }
  | parameter  { $1 }
  | definition { $1 }
;

coq_hyp_def_list:
  | coq_hyp_def coq_hyp_def_list   { $1 :: $2 }
  | /* empty */                    { [] }
;

%%
