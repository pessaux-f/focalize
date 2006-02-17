/*  Copyright 2006 INRIA  */
/*  $Id: parser_sat.mly,v 1.2 2006-02-17 15:55:12 doligez Exp $  */

%token UNSATISFIABLE
%token EOF

/* precedences for coq syntax */

%left apply

%start cimefile
%type <bool> cimefile

%%

/* Focal Syntax */

cimefile:
  | UNSATISFIABLE
      {true}
  | EOF
      {false}
;

%%
