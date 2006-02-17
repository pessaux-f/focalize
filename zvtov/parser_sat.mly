
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
