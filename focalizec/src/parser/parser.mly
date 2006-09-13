%{
(* $Id: parser.mly,v 1.1 2006-09-13 13:59:13 weis Exp $ *)

open Parsetree;;

let mk d = {
  ast_loc = ;
  ast_desc = d;
};;

%}

%token EOF

%token <string> LIDENT
%token <string> UIDENT
%token <string> INT
%token <string> STRING

%token PLUS
%token DASH
%token STAR
%token SLASH
%token LPAREN
%token RPAREN
%token COMMA
%token QUOTE
%token DOUBLEQUOTE
%token BACKSLASH
%token DASH_GT
%token LT_DASH_GT
%token SHARP
%token BANG
%token BAR
%token UNDERSCORE
%token EQ
%token SEMI
%token COLON
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

%nonassoc LT_DASH_GT
%right DASH_GT
%left COMMA AND OR
%nonassoc NOT
%left PLUS DASH
%left STAR SLASH

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
  | thm  { $1 }
  | atc  { $1 }
  | spec { $1 }
  | collection { $1}
  | type_def { $1 }
  | foc_header { mkinst(Foc_header($1)) }
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

binding_list:
  | binding { [$1] }
  | binding AND binding_list { $1 :: $3 }
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




foc_header:
  | USES LIDENT SEMI_COLON SEMI_COLON 
     { 
       mkfoc_header(Uses( $2)) }
  | OPEN LIDENT SEMI_COLON SEMI_COLON 
     { 
       mkfoc_header(Open( $2))  }
;

ident:
  | LIDENT { Sample_id( $1 ) }
  | UIDENT { Sample_id( $1 ) }
;

typ:
  | typ_simpl { $1 }
  /* Précedence des tuples comme en OCaml */
  | typ_simpl TIMES typ_times_list { mktyp( Typ_tuple( $1::$3)) }
;

typ_simpl:
  | LIDENT 
        { 
	  mktyp(Typ_simple( $1 )) }
  | QUOTE LIDENT 
        { 
	  mktyp(Typ_poly( $2 )) }
  | LIDENT LPAREN typ_list RPAREN 
        { 
	  mktyp(Typ_constr(Sample_id($1), $3)) }
  | typ_simpl IMPLIES typ_simpl 
        { 
	  mktyp(Typ_arrow( $1, $3)) }
  | SELF 
        { 
	  mktyp(Typ_self) }
  | LPAREN typ RPAREN 
        { 
	  $2 }
  | PROP 
      { 
	mktyp(Typ_simple( "Prop" )) } 
;

typ_times_list:
  | typ_simpl { [$1]}
  | typ_simpl TIMES typ_times_list { $1::$3} 
;

typ_list:
  | typ { [$1]}
  | typ COMMA typ_list { $1::$3} 
;

exp:
  | INTEGER_LITTERAL 
       { 
         mkexp(Exp_constant( Const_int( int_of_string $1 )))}
  | STRING_LITTERAL  
       {  
	 mkexp(Exp_constant( Const_string( $1 )))}
  | exp_ident        
       { 
	 mkexp(Exp_ident( $1 ))} 
  | LET rec_opt decl_let EQUAL exp IN exp 
       { 
	 mkexp(Exp_let($2, $3, $5, $7)) }
  | FUN LIDENT IMPLIES exp  
       { 
	 mkexp(Fun(Sample_id($2),$4)) }
  | FUN LPAREN LIDENT IN typ RPAREN  IMPLIES exp  
       { 
	 mkexp(Fun_prm(Sample_id($3), $5, $8))}
  | call             
       { dbg "exp_9";
         mkexp($1)}
  | MATCH exp WITH pipe_opt patt_list END 
       { 
	 mkexp(Match($2, $5)) } 
  | IF exp THEN exp ELSE exp 
       { dbg "exp_11";
         mkexp(If($2, $4, $6))} 
  /* Modif exp -> exp_list pour les tuples */
  /* | LPAREN exp_list RPAREN { dbg "exp_12b" } */   
  | LPAREN exp RPAREN 
      { 
	$2 }
  | SELF                           /* Ajouter */
       { 
	 mkexp(Self_exp)}   
;

exp_ident:
  | LIDENT { Sample_id( $1 ) } 
  | ident_opt SHARP ident 
      { 
        match $3 with
	| Sample_id(s) -> Sharp_id( $1, s)
	| _ -> assert( false ) }
  | self_opt BANG LIDENT 
      { 
	Bang_id( $1, $3) }
  | LIDENT BANG LIDENT 
      { 
	Bang_id( Some( Sample_id $1), $3) }
;

call:
  exp LPAREN exp_list RPAREN   /* Reduction exp -> exp_ident */
      { dbg "call_1";
        Call( $1, $3)}  
;

call_body:
  exp LPAREN exp_list RPAREN   /* Reduction exp -> exp_ident */
      { dbg "call_1";
        Call_body( $1, $3)}  
;

ident_opt:
  |        { None }
  | ident  { Some( $1) }
;

self_opt:
  |        { None }
  | SELF   { Some( Self_id ) }
;

rec_opt:
  |       { Nonrecursive }
  | REC   { Recursive }
;

decl_let:
  | LIDENT prms_opt in_typ_opt 
       { dbg "decl_let_1";
         mkdecl_let(Decl_let(Sample_id($1), $2, $3))}
   /* Ajout des tuples */
  | prms in_typ_opt 
       { dbg "decl_let_2";
         mkdecl_let(Tuple($1, $2))}
;

prms_opt:
  |       { None }
  | prms  { Some $1 }
;

prms: 
  | LPAREN prms_list RPAREN { $2 }
;

prms_list:
  | prm  { [$1] }
  | prm COMMA prms_list { $1::$3}
;
 
prm:
  | LIDENT in_typ_opt 
      { 
        mkprm( Prm_in( Sample_id($1), $2))}
  | LIDENT IS typ                 /* Ajouter */
      { dbg "prms_list_1b";
        mkprm( Prm_is( Sample_id($1), $3))} 
;

in_typ_opt:
  |         { None }
  | IN typ  { Some $2 }
;


exp_list:
  | exp { [$1] }  
  | exp COMMA exp_list  { $1::$3 }
;

pipe_opt:
  |      { dbg "pipe_opt_1" }
  | PIPE { dbg "pipe_opt_2" }
;

patt_list:
  | patt { [$1]}    
  | patt PIPE patt_list { $1::$3 }
;

patt:
  | left_patt IMPLIES exp 
      { dbg "patt_1";
        mkpatt(Pattern($1,$3))}
;   

left_patt:
  | lid_or_wildcard 
      { 
	Lid_wcard($1) }
  | ident_opt SHARP UIDENT patt_prm_opt 
      { dbg "left_patt_2";
        Patt_typ(Sharp_id($1, $3), $4)}
;

lid_or_wildcard:
  | LIDENT { Lid(Sample_id($1)) }
  | UNDERSCORE { Wcard }
;

patt_prm_opt:
  |          { None }
  | patt_prm { Some $1 }
;

patt_prm:
  | LPAREN patt_prm_list RPAREN { $2 }
;

patt_prm_list:
  | lid_or_wildcard     { [$1] }
  | lid_or_wildcard COMMA  patt_prm_list { $1::$3 }
;


prop:
  | exp 
      { 
	mkprop(Pprop_exp($1)) }
  | ALL lident_list IN typ COMMA prop 
      { 
	mkprop(Pprop_all( $2, Some $4, $6)) }
  | ALL lident_list COMMA prop 
      { 
	mkprop(Pprop_all( $2, None, $4)) }
  | EX lident_list IN typ COMMA prop
      { 
	mkprop(Pprop_ex( $2, $4, $6)) }
  | prop AND prop 
      { 
	mkprop(Pprop_and( $1,$3)) }
  | prop OR prop 
      { 
	mkprop(Pprop_or( $1,$3)) }
  | prop IMPLIES prop 
      { 
	mkprop(Pprop_implies( $1,$3)) }
  | prop EQUIV prop 
      { 
	mkprop(Pprop_equiv( $1,$3)) }
  | NOT prop 
      { 
	mkprop(Pprop_not( $2 )) }
  | LPAREN prop RPAREN 
      { 
	$2 }
;

lident_list:
  | LIDENT { [Sample_id($1)] }
  | LIDENT lident_list { Sample_id($1)::$2 }
;

proof:
  | ASSUMED { mkproof( Assumed )}
  | zenon_cmd_list { mkproof( Zenon_code( $1 )) }
  | CODE_COQ { mkproof( Coq_code )}
;

thm:
  | THEOREM LIDENT COLON prop PROOF COLON proof SEMI_COLON SEMI_COLON 
      { 
	mkinst ( Thm(Sample_id($2), $4, $7)) }
;

atc:
  | ATTACH LIDENT ident proof_opt SEMI_COLON SEMI_COLON 
      { dbg "atc_1";
        mkinst ( Atc(Sample_id($2), $3, $4))}
;

proof_opt:
  |                   { None }
  | PROOF COLON proof { Some $3 }
;

lpt_body:
  | LETPROP LIDENT prms_opt EQUAL prop 
      { 
	mklpt_body( Letprop( (Sample_id $2), $3, $5))}
;

lpt:
  | lpt_body SEMI_COLON SEMI_COLON { mkinst (Lpt $1) }
;


spec:
  | SPECIES LIDENT spec_prms_opt inherits_opt EQUAL body_opt END 
      { dbg "spec_1";
        mkinst( Spec(Sample_id($2), $3, $4, $6))}
;

collection:
  | COLLECTION LIDENT IMPLEMENTS spc_bind EQUAL def_spec_opt END 
      { dbg "coll_1";
	mkinst( Coll(Sample_id($2), $4, $6))
      }
;

body_opt:
  |       { None }
  | body  { Some $1 }
;

body:
  | field_list { $1 }
;

spec_prms_opt:
  |            { None }
  | spec_prms  { Some $1 }
;

spec_prms:
  | LPAREN spec_prms_list RPAREN { $2 }
;

spec_prms_list:
  | spec_prm { [$1] }
  | spec_prm COMMA spec_prms_list { $1::$3 }
;

spec_prm:
  | LIDENT IN typ 
      { 
        mkspec_prm(Spec_prm_in(Sample_id($1), $3)) }
  | LIDENT IS spc_bind 
      { 
        mkspec_prm(Spec_prm_is(Sample_id($1), $3)) }
;

inherits_opt:
  |             { None }
  | INHERITS inh_list { Some $2 }
;

inh_list:
  | inh { [$1] }
  | inh COMMA inh_list { $1::$3 }
;

inh:
  | spc_bind { $1 }
;

spc_bind:
  | LIDENT inst_prm_opt { (mkspc_bind (Sample_id $1) $2) }
;

inst_prm_opt:
  |                { None }
  | LPAREN inst_prm_list RPAREN  { Some $2 }
;

inst_prm_list:
  | inst_prm { [$1] }
  | inst_prm COMMA inst_prm_list { $1::$3 }
;

inst_prm:
  | exp { $1 }
;

field_list:
  | field { [$1] }  
  | field field_list { $1::$2 }
;

field:
  | def_spec { mkfield(Def_spec($1)) }  
  | decl_spec { mkfield(Decl_spec($1)) }  
  | LOCAL def_spec { mkfield(Local_def_spec($2)) }  
;

def_spec:

  | def_body SEMI_COLON 
       { 
	 mkdef_spec( Def_body($1)) } 
  | lpt_body SEMI_COLON 
       { 
	 mkdef_spec( Lpt_body($1)) } 
  | PROOF OF LIDENT EQUAL dep_clause_list_opt proof SEMI_COLON 
       { 
	 mkdef_spec(Proof_of(Sample_id($3), $5, $6)) } 
  | REP EQUAL typ SEMI_COLON 
       { 
	 mkdef_spec(Def_Rep($3))  }
  | THEOREM LIDENT COLON prop PROOF COLON dep_clause_list_opt proof SEMI_COLON 
       { dbg "def_spec_5";
	 mkdef_spec(Theorem(Sample_id($2), $4, $7, $8))} 
;

decl_spec:
  | SIG LIDENT IN typ SEMI_COLON  
      { 
	mkdecl_spec(Sig( Sample_id($2), $4)) } 
  | PROPERTY LIDENT COLON prop SEMI_COLON  /* Modif typ -> prop */ 
      { 
	mkdecl_spec(Decl_prop( Sample_id($2), $4)) }  
  | REP SEMI_COLON  
      { 
	mkdecl_spec(Decl_rep) }
;

def_spec_opt:
  |               { None } 
  | def_spec_list { Some $1 } 
;

def_spec_list:
  | def_spec      { [$1] } 
  | def_spec def_spec_list { $1::$2 } 
;

dep_clause_list_opt:
  |                 { None } 
  | dep_clause_list { Some $1 } 
;

dep_clause_list:
  |dep_clause { [$1] } 
  |dep_clause dep_clause_list { $1::$2 } 
;

dep_clause:
  | DECL colon_opt lident_list SEMI_COLON 
         { 
	   mkdep_clause(Decl ($3)) } 
  | DEF colon_opt lident_list SEMI_COLON  
         { 
	   mkdep_clause(Dep ($3))  } 
;

colon_opt:
  |       { dbg "colon_opt_1" } 
  | COLON { dbg "colon_opt_2" } 
;

type_def:
  | TYPE LIDENT lident_list_opt EQUAL type_decl SEMI_COLON SEMI_COLON 
      { dbg "type_def_1";
        mkinst( Type_def( Sample_id($2), $3, $5))} 
;

lident_list_opt:
  |             { None } 
  | lident_list { Some $1 } 
;

type_decl:
  | ALIAS typ { mktype_decl(Alias($2)) } 
  | list_decl_list {  mktype_decl(List_decl($1)) } 
;

list_decl_list:
  | list_decl { [$1] } 
  | list_decl list_decl_list { $1::$2 } 
;

list_decl:
  | CAML ml_path_lident SEMI_COLON 
      { 
	mklist_decl(Caml($2)) } 
  | CAML_LINK ml_path_lident SEMI_COLON 
      { 
	mklist_decl(Caml_link($2)) } /* Ajouter */ 
  | COQ_LINK ident SEMI_COLON 
      { dbg "list_decl_2";
	mklist_decl(Coq_link($2)) } 
  | constr SEMI_COLON 
      { 
	mklist_decl($1)} 
;

ml_path_lident:
  | ml_path_opt LIDENT { dbg "ml_path_lident_1";
			 begin
		           match $1 with
			   | None -> [$2]
			   | Some l -> l@[$2]
			 end} 
;

ml_path_uident:
  | ml_path_opt UIDENT { dbg "ml_path_uident_1";
			 begin
		           match $1 with
			   | None -> [$2]
			   | Some l -> l@[$2]
			 end} 
;

ml_path_opt:
  |         { None } 
  | ml_path { Some $1 } 
;

ml_path:
  | UIDENT DOT { [$1] } 
  | UIDENT DOT ml_path { $1::$3 } 
;

constr:
  | UIDENT constr_opt IN typ  { Constr($1, $2, $4) }
;

constr_opt:
  |   { None }
  | LPAREN ml_path_uident ident_opt RPAREN 
      { dbg "constr_opt_2";
        Some( $2, $3)}
;



/* Syntaxe Zenon */
zenon_cmd_list:
  | zenon_cmd { [$1] }
  | zenon_cmd zenon_cmd_list { $1::$2 }
;

zenon_cmd:
  | BY zenon_ident_list_opt { Zenon_by( $2) }
  | DEF zenon_ident_list { Zenon_def($2) }
;

zenon_ident_list_opt:
  |                  { None}
  | zenon_ident_list { Some $1 }
;

zenon_ident_list:
  | zenon_ident { [$1] }
  | zenon_ident COMMA zenon_ident_list { $1::$3 }
;

zenon_ident:
  | exp_ident { $1 }
;
