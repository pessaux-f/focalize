%{
open Own_expr;;
open Own_basics;;
open Own_types;;
open Lexing;;
open Whattodo;;
open Own_xml;;
open Context_test;;


let print_pos n =
   let s = rhs_start_pos n in
   let e = rhs_end_pos n in
   match get_input_lexbuf () with
   | None ->
       Printf.printf "Line %d: characters %d-%d.\n"
                   s.pos_lnum
                   (s.pos_cnum - s.pos_bol)
                   (e.pos_cnum - s.pos_bol)
   | Some lb ->
     let pos0 = -lb.lex_abs_pos in
     if pos0 < 0 then
       Printf.printf "Line %d: characters %d-%d:\n"
                     s.pos_lnum
                     (s.pos_cnum - s.pos_bol)
                     (e.pos_cnum - s.pos_bol)
     else
      let end_pos = lb.lex_buffer_len - pos0 - 1 in     
      let line_start = ref 0 and line_end = ref 0 in
      for pos = 0 to end_pos do
        if lb.lex_buffer.[pos + pos0] = '\n' then begin
          if s.pos_cnum > pos then incr line_start;
          if e.pos_cnum > pos then incr line_end;
        end
      done;
     let line = ref 0 in
     prerr_string "  "; 
     for pos = 0 to end_pos do
       let c = lb.lex_buffer.[pos + pos0] in
       if !line <= !line_start && c <> '\n' then 
          prerr_char c;
       if c = '\n' then
         begin
         line := !line + 1;
         if !line <= !line_start then
           prerr_string "\n  ";
         end;
     done;
     prerr_char '\n';
     prerr_string "  "; 
     for i = 0 to (s.pos_cnum - s.pos_bol) - 1 do
       prerr_char ' '
     done;
     for i = s.pos_cnum - s.pos_bol to e.pos_cnum - e.pos_bol - 1 do
       prerr_char '^'
     done;
     prerr_newline ();;

type parse_error =
| Rparen | Lparen | Eof | Specname | Propname
| Invalidchar | Commaeof | Twoparam | Twoprop
| Invalidkeyword | Ident | BangDiese | Syntax;;

let print_message i err =
  print_pos i;
  prerr_string "Syntax error";
  begin
  match err with
  | Rparen -> prerr_string ": ')' expected"
  | Lparen -> prerr_string ": '(' expected"
  | Ident -> prerr_string ": identifier expected";
  | Eof -> prerr_string ": End of input expected"
  | Specname -> prerr_string ": species name expected"
  | Propname -> prerr_string ": property name expected"
  | Invalidchar -> prerr_string ": invalid character"
  | Commaeof -> prerr_string ": ',' or end of input expected"
  | Twoparam ->
    prerr_string ": two parameters in sequence, maybe you forgot a ','"
  | Twoprop ->
    prerr_string ": two property in sequence, maybe you forgot a ','"
  | Invalidkeyword -> prerr_string ": invalid keyword"
  | BangDiese -> prerr_string ": '#' or '!' expected"
  | Syntax -> ()
  end;
  prerr_newline ();
  exit 10;;

let type_name_of_uident s i =
  match s with
  | "INT" -> foctint
  | "Self" -> foctself
  | "UNIT" -> foctunit
  | "LIST" -> foctlist
  | "BOOL" -> foctbool
  | "STRING" -> foctstring
  | "RESULT" -> result_type
  | "VERDICT" -> verdict_type
  | "OPTION" -> foctoption
  | "FLOAT" -> foctfloat 
  | _ -> print_message i Invalidkeyword;;

let ident_of_uident s i =
  match s with
  | "FST" -> focfst
  | "NIL" -> focnil
  | "CONS" -> foccons
  | "OR" -> focor
  | "AND" -> focand
  | "PRED" -> focpred
  | "SUCC" -> focsucc
  | "ADD_INT" -> focaddint
  | "SND" -> focsnd
  | "INT_EQUAL" -> focintequal
  | "INT_GT" -> focintgt
  | "SC" -> focstringconcat
  | "CRP" -> foccrp
  | "STRUCT_EQUAL" -> focequal
  | "VUNIT" -> focunit
  | "TRUE" -> foctrue
  | "FALSE" -> focfalse
  | "UNFAILED" -> focunfailed
  | "FAILED" -> focfailed
  | "FOC_ERROR" -> focerror
  | _ -> print_message i Invalidkeyword;;
%}

/* FOCAL Tokens */
%token UNDERSCORE /* "_" */
%token <string> IDENT
%token <int> INT
%token FUN 	/* "fun" */
%token PIPE 	/* "|" */
%token IN 	/* "in" */
%token IF 	/* "if" */
%token MATCH 	/* "match" */
%token AS 	/* "as" */
%token WITH 	/* "with" */
%token REC 	/* "rec" */
%token THEN 	/* "then" */
%token TYPE 	/* "type" */
%token ARO 	/* "@" */
%token <string> UIDENT
%token <string> IIDENT
%token <string> PIDENT
%token SEMICOLON /* ; */
%token ELSE 	/* "else" */
%token LET 	/* "let" */
%token LPAREN  	/* "(" */
%token RPAREN  	/* ")" */
%token CAML  	/* "caml" */
%token IMPORT  	/* "import" */
%token ARROW  	/* "->" */
%token EQ  	/* "=" */
%token COMMA  	/* "," */
%token BANG  	/* "!" */
/* %token <string> CONST */


/**********************/
/* Focal type         */

%token LPAREN
%token RPAREN
%token ARROW
%token STAR
%token COMMA
%token <string> VAR
%token IDENT

/**********************/

/* Tokens */
%token EOF
%token <string> IDENT
%token <int> INT
%token LPAREN   /* "(" */
%token RPAREN   /* ")" */
%token COMMA    /* "," */
%token BANG    /* "!" */
%token DIESE    /* "#" */
%token FUN     /* "fun" */
%token ARROW   /* "->" */
%token <string>STRING  /* strings */
%token <string>LEXERROR  /* all others */

/* Xml */

%token <string>XML_IDENT XML_HEADER BTAG_IDENT ETAG_IDENT BETAG_IDENT
%token LESS GREATER SLASH INTER

/* precedence : */ 
%right FUN LET
%right SEMICOLON
%right IF MATCH PIPE
%right LPAREN
%nonassoc IDENT
%nonassoc DIESE BANG
%nonassoc error
%right ARROW
%right STAR

%start expr_species properties_test topexpr_focal
%start expr_focal type_focal meth_focal expr_xml
%start test_context 
%type <Own_xml.xml_tree> expr_xml
%type <Own_expr.species_test> expr_species
%type <string list> properties_test
%type <Own_expr.toplevel_def> topexpr_focal
%type <Own_expr.methods> meth_focal
%type <Own_expr.myexpr> expr_focal
%type <Own_types.typ> type_focal
%type <Context_test.test_context> test_context
%%

/* ************************************************************************* */
/* *                           Species under test                          * */
/* *                                                                       * */
/* * spc ::= ident [(prm[, ..., prm] )]                                    * */
/* * prm ::= [ident =] prm_collection | prm_entity                         * */
/* * prm_collection ::= ident                                              * */
/* * prm_entity ::= string | int | [ident]!ident | [ident]#ident |         * */
/* *                prm_entity(prm_entity[, ..., prm_entity]) |            * */
/* *                fun ident in type -> prm_entity                        * */
/* ************************************************************************* */

expr_species:
| IDENT LPAREN l_prm RPAREN EOF   { ($1, $3) }
| IDENT EOF                       { ($1, []) }
| IDENT LPAREN l_prm RPAREN error {print_message 5 Eof}
| IDENT LPAREN l_prm error        {print_message 4 Rparen}
| %prec error IDENT error         {print_message 2 Lparen}
| EOF                             {print_message 1 Specname}
| LEXERROR                        {print_message 1 Invalidchar}
| error                           {print_message 1 Specname}

l_prm:
| %prec IDENT prm { [$1] }
| prm COMMA l_prm { $1::$3 }
| prm prm         {print_message 2 Twoparam}
;

prm:
| LEXERROR       {print_message 1 Invalidchar}
| prm_collection { InstPrmColl(None,$1) }
| IDENT EQ prm_collection { InstPrmColl(Some $1, $3) }
| prm_entity     { InstPrmEnt $1 }
;

prm_collection:
| IDENT {$1}
;

l_prm_entity:
| prm_entity                    { [$1] }
| prm_entity COMMA l_prm_entity { $1::$3 }
;

prm_entity:
| STRING                                { expr_string $1} 
| INT                                   { expr_int $1} 
|       BANG IDENT                      { expr_meth focself $2 []} 
| UIDENT BANG IDENT                     { expr_meth $1 $3 []} 
|       DIESE IDENT                     { expr_glob (Prefix(None, $2)) } 
| IDENT DIESE IDENT                     { expr_glob (Prefix(Some $1, $3)) } 
| %prec FUN  FUN IDENT IN type_focal ARROW prm_entity
                                        { expr_fun  $2 $4 $6 }
| prm_entity LPAREN l_prm_entity RPAREN { expr_app $1 $3}  
| %prec error       BANG error          { print_message 2 Ident} 
| %prec error UIDENT BANG error         { print_message 3 Ident} 
| %prec error       DIESE error         { print_message 2 Ident} 
| %prec error IDENT DIESE error         { print_message 3 Ident} 
| %prec error IDENT error               { print_message 2 BangDiese}
| %prec error error                     { print_message 1 Syntax}
;

/* ************************************************************************** */
/* *                             Test context                               * */
/* *                                                                        * */
/* * test_context ::= [let ident = s[(ident, ...)] in test_context ]        * */
/* *                | [let ident = expr in test_context ]                   * */
/* *                | s[(ident, ...)]                                       * */
/* *                                                                        * */
/* * s ::= ident # uident                                                   * */
/* *                                                                        * */
/* * expr ::= string | int | [ident]!ident | [ident]#ident                  * */
/* *        | prm_entity(prm_entity[, ..., prm_entity])                     * */
/* *        | fun ident in type -> prm_entity                               * */
/* *                                                                        * */
/* ************************************************************************** */

ident_uident:
| IDENT { $1 }
| UIDENT { $1 }
;

test_context:
| LET ident_uident EQ species_context IN test_context
                    { tc_add_bc $6 (create_bc $2 (bca_c $4)) }
| LET ident_uident EQ entity IN test_context
                    { tc_add_bc $6 (create_bc $2 (bca_e $4)) }
| species_context   { create_tc [] $1 }
| EOF               {print_message 1 Specname}
| %prec error error {print_message 1 Syntax}

species_context:
| species_name { create_sc $1 [] } 
| species_name LPAREN test_context_param_list RPAREN { create_sc $1 $3 } 

species_name:
| IDENT DIESE UIDENT {create_species_name $1 $3};;

test_context_param_list:
| ident_uident { [$1] }
| ident_uident COMMA test_context_param_list { $1::$3 }

entity:
| STRING                                { expr_string $1}
| INT                                   { expr_int $1} 
|       BANG IDENT                      { expr_meth focself $2 []} 
| UIDENT BANG IDENT                     { expr_meth $1 $3 []} 
|       DIESE IDENT                     { expr_glob (Prefix(None, $2)) } 
| IDENT DIESE IDENT                     { expr_glob (Prefix(Some $1, $3)) } 
| %prec FUN  FUN IDENT IN type_focal ARROW prm_entity { expr_fun  $2 $4 $6 }
| prm_entity LPAREN l_prm_entity RPAREN { expr_app $1 $3}  
| %prec error       BANG error          {print_message 2 Ident} 
| %prec error UIDENT BANG error         {print_message 3 Ident} 
| %prec error       DIESE error         {print_message 2 Ident} 
| %prec error IDENT DIESE error         {print_message 3 Ident} 
| %prec error IDENT error               {print_message 2 BangDiese}
| %prec error error                     {print_message 1 Syntax}
;


/* ************************************************************************* */
/* *                   Properties under test                               * */
/* *                                                                       * */
/* * prop ::= ident[, ... , ident]                                         * */
/* *                                                                       * */
/* ************************************************************************* */

properties_test:
| IDENT                       { [$1] }
| IDENT COMMA properties_test { $1::$3 }
| LEXERROR                    {print_message 1 Invalidchar}
| %prec error IDENT error     {print_message 2 Commaeof}
| IDENT IDENT                 {print_message 2 Twoprop}
| %prec error error           {print_message 1 Propname}
;


/* ************************************************************************* */
/* *                   Focal like toplevel expression                      * */
/* *                                                                       * */
/* * topexpr ::= let ident = expr |                                        * */
/* *          let ident in type = expr |                                   * */
/* *          let ident ([var, ..., var]) in type = expr |                 * */
/* *          expr |                                                       * */
/* *          type uident = type_def |                                     * */
/* *          type ident = type_def                                        * */
/* *                                                                       * */
/* * type_def ::=  uident in type ; [type_def]                             * */
/* ************************************************************************* */

topexpr_focal :
  LET IDENT EQ expr_focal EOF 
  { ObjToplet ($2, None, $4) }
| LET IDENT IN type_focal EQ expr_focal EOF 
  { ObjToplet ($2,Some $4, $6) }
| LET IDENT LPAREN list_var RPAREN IN type_focal EQ expr_focal EOF 
  { ObjToplet ($2,Some $7, $4 $9) }
| LET IDENT LPAREN RPAREN IN type_focal EQ expr_focal EOF 
  { ObjToplet ($2,Some $6, $8) }
| expr_focal
  { ObjTopcall $1 }
| TYPE UIDENT EQ expr_type_def
   {ObjType(type_name_of_uident $2 2,$4) }
| TYPE IDENT EQ expr_type_def
   {ObjType($2,$4) }
| error { print_message 1 Syntax }
| LEXERROR   {print_message 1 Invalidchar}
;

expr_type_def:
| UIDENT SEMICOLON expr_type_def
   {($1,[])::$3} 
| UIDENT LPAREN l_type_focal RPAREN SEMICOLON expr_type_def
   {($1,$3)::$6} 
| UIDENT LPAREN l_type_focal RPAREN SEMICOLON 
   {[$1,$3]}
;


/* ************************************************************************* */
/* *                        Focal like expression                          * */
/* *                                                                       * */
/* * expr ::= fun ident in ( type ) -> expr |                              * */
/* *          if expr then expr else expr |                                * */
/* *          expr_simpl |                                                 * */
/* *          let ident = expr in expr |                                   * */
/* *          ( expr ) |                                                   * */
/* *          [ident]#ident |                                              * */
/* *          [ident]!ident[(expr, ..., expr)] |                           * */
/* *          @uident |                                                    * */
/* *          #uident |                                                    * */
/* *          expr([expr, ..., expr]) |                                    * */
/* *          [ident]!ident[(expr, ..., expr)] |                           * */
/* *          caml import ident |                                          * */
/* *          match expr with cases |                                      * */
/* *          string                                                       * */
/* * cases ::= \| pattern -> expr ... \| pattern -> expr                   * */
/* * pattern ::= | @uident[(ident, ..., ident)]                            * */
/* *             | @ident[(ident, ..., ident)]                             * */
/* *                                                                       * */
/* ************************************************************************* */

/* Les expressions générales (utilisé dans les définitions toplevel et dans les
méthodes */
expr_focal:
  %prec FUN FUN IDENT ARROW expr_focal
  { expr_fun_notyp $2 $4 }
|  %prec FUN FUN IDENT IN LPAREN type_focal RPAREN ARROW expr_focal
  { MFun($2, Some $5, $8) }
| %prec IF IF expr_focal THEN expr_focal ELSE expr_focal
  { MIfte($2,$4,$6) }
| expr_simpl { $1 }
| %prec LET LET IDENT EQ expr_focal IN expr_focal
    { MVarloc(false, ($2, None),$4,$6) }
| %prec LET LET REC IDENT EQ expr_focal IN expr_focal
    { MVarloc(true, ($3, None),$5,$7) }
| LPAREN expr_focal RPAREN
    { $2 }
| DIESE IDENT
    { MGlob_id(Prefix(None, $2)) }
| IDENT DIESE UIDENT
    { MGlob_id(Prefix(Some $1, $3)) }
| DIESE IIDENT
    { MGlob_id(Infix($2)) }
| IDENT DIESE PIDENT
    { MGlob_id(Prefix(Some $1, $3)) }
| DIESE PIDENT
    { MGlob_id(Prefix(None, $2)) }
| %prec SEMICOLON expr_focal SEMICOLON expr_focal
    { expr_seq $1 $3 (use_seq_function ()) }
| ARO UIDENT
    { MGlob_id(ident_of_uident $2 2) }
| DIESE UIDENT
    { MGlob_id(Prefix(None, $2)) }
| IDENT DIESE IDENT
    { MGlob_id(Prefix(Some $1, $3)) }
| CAML IMPORT IDENT
    { MCaml_def($3) }
| MATCH expr_focal WITH l_case
{MMatch(($2, None),$4) }
| %prec IDENT BANG IDENT
    { MMeth(None, $2) }
/*     { MApp(MMeth(MId focself, $2) , []) } */
| %prec UIDENT UIDENT BANG IDENT
    { MMeth(Some $1, $3) }
/*     { MApp(MMeth(MId $1, $3) , []) } */
/*
| %prec IDENT BANG IDENT LPAREN l_expr_focal RPAREN
    { MApp(MMeth(MId focself, $2) , $4) }
| %prec UIDENT UIDENT BANG IDENT LPAREN l_expr_focal RPAREN
    { MApp(MMeth(MId $1, $3) , $5) }
*/
| expr_focal LPAREN l_expr_focal RPAREN
{ MApp($1, $3) }
| expr_focal LPAREN RPAREN
{ MApp($1, []) }
| STRING {MString $1}
| error { print_message 1 Syntax }
| LEXERROR   {print_message 1 Invalidchar}
;

l_expr_focal:
|  expr_focal { [$1, Own_types.TAtom(Some focbasics, "unit")] }
|  expr_focal COMMA l_expr_focal { ($1, Own_types.TAtom(Some focbasics, "unit")) :: $3 }
;

l_ident:
| IDENT { [$1] }
| IDENT COMMA l_ident { $1::$3 }
;

l_case :
| %prec MATCH   PIPE pattern ARROW expr_focal
   { [let a,b = $2 in a,b,$4] }
| %prec MATCH   PIPE pattern ARROW expr_focal l_case
   { (let a,b = $2 in a,b,$4)::$5}

pattern:
| ARO UIDENT
  {ident_of_uident $2 2, [] }
| UIDENT
  { Prefix(None, $1), [] }
| %prec UIDENT ARO UIDENT LPAREN l_ident RPAREN
  { ident_of_uident $2 2, List.map (fun e -> Some e) $4 }
| %prec UIDENT UIDENT LPAREN l_ident RPAREN
  { Prefix(None, $1), List.map (fun e -> Some e) $3 }
;


/* ************************************************************************* */
/* *                           Focal like methods                          * */
/* *                                                                       * */
/* * expr ::= let [rec] ident[([var, ..., var])] in type = expr            * */
/* ************************************************************************* */

meth_focal:
| LET IDENT IN type_focal EQ expr_focal     {meth_create $2 $4 $6 false}
| LET REC IDENT IN type_focal EQ expr_focal {meth_create $3 $5 $7 true}
| LET IDENT LPAREN list_var RPAREN IN type_focal EQ expr_focal
                                            {meth_create $2 $7 ($4 $9) false}
| LET IDENT LPAREN RPAREN IN type_focal EQ expr_focal
                                            {meth_create $2 $6 $8 false}
| LET REC IDENT LPAREN list_var RPAREN IN type_focal EQ expr_focal
                                            {meth_create $3 $8 ($5 $10) true}
| error { print_message 1 Syntax }
| LEXERROR   {print_message 1 Invalidchar};

/* ************************************************************************* */
/* *                           Variable definition                         * */
/* *                                                                       * */
/* * var ::= ident in type                                                 * */
/* ************************************************************************* */

/* A list of variables with type separated by commas. */
list_var:
  IDENT IN type_focal { fun e -> MFun($1, Some $3,e) }
| IDENT IN type_focal COMMA list_var { fun e -> MFun($1, Some $3,$5 e)}
;

/* ************************************************************************* */
/* *                           Simple expression                           * */
/* *                                                                       * */
/* * expr_simpl ::= int | ident                                            * */
/* ************************************************************************* */

expr_simpl:
| INT
    { MInt($1) }
| IDENT
    { MVar($1, None) }
;

/* ************************************************************************* */
/* *                           Focal types                                 * */
/* *                                                                       * */
/* * type ::= uident | ident | 'ident | type -> type | ( type ) |          * */
/* *          ident ( type ) | uident ( type) | type * type                * */
/* ************************************************************************* */

type_focal:
  | ARO UIDENT                        { TAtom(None, type_name_of_uident $2 1) }
  | UIDENT                            { TAtom(None, $1) }
  | IDENT                             { TAtom(None, $1) }
  | VAR                               { TAtom(None, $1) }
  | type_focal ARROW type_focal       { TFct($1, $3) }
  | LPAREN type_focal RPAREN          { $2 }
  | IDENT LPAREN l_type_focal RPAREN  { TPrm(None, $1, $3) }
  | ARO UIDENT LPAREN l_type_focal RPAREN { TPrm(None, type_name_of_uident $2 1, $4) }
  | UIDENT LPAREN l_type_focal RPAREN { TPrm(None, $1, $3) }
  | type_focal STAR type_focal        { TProd($1,$3) }
  | error                             { print_message 1 Syntax };
 
l_type_focal:
  | type_focal { [$1] }
  | type_focal COMMA l_type_focal {$1::$3};


/* ************************************************************************* */
/* *                                   XML                                 * */
/* *                                                                       * */
/* * type ::= uident | ident | 'ident | type -> type | ( type ) |          * */
/* *          ident ( type ) | uident ( type) | type * type                * */
/* ************************************************************************* */


expr_xml:
 | xml_header tag   { $2 }
 | LEXERROR {print_message 1 Invalidchar}
 | error    {print_message 1 Invalidkeyword}
 | EOF      {print_message 1 Invalidchar}

xml_header:
 | XML_HEADER { () }
 | XML_HEADER xml_header { () }
;

tag_list:
  | tag { [$1] }
  | tag tag_list { $1::$2 }
;

tag:
  | BTAG_IDENT tag_list  ETAG_IDENT
            { if $1 = $3 then Node($1,$2) else failwith "erreur" }
  | BTAG_IDENT XML_IDENT ETAG_IDENT
            { if $1 = $3 then Leave($1,Some $2) else failwith "erreur" }
  | BTAG_IDENT IDENT     ETAG_IDENT
            { if $1 = $3 then Leave($1,Some $2) else failwith "erreur" }
  | BTAG_IDENT ETAG_IDENT
            { if $1 = $2 then Leave($1,None) else failwith "erreur" }
  | BETAG_IDENT
            { Leave($1,None) }
;

