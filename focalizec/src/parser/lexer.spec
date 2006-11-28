High level specification of the lexer (no ;; after each definition. *)
======================================================================

Three kinds of chars:

spaces ::= [ ' ' '\t' '\n' '\r' ]

punctuation ::= [ ',' ';' '.' '(' ')' '[' ']' '{' '}' ]

other ::=

(*******
Boolean operators: && || ~| (almost as in Caml)
Proposition operators: /\ \/ -> <-> ~ (as in Coq)

Infixes généraux:
- une suite de symboles est supposée être un identificateur infix
  ex: + ++ ** -><- :=

- la précédence de l'infixe correspondant est donnée par le ou les premiers
caractères du symbole; par exemple ++ a la même précédence (et règle
d'associativité que +), **+ a la même précédence que **.

- Tout token commençant par un caractère symbolique (i.e. non alpha-numérique)
 définit une priorité dans le parser. Exemple @ définit une priorité, de même
 que : et même , ou ;.

(0) Les identificateurs alphanumériques, noms propres et noms communs (!)

Start_L_Ident ::= [_a-z]
Start_U_Ident ::= [A-Z]

Continue_Ident ::= [_a-zA-Z0-9]

L_Ident := Start_L_Ident (Continue_Ident*)
U_Ident := Start_U_Ident (Continue_Ident*)

(1) Les identificateurs infixes, noms d'opérations binaires

Start_Infix ::= + - * ^ @ % & | , : = \ / < > #
         Pas ' ni " qui sont des délimiteurs de chaînes et caractères

Continue_Infix := Start_Infix
                | Start_Prefix
                | Continue_Ident

Infix ::= Start_Infix (Continue_Infix*)

Rq: End_Infix ::= SPACE  (::= blanc tab newline) ( ) [] {}
*******)
