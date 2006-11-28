High level specification of the lexer (no ;; after each definition. *)
======================================================================

Three kinds of chars:

spaces ::= [ ' ' '\t' '\n' '\r' ]

punctuation ::= [ ',' ';' '.' '(' ')' '[' ']' '{' '}' ]

other ::=

(*******
Boolean operators: && || ~| (almost as in Caml)
Proposition operators: /\ \/ -> <-> ~ (as in Coq)

Infixes g�n�raux:
- une suite de symboles est suppos�e �tre un identificateur infix
  ex: + ++ ** -><- :=

- la pr�c�dence de l'infixe correspondant est donn�e par le ou les premiers
caract�res du symbole; par exemple ++ a la m�me pr�c�dence (et r�gle
d'associativit� que +), **+ a la m�me pr�c�dence que **.

- Tout token commen�ant par un caract�re symbolique (i.e. non alpha-num�rique)
 d�finit une priorit� dans le parser. Exemple @ d�finit une priorit�, de m�me
 que : et m�me , ou ;.

(0) Les identificateurs alphanum�riques, noms propres et noms communs (!)

Start_L_Ident ::= [_a-z]
Start_U_Ident ::= [A-Z]

Continue_Ident ::= [_a-zA-Z0-9]

L_Ident := Start_L_Ident (Continue_Ident*)
U_Ident := Start_U_Ident (Continue_Ident*)

(1) Les identificateurs infixes, noms d'op�rations binaires

Start_Infix ::= + - * ^ @ % & | , : = \ / < > #
         Pas ' ni " qui sont des d�limiteurs de cha�nes et caract�res

Continue_Infix := Start_Infix
                | Start_Prefix
                | Continue_Ident

Infix ::= Start_Infix (Continue_Infix*)

Rq: End_Infix ::= SPACE  (::= blanc tab newline) ( ) [] {}
*******)
