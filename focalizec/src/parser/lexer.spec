#**********************************************************************#
#                                                                      #
#                        FoCaL compiler                                #
#                                                                      #
#            Fran�ois Pessaux                                          #
#            Pierre Weis                                               #
#            Damien Doligez                                            #
#                               LIP6  --  INRIA Rocquencourt           #
#                                                                      #
#  Copyright 2008 LIP6 and INRIA                                       #
#  Distributed only by permission.                                     #
#                                                                      #
#**********************************************************************#

# $Id: lexer.spec,v 1.7 2009-04-10 07:06:48 weis Exp $

High level specification of the Focalize lexer
==============================================

Could be use as a set of drafts for the documentation.

2009/04/02:
===========
Documentation with levels for file header and section titles (addition of
inter phrases documentation tokens and phrases).

2009/02/12:
===========
Adding special rules for (*** starting lines.

2009/01/22:
===========
Documentation tokens have to be revisited to add a tag to them.

let annotation_tag = '{@' [^ '}']* '}'

In the main lexer:
...
  (* Documentation *)
| "(**" (annotation_tag? as tag)
  { reset_documentation_buffer ();
    documentation_start_pos :=
      Some (lexbuf.lex_start_p, lexbuf.lex_curr_p);
    documentation lexbuf;
    begin match !documentation_start_pos with
    | Some (start_pos, _) -> lexbuf.lex_start_p <- start_pos
    | _ -> assert false end;
    DOCUMENTATION (tag, get_stored_documentation ()) }
...

2009/01/21:
===========
Conformance to reference manual (!)
Sharing of escaped characters as a regular expression used where we need
escapes. That way escaped chars are always the same.

2008/10/12:
===========
Revisit and test '`' starting lexems.
Generalize treatment of [] and () and :: in parser.

2008/10/20: infix version of prefix ident
=========================================
Should we use `ident` (and also `Ident`) to define and use an infix (regular)
alphanumerical ident ?
This suggests to remove ` from symbolic uppercase infixes.

2008/10/17: Classifying tokens.
===============================

First trial to understand the implications for the lexer of the necessary
identifier classifications for the parser. For instance, how the lexer must
deal with stratification between module and regular value names ? Or
constructor versus regular value names ? How can the lexer (properly) deal
with ``fixity'' status of names (infix/prefix/always prefix) ? How can the
lexer deal with the big world of names division (alphanumerical/symbolic) ?
Last but not least, how to deal with the cartesian product of all those
(necessary) categorization ``plans'' ?

To fix the vocabulary, let's name:
 - ``Symbolism'' the big name partition between alphanumeric and symbolic names,
 - ``Fixity'' the syntactic partition between infix/prefix/always prefix of names,
 - ``Syntaxity'' the syntactic partition between value/constructor, type/module ,
    collection/value, and function/species syntactic category of names.

Then we have to define how the lexer must divide tokens into those various
partitions, i.e. give them a value in the cartesian product

 Symbolism x Fixity x Syntaxity.

In the first place, we need to define those various partitions to get a true
partition (in the mathematical sense) of the set of names.

For the time being in Focalize, the partitions are more or less defined as:
 - Symbolism is ``purely alphanumerical''/``symbolic''.
 - Fixity is ``infix''/``always prefix''/``regular''.
 - Syntaxity is ``lowercase''/``uppercase''.

Simple examples:
 - ``x'' is (``purely alphanumerical'', ``regular'', ``lowercase'').
 - ``True'' is (``purely alphanumerical'', ``regular'', ``uppercase'').

 - ``+'' is ``symbolic'' and ``infix''; however, its syntaxity is not
   explicit in the lexer/parser (may be we can treat this as an extra ``ad
   hoc'' class of syntaxity ? Mmm, ugly!).

Trickier:
 - ``::'' is (``symbolic'', ``infix'', ``uppercase'').
   In effect:
   * ``::'' is not alphanumerical, hence is ``symbolic'';
   * ``::'' is a constructor, hence is ``uppercase'';
   * ``::'' is an infix, hence is ``infix''.

 - ``()'' and ``[]'' are (``symbolic'', ``regular'', ``uppercase'').

 - ``+'' is (``symbolic'', ``infix'', ``lowercase'').
   In effect:
   * ``+'' is not ``uppercase'' (it cannot be used as a constructor name),
     hence it  lowercase. For the same reason:
 - ``~'' is (``symbolic'', ``always prefix'', ``lowercase'').

Relationship between those lexical partitions is not explicit if always
clear; furthermore, it has some ``rugosity'' to be worked out.

High level specification of the lexer (no ;; after each definition. *)
======================================================================

Three kinds of chars:

spaces ::= [ ' ' '\t' '\n' '\r' ]

punctuation ::= [ ',' ';' '.' '(' ')' '[' ']' '{' '}' ]

other ::=

2008/10/14 (PW)
----------
The lexer also scans ( [infix] ) to return the internal ident [infix] (and
similarly for prefix idents).
We certainly have to decide which infixes are possibly upper case / lower
case idents.

- () is treated especially: it is uppercase (a constant constructor for unit)

- [] is treated especially: it is uppercase (an infix binary constructor for lists)
- :: is uppercase: a constructor for lists.

Generalization:
- [...] is uppercase: a constant constructor.
  (e.g. [=], [!], or [_|_] for bottom)
- :...: is uppercase: an infix binary constructor.
  (e.g. :U: or :+: could be used as a binary infix constructor).

- ( + ) is lowercase: an operator for arithmetics.
    - * / ** < > =
    ^ @

- && || ~| |<>| are lowercase

The lexer scans documentation comments (** ... *)

(*******
Boolean operators: && || ~| (almost as in Caml)
                   2008/10/08 adding |<>| for xor
                   (to suggest ``or but not equal'', hence || but <>)
Proposition operators: /\ \/ -> <-> ~ (as in Coq)
                       2008/10/17 I suggest to use |#| for xor ?

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

let rec foo (x) =
  (Module#)foo (x + 1)

Peut s'�crire:
let rec foo (x) =
  (Module.)foo (x + 1)

(Le # devient foo et il est implicite s'il existe un module ouvert d�finissant
foo).

Collection and species Start with an uppercase letter.

Acces to collection is
  Col!ident

Dans une esp�ce, on d�finit 

species ... (Col is Ring, v in Col)
let rec foo (x) =
  Col!foo (x + val) + foo (val)
end
