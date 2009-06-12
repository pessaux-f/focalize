#**********************************************************************#
#                                                                      #
#                        FoCaL compiler                                #
#                                                                      #
#            François Pessaux                                          #
#            Pierre Weis                                               #
#            Damien Doligez                                            #
#                               LIP6  --  INRIA Rocquencourt           #
#                                                                      #
#  Copyright 2008 LIP6 and INRIA                                       #
#  Distributed only by permission.                                     #
#                                                                      #
#**********************************************************************#

# $Id: parser.spec,v 1.19 2009-06-12 07:42:23 weis Exp $

High level specification of the Focalize lexer
==============================================

Could be use as a set of drafts for the documentation.

12/06/09: Getting rid of mk_cons mk_nil
=======================================
There is no reason why to hard wire the scope of constructors [] and :: to
file "basics" from within the parser. This should be treated the same as all
other value constructors, in particular redefinable by the user.

We had:

  | pattern COLON_COLON pattern
    { mk (P_constr (mk_cons (), [ $1; $3 ])) }
  | pattern IUIDENT pattern
    { mk (P_constr (mk_global_constructor_ident None (Vuident $2), [ $1; $3 ])) }

this should no read

  | pattern COLON_COLON pattern
    { mk (P_constr (mk_global_constructor_ident None (Vuident "::"), [ $1; $3 ])) }
  | pattern IUIDENT pattern
    { mk (P_constr (mk_global_constructor_ident None (Vuident $2), [ $1; $3 ])) }



07/05/08: Irrefutable patterns and access to component of tuples.
=================================================================

We would like to bind a tuple to an expression in one let.
It means we want to bind patterns instead of bound_vname.
E.g.
 let (x, y) = pair
 let (x, _) = pair
 let (_, y) = pair
May be let _ = or let () = ?

The rule
  | bound_vname EQUAL logical_expr termination_proof_opt
    { mk {
        b_name = $1; b_params = []; b_type = None;
        b_body = Parsetree.BB_logical $3;
        b_termination_proof = $4;
      }
    }
becomes
  | bound_pattern EQUAL logical_expr termination_proof_opt
    { mk {
        b_name = $1; b_params = []; b_type = None;
        b_body = Parsetree.BB_logical $3;
        b_termination_proof = $4;
      }
    }
and we define:

bound_pattern:
  | bound_vname {mk (BP_var $1)}
  | UNDERSCORE { mk (BP_wild)}
  | LPAREN bound_pattern COMMA bound_pattern_comma_list RPAREN
    { mk (BP_tuple ($2 :: $4)) }*/
;
bound_pattern_comma_list:
  | bound_pattern { [ $1 ] }
  | bound_pattern COMMA bound_pattern_comma_list { $1 :: $3 }
;

In parsetree.mli we must add:
type bound_pattern = bound_pattern_desc ast
and bound_pattern_desc =
-- Unless we can prove it exhaustive
-- | BP_const of constant
  | BP_var of vname
-- Useless ?
--  | BP_as of bound_pattern * vname
  | BP_wild
-- Unless we can prove it exhaustive
--  | BP_constr of constructor_ident * bound_pattern list
-- Unless we can prove it exhaustive
--  | BP_record of (label_ident * bound_pattern) list
  | BP_tuple of bound_pattern list
-- Is it useful ?
  | BP_paren of bound_pattern
;;

04/05/08

No macros in the parser, nor in the lexer!

\subsection{Definitions - usage of syntactic constructs}

As a general rule, the syntactic construct for the definition of a concept
should be as similar as possible to the syntactic construct associated to the
usage of the concept.

\subsection{Type definitions}

As explained above, type definitions should be as similar as possible to their
usage syntactic pattern.

Before, the definition of a polymorphic list type was:

\begin{verbatim}
type list a =
  caml_link list;
  Nil ([]) in list(a);
  Cons ((::)) in a -> list(a) -> list(a);
;;
\end{verbatim}

We propose a simpler more regular syntax:
\begin{verbatim}
type list (a) =
  | Nil
  | Cons (a, list (a))
;;
\end{verbatim}

Example: a more complex definition of a {\tt tree} type constructor with 2 variables.
\begin{verbatim}
type tree (a, b) =
  | Empty (b)
  | Node (a, b, tree (a, b))
;;
\end{verbatim}

\subsection{External directives}

External directives are used to link a Focal concept to external languages or
systems for the purpose of compilation, proof checking, proof generation, or
documentation.

One proposition is to define external clauses that systematically link
identifiers to both Caml and Coq (in that order), using character strings.

Example:
\begin{verbatim}
external Cons = "(::)" "cons";;
\end{verbatim}

Since there may be more than one language or system to link with, the external
clause may be a bit more structured, as in

\begin{verbatim}
external Cons =
 | caml -> "( :: )"
 | coq -> "cons";;
\end{verbatim}

Also we may want to differentiate the concepts bound (and to be more similar to
the specifications of identifiers):

\begin{verbatim}
external type list =
 | caml -> "list"
 | coq -> "List.list"
;;

external val Cons =
 | caml -> "( :: )"
 | coq -> "cons";;
\end{verbatim}

\subsection{External definitions for types}
An external directive for a type definition links the type and its
constructors or labels to external languages for the compilation and proofs.


2007/10/24

External type definition for sum types:

\begin{verbatim}
(* Focalize binding of the foc_list type constructor and its associated
   contructors or labels. *)
type foc_list ('a) =
  internal
  | Nil
  | Cons ('a, foc_list ('a))

  external
  | caml -> "'a list"
  | coq -> "List.list ['a]"

  and Nil =
  | caml -> "[]"
  | coq -> "nil"

  and Cons =
  | caml -> "( :: )"
  | coq -> "cons"
;;
\end{verbatim}

External definitions for record types:

\begin{verbatim}
(* Focalize binding of the foc_record type constructor and its associated
   contructors or labels. *)
type foc_record ('a) =
  internal
  { hcode : int; contents : 'a; }

  external
  | caml -> "'a Focvasives.foc_record"
  | coq -> "Focq.foc_record ['a]"

  and hcode =
  | caml -> "Focvasives.fv_hashing_code"
  | coq -> "Focq.hc"

  and contents =
  | caml -> "Focvasives.fv_contents"
  | coq -> "Focq.conts"
;;
\end{verbatim}


External type definition for abbreviation types:

\begin{verbatim}
type foc_pair ('a, 'b) =
  [internal] (* No internal definition: type is fully abstract. *)
  external
  | caml -> "('a, 'b)"
  | coq -> "{fst : 'a; snd : 'b}"
;;

type foc_diag ('a) =
  internal ('a * 'a)
  external
  | caml -> "('a * 'a)"
  | coq -> "{ fst : 'a ; snd : 'a}"
;;

type int =
  external
  | caml -> "int"
  | coq -> "Z"
;;
\end{verbatim}



\subsection{External directives for values}

We could have chosen:

\begin{verbatim}
let sc = (external : string -> string -> string);;

external value sc =
  | caml -> "concat"
  | coq -> "^"
;;
\end{verbatim}

But we prefer something more similar to what is widely accepted for external
type definitions:

\begin{verbatim}
let sc =
  internal string -> string -> string
  external
  | caml -> "Pervasives.( ^ )"
  | coq -> "Focq.catenate"
;;
\end{verbatim}



\subsection{How to explicit "externally" the types internally embedded in the FoCaL algebra. Ideas for perhaps later.}
(** There is a predefined ``module'' (file in fact) named Predefined,
    Internal which is known by the compiler. *)

type int =
  internal Internal#int
  external
  | caml -> "int"
  | coq -> "Z"
;;

(** Internal#t ne peut apparaître que dans une clause internal et ne peut
faire référence qu'à un constructeur de l'algèbre des types de base. *)

type t =
  internal Internal#int
  external
  | caml -> "int"
  | coq -> "Z"
;;

(** Crée dans l'environnement
 - une liaison de t vers Internal#int dans l'environnement de typage du
   programme: similaire à une abbreviation de Caml,
 - une liaison de Internal#int vers t dans l'environnement ``Internal'',
celui qui dit que 1 est de type (assoc Internal#int ``environnement
Internal'').
 - la (re)définition d'une liaison pour Internal#int est interdite (sauf une
fois si une option non documentée du compilateur ?).


*)


(* The type t is concrete. *)

(1 : t)

type t =
  internal (* Fully abstract *)
  external
  | caml -> "int"
  | coq -> "Z"
;;
(** Les valeurs de t sont abstraites. Et donc: *)

(1 /: t);;

(** Defining external vision of the internal type algebra *)

Internal#bool
Internal#int
Inutile de définir car sémantique bien comprise Internal#``->''
Inutile de définir car sémantique bien comprise Internal#``*''



Où est vissé le type int dans le compilo ?

Dans l'algèbre de type ? Pas vraiment: c'est un constructeur constant comme
un autre.

En fait on visse le fait que ST_construct "int" est connu du compilateur au
moment où on type les constantes entières.

Dans le aprser, à la place de générer une constante E_const (C_int "123"), on
pourrait générer un appel à une fonction de conversion dans le parser. Par
exemple, en appelant cette fonction int_litteral, on génèrerait:

INT s -> E_app (mk (E_var "int_litteral"), mk (C_string s))

Le type int serait ainsi ``dévissé'' du typeur des constantes.

La fonction int_litteral devrait être définie en focalize comme external
(sans définition interne):

let int_litteral =
  internal: string -> integer
  external
  | caml -> "Pervasives.int_of_string"
  | coq -> ""
;;

L'intérêt annexe est la possibilité de redéfinir les constantes de base,
simplement en redéfinissant une fonction utilisateur. Par exemple
let int_litteral =
  internal: string -> big_integer
  external
  | caml -> "Num.num_of_string"
  | coq -> "??"
;;

Par extension on pourrait aussi avoir bool_litteral, char_litteral ...,
pour dévisser les constantes de type correspondantes.

Dans le fichier de définition des concepts internes on définirait la liaison
entre le type connu du compilateur et le type connu de l'environnement
utilisateur par une définition de type external avec un champ internal
dénotant un ``built-in'' (un identificateur du pseudo-``module'' Internal):




\section{Comments}

\subsection{Documentation, alias structured comments}

Structured comments

\begin{itemize}
\item must be integrated in the parsetree,
\item must be parsed.
\end{itemize}

- on a decidé d'abandonner l'ssociation entre une méthode FoCaL et un nom
  externe pour mathml, openmath ou LaTeX dans le commentaire de la méthode.

- Dans l'entete des fichiers on fait un binding du genre

   @author(Moi)
   @title(Boolean Algebras)
   @descrition(Boolean Alberas are a model for abstract classical logics)
   @extern(
          "mathml" | "plus" -> "<plus/>', "mult" -> "<times/>";
          "tex "   | "plus" -> "+". "mult -> "";
          )

 - tous les commentaires sont à arguments: @title(), @description() ...

 - dans les commentaires de méthode on autorise du texte libre avec la
   possibilité de mettre des @focal(plus(x,y)) qui sont vérifiés du genre

   (**
       In a boolean algebra @focal(!plus(x,y)) denotes
       logical or of @focal(x) and @focal(y)

     *)

 - la portée des commentaires d'une méthode est celle de l'héritage normal. La
   notation est celle liée au fichier associé à l'héritage normal.

 - Il faut penser à ce qu'on met dans du texte libre et aux caractères spéciaux
   XML.

The general parser only defines where documentation parts can be written.

A documentation text must start with "(**" and ends with "*)".

A documentation text has only one limitation it cannot contains non escaped
occurrence of the two character string "*)"  (in case we want this two chars,
we must write a \ (as usual in focal), namely "*\)" ).

For the lexer it is a token Documentation of string.

The documentation text is parsed afterwards by dedicated parser(s). The
dedicated parser should run after the normal parser and before the
type-checker. A simple but correct implementation for the documentation parser
is identity.

What could be documented ? Where to put the documentation texts ?

Documentation just before the keyword that introduces the construction.

What could be documented:

sig, property, rep, letprop, theorem, proof (and proof steps), let, species,
collection.

Documentation tools should mark especially the assumed occurrences in proof.

\subsection{Unstructured comments}

Unstructured comments

\begin{itemize}
\item may appear anywhere in the source code,
\item are ignored and thrown during the lexing phase of parsing.
\end{itemize}

Uniline comments:
- start by -- and
- end by a new line


In proofs.

let (x : self) = 1;;

theorem foo : enonce = preuve;;


\section{Pensées pour éviter les champs mutables de type dans l'ast}
scope : 'a ast -> untyped ast
infer : untyped ast -> Types.simple_type ast
parse : string -> untyped ast
pass :  Types.simple_type ast -> quelconque ast

(*
Suite =E0 notre discussion de aujourd'hui concernant notre
discussion d'un jour ancien, voil=E0 ce que je voudrais en Focal.

Si j'ai

species toto (a1 is a, b1 is b, c1 is c) =3D
...

end

Si je veux d=E9finir une esp=E8ce tata qui h=E9rite de toto et qui prend
les m=EAmes param=E8tres que toto, pouvoir faire

species tata(params of toto)
inherits toto(params of toto) =3D
...
end

Mieux, si je veux en plus rajouter un param=E8tre, je pourrais faire

species tutu(params of toto, d1 is d, e1 is e)
inherits toto(params of toto) =3D
end

Plus mieux. Si je veux param=E8tre mon esp=E8ce titi par une collection =20=

toto :

species titi(params of toto, t1 is toto(params of toto), d1 is d, e1 =20
is e)
inherits tutu(params of toto,d1)
end

Encore plus mieux, je pourrais faire.

species tyty(params of toto and tutu))
inherits toto(params of toto),
	       tutu(params of tutu) =3D

end

Encore plus mieux, si je peux faire toute combinaison tordue et =20
grotesque de tous
les cas de figures pr=E9sent=E9s ci-dessus, et que =E7a marche :-)

Bien s=FBr, je vois =E7a comme une aide syntaxique, c'est-=E0-dire que =
si =20
il y a le moindre
conflit de nom de variables entre les diff=E9rents "params of", le =20
compilo a le droit de
me dire gentiment d'aller faire du Java parce que visiblement Focal =20
c'est trop
compliqu=E9 pour moi.

Voil=E0, tu sais tout. Si tu as une question, tu peux me la poser :-D

Merci

Charles


--
Charles Morisset
UPMC - LIP6 - Equipe SPI
104 av du Pr=E9sident Kennedy
F-75016 Paris, France

+ 33 6 65 13 93 06
+ 33 1 44 27 51 28

http://www-spi.lip6.fr/~morisset/
*)
