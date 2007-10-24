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
 | Caml -> "( :: )"
 | Coq -> "cons";;
\end{verbatim}

Also we may want to differentiate the concepts bound (and to be more similar to
the specifications of identifiers):

\begin{verbatim}
external type list =
 | Caml -> "list"
 | Coq -> "List.list"
;;

external val Cons =
 | Caml -> "( :: )"
 | Coq -> "cons";;
\end{verbatim}

\subsection{External directives for types}

An external directive for a type definition links the type and its
constructors or labels to external languages for the compilation and proofs.

External directives are (for the time being) inter-mixted with the type
definition. We propose to separate them. A Focal type definition becomes
a simple focal type definition, coupled with a set of external directives:

We now get:

\begin{verbatim}
type foc_list (a) = external
  | Nil
  | Cons (a, foc_list (a))
;;

external type foc_list =
 | Caml -> "list"
 | Coq -> "List.list"
;;

external value Nil =
 | Caml -> "[]"
 | Coq -> "nil";;

external value Cons =
 | Caml -> "( :: )"
 | Coq -> "cons";;

type foc_int = external;;
external type foc_int =
 | Caml -> "int"
 | Coq -> "Z"
;;
\end{verbatim}

\subsection{External definitions for types}

2007/09/26 Autre id�e: one time definition.

We need:

\begin{verbatim}
(* Focalize binding of the foc_list type constructor and its associated
   contructors or labels. *)
type foc_list (a) = external
  | Nil
  | Cons (a, foc_list (a))
;;

(* External binding of the foc_list type constructor:
   type arguments of type constructor foc_list are important here
   since they are externalized into foreign languages. *)
external type foc_list ('a) =
 | Caml -> "'a list"
 | Coq -> "List.list ['a]"
;;

external value Nil =
 | Caml -> "[]"
 | Coq -> "nil"
;;

external value Cons =
 | Caml -> "( :: )"
 | Coq -> "cons"
;;
\end{verbatim}

External type definition for sum types:

\begin{verbatim}
(* Focalize binding of the foc_list type constructor and its associated
   contructors or labels. *)
type foc_list ('a) =
 internal
 | Nil
 | Cons ('a, foc_list ('a))

 external
 | Caml -> "'a, list"
 | Coq -> "List.list ['a]"

 and Nil =
 | Caml -> "[]"
 | Coq -> "nil"

 and Cons =
 | Caml -> "( :: )"
 | Coq -> "cons"
;;
\end{verbatim}

External type definition for abbreviation types:

\begin{verbatim}
type foc_pair ('a, 'b) =
  [internal] (* No internal definition: type is fully abstract. *)
  external
  | Caml -> "('a, 'b)"
  | Coq -> "{fst : 'a; snd : 'b}"
;;

type foc_diag ('a) =
  internal ('a * 'a)
  external
  | Caml -> "('a, 'a)"
  | Coq -> "{fst : 'a; snd : 'a}
;;

type int =
  external
  | Caml -> "int"
  | Coq -> "Z"
;;

(** There is a predefined ``module'' (file in fact) named Predefined,
    Internal which is known by the compiler. *)

type int =
  internal Internal#int
  external
  | Caml -> "int"
  | Coq -> "Z"
;;

(** Internal#t ne peut appara�tre que dans une clause internal et ne peut
faire r�f�rence qu'� un constructeur de l'alg�bre des types de base. *)

type t =
  internal Internal#int
  external
  | Caml -> "int"
  | Coq -> "Z"
;;

(** Cr�e dans l'environnement
 - une liaison de t vers Internal#int dans l'environnement de typage du
   programme: similaire � une abbreviation de Caml,
 - une liaison de Internal#int vers t dans l'environnement ``Internal'',
celui qui dit que 1 est de type (assoc Internal#int ``environnement
Internal'').
 - la (re)d�finition d'une liaison pour Internal#int est interdite (sauf une
fois si une option non document�e du compilateur ?).


*)
    

(* The type t is concrete. *)

(1 : t)

type t =
  internal (* Fully abstract *)
  external
  | Caml -> "int"
  | Coq -> "Z"
;;
(** Les valeurs de t sont abstraites. Et donc: *)

(1 /: t);;

(** Defining external vision of the internal type algebra

Internal#bool
Internal#int
Inutile de d�finir car s�mantique bien comprise Internal#``->''
Inutile de d�finir car s�mantique bien comprise Internal#``*''

 *)

(** O� est viss� le type int dans le compilo ?

Dans l'alg�bre de type ? Pas vraiment: c'est un constructeur constant comme
un autre.

En fait on visse le fait que ST_construct "int" est connu du compilateur au
moment o� on type les constantes enti�res.

Dans le aprser, � la place de g�n�rer une constante E_const (C_int "123"), on
pourrait g�n�rer un appel � une fonction de conversion dans le parser. Par
exemple, en appelant cette fonction int_litteral, on g�n�rerait:

INT s -> E_app (mk (E_var "int_litteral"), mk (C_string s))

Le type int serait ainsi ``d�viss�'' du typeur des constantes.

La fonction int_litteral devrait �tre d�finie en focalize comme external
(sans d�finition interne):

let int_litteral =
  internal: string -> integer
  external
  | Caml -> "Pervasives.int_of_string"
  | Coq -> ""
;;

L'int�r�t annexe est la possibilit� de red�finir les constantes de base,
simplement en red�finissant une fonction utilisateur. Par exemple
let int_litteral =
  internal: string -> big_integer
  external
  | Caml -> "Num.num_of_string"
  | Coq -> "??"
;;

Par extension on pourrait aussi avoir bool_litteral, char_litteral ...,
pour d�visser les constantes de type correspondantes.

Dans le fichier de d�finition des concepts internes on d�finirait la liaison
entre le type connu du compilateur et le type connu de l'environnement
utilisateur par une d�finition de type external avec un champ internal
d�notant un ``built-in'' (un identificateur du pseudo-``module'' Internal):



 *)

\end{verbatim}

External definitions for record types:

\begin{verbatim}
(* Focalize binding of the foc_record type constructor and its associated
   contructors or labels. *)
type foc_record ('a) =
  internal
  { hcode : int; contents : 'a; }

 external
 | Caml -> "'a Focvasives.foc_record"
 | Coq -> "Focq.foc_record ['a]"

 and hcode =
 | Caml -> "Focvasives.fv_hashing_code"
 | Coq -> "Focq.hc"

 and contents =
 | Caml -> "Focvasives.fv_contents"
 | Coq -> "Focq.conts"
;;
\end{verbatim}

\begin{verbatim}
type foc_pair ('a, 'b) =
  [internal]
  external
  | Caml -> "('a, 'b)"
  | Coq -> "{fst : 'a; snd : 'b}
;;

type foc_diag ('a) =
  internal ('a * 'a)
  external
  | Caml -> "('a, 'a)"
  | Coq -> "{fst : 'a; snd : 'a}
;;
\end{verbatim}

\subsection{External directives for values}

We could have chosen:

\begin{verbatim}
let sc = (external : string -> string -> string);;

external value sc =
  | Caml -> "concat"
  | Coq -> "^"
;;
\end{verbatim}

But we prefer something more similar to what is widely accepted for external
type definitions:

\begin{verbatim}
let sc =
  internal string -> string -> string
  external
  | Caml -> "Pervasives.( ^ )"
  | Coq -> "Focq.catenate"
;;
\end{verbatim}

\section{Comments}

\subsection{Documentation, alias structured comments}

Structured comments

\begin{itemize}
\item must be integrated in the parsetree,
\item must be parsed.
\end{itemize}

- on a decid� d'abandonner l'ssociation entre une m�thode FoCaL et un nom
  externe pour mathml, openmath ou LaTeX dans le commentaire de la m�thode.

- Dans l'entete des fichiers on fait un binding du genre

   @author(Moi)
   @title(Boolean Algebras)
   @descrition(Boolean Alberas are a model for abstract classical logics)
   @extern(
          "mathml" | "plus" -> "<plus/>', "mult" -> "<times/>";
          "tex "   | "plus" -> "+". "mult -> "";
          )

 - tous les commentaires sont � arguments: @title(), @description() ...

 - dans les commentaires de m�thode on autorise du texte libre avec la
   possibilit� de mettre des @focal(plus(x,y)) qui sont v�rifi�s du genre

   (**
       In a boolean algebra @focal(!plus(x,y)) denotes
       logical or of @focal(x) and @focal(y)

     *)

 - la port�e des commentaires d'une m�thode est celle de l'h�ritage normal. La
   notation est celle li�e au fichier associ� � l'h�ritage normal.

 - Il faut penser � ce qu'on met dans du texte libre et aux caract�res sp�ciaux
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


