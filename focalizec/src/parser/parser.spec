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
type list (a) =
  | Nil
  | Cons (a, list (a))
;;

external type list =
 | Caml -> "list"
 | Coq -> "List.list"
;;

external val Nil =
 | Caml -> "[]"
 | Coq -> "nil";;

external val Cons =
 | Caml -> "( :: )"
 | Coq -> "cons";;
\end{verbatim}

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
- start by %% and
- end by a new line
