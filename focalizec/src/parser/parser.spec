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

\subsection{Structured comments}

Structured comments
\begin{itemize}
\item must be integrated in the parsetree,
\item must be parsed.
\end{itemize}

\subsection{Unstructured comments}

Unstructured comments
\begin{itemize}
\item may appear anywhere in the source code,
\item are ignored and thrown during the lexing phase of parsing.
\end{itemize}

