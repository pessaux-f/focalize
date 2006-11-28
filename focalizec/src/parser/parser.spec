Les définitions de type doivent être aussi proches que possible de la syntaxe
de leur utilisation.

(* Old *)
type list a = 
  caml_link list;
  Nil ([]) in list(a);
  Cons ((::)) in a -> list(a) -> list(a);
;;

(* New *)
type list (a) =
  | Nil
  | Cons (a, list (a))
;;

external list = "list" "List.list";;
external Nil = "[]" "nil";;
external Cons = "(::)" "cons";;

type list (a, b) =
  | Nil (b)
  | Cons (a, b, list (a, b))
;;

Commentaires structurés:

- ils doivent être accrochés dans le parsetree,
- ils doivent donc être lexés et parsés.

Commentaires non structurés:

- ils sont jetés.
