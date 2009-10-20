
(** A prolog term is a function name applied to a list of term *)
type prolog_term =
  | Prolog_comment of string
  | Prolog_fun of string * prolog_term list
  | Prolog_var of string
  | Prolog_conjunction of prolog_term list
  | Prolog_list of prolog_term list
  | Prolog_int of int;;

(** A prolog clause is composed of an head follow by a body *)
type prolog_clause = prolog_term option * prolog_term list;;

let prolog_is_comment =
  function
    | Prolog_comment _ -> true
    | _ -> false;;

(** A prolog program is a list of clause *)
type prolog_pgm = prolog_clause list;;

let prolog_list l = Prolog_list l;;

let prolog_int i = Prolog_int i;; 

let prolog_fun s l = Prolog_fun(s,l);; 

let prolog_fun_bin s t1 t2 = Prolog_fun(s,[t1;t2]);; 

let prolog_var x = Prolog_var(x);;

let prolog_fd_equal e1 e2 =
  prolog_fun "#=" [e1;e2];;

let prolog_equal e1 e2 =
  prolog_fun "=" [e1;e2];;

let prolog_clause head body : prolog_clause =
  head, body;;

(* We should add all the functions that construct equality over termes, match,
 * ite etc... *)

