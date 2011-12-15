% prolog program

/* ************************************************************************* */
/*                                  Pairs                                    */
/* ************************************************************************* */


pair(R, A, B) :-
  R = pair(A, B).

first(R, C) :-
  when(nonvar(C),
       (C = pair(T,_) -> R = T; fail)).

scnd(R, C) :-
  when(nonvar(C),
       (C = pair(_,T) -> R = T; fail)).

/* ************************************************************************* */
/*                                  Booleans                                 */
/* ************************************************************************* */

/* Conjunction */
and_b_no(X, Y) :-
  when((nonvar(X); nonvar(Y)),
      ( X == 1 ->
        Y = 0
      ;
        ( Y == 1 ->
          X = 0
        ; true
        )
      )).

and_b_yes(X, Y) :-
  X = 1, Y = 1.

and_b(R, X, Y) :-
  when((nonvar(X), nonvar(Y)),
       ((X == 1, Y == 1) -> R =1;R=0)),
  when(nonvar(R),
       ( R == 1 ->
         and_b_yes(X,Y)
       ; and_b_no(X,Y)
       )
      ).

/* Disjunction  */
or_b_no(X, Y) :-
  X = 0, Y = 0.

or_b_yes(X, Y) :-
  when((nonvar(X); nonvar(Y)),
      ( X == 0 ->
        Y = 1
      ;
        ( Y == 0 ->
          X = 1
        ; true
        )
      )).

or_b(R, X, Y) :-
  when((nonvar(X), nonvar(Y)),
       ((X == 0, Y == 0) -> R #=0;R#=1)),
  when(nonvar(R),
       ( R == 1 ->
         or_b_yes(X,Y)
       ; or_b_no(X,Y)
       )
      ).

/* negation */

disjoint_bool_vp(X,P) :-
  P == 1 -> X = 0; X = 1.

disjoint_bool_vv(X, Y) :-
  when(nonvar(X), disjoint_bool_vp(Y,X)),
  when(nonvar(Y), disjoint_bool_vp(X,Y)).

not_b(R,X) :-
  disjoint_bool_vv(R,X).

/* exlusive or */

xor_b(R, X, Y) :-
  when(nonvar(R),
       ( R == 0 ->
         X #= Y
       ; disjoint_bool_vv(X, Y)
       )),
  when(nonvar(X),
       ( X == 0 -> 
         R #= Y
       ; disjoint_bool_vv(R,Y)
       )),
  when(nonvar(Y),
       ( Y == 0 -> 
         R #= X
       ; disjoint_bool_vv(R,X)
       )).

/* ************************************************************************* */
/*                                 Levé d'exception                          */
/* ************************************************************************* */

foc_error(_) :- fail.

/* ************************************************************************* */
/*                                 Option type                               */
/* ************************************************************************* */

is_failed(R,X) :-
  match([], X, 
     [pattern(failed,[R #= 1]),
      pattern(unfailed(_),[R #= 0])
     ]).

non_failed(R, X) :-
  match([], X, 
     [pattern(failed,[foc_error(_Err)]),
      pattern(unfailed(A),[R = A])
     ]).

/* ************************************************************************* */
/*                                  Integers                                 */
/* ************************************************************************* */

/*

let string_of_int (x in int) in string = caml soi;;
let int_of_string (x in string) in int = caml ios;;
let print_int (x in int) in unit = caml pi;;

*/

   /* Binary relation */
int_eq(R, X, Y) :-
  R #<=> X #= Y.

int_lt(R, X, Y) :-
  R #<=> X #< Y.

int_leq(R, X, Y) :-
  R #<=> X #=< Y. 

int_geq(R, X, Y) :- 
  R #<=> X #>= Y. 

int_gt(R, X, Y) :-
  R #<=> X #> Y. 

    /* Binary operation */
int_minus(R,X,Y) :-
  R #= X - Y.

int_mult(R,X,Y) :-
  R #= X * Y.

int_plus(R,X,Y) :-
  R #= X + Y.

int_div(R,X,Y) :-
  R #= X / Y.

int_mod(R, X, Y) :-
 R #= X mod Y.

int_max(R, X, Y) :-
  maximum(R,[X, Y]).

int_min(R, X, Y) :-
  minimum(R,[X, Y]).

int_opp(R, X) :-
  R #= 0 - X.

succ(R,X) :-
  R #= X + 1.

pred(R, X) :-
  R #= X - 1.

/*
let base_eq (x in 'a, y in 'a) in bool = caml beq with coqdef beq;;
let phys_eq (x in 'a, y in 'a) in bool = caml str_eq;;

*/
/*
   String opérations :
let sc (s1 in string, s2 in string) in string = caml sc;;
let str_lt (s1 in string, s2 in string) in bool = caml str_lt;;
let print_string (s1 in string) in unit = caml prt;;
let print_newline (x in unit) = #print_string("\n");;
*/
/*
 Les reférences :
let ref (x in 'a) in ref('a) = caml ref;;
let get (x in ref('a)) in 'a = caml access;;
let set (x in ref('a),y in 'a) in unit = caml set;;
*/

ref(_R, _X) :- fail.
get(_R, _X) :- fail.
set(_R, _X, _Y) :- fail.

