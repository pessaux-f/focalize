Require Export external.

Parameter location : Set.

Record ast (D : Set) (T : Set) : Set := mk_ast {
  ast_desc : D;
  ast_type : T;
  ast_loc : option location
}.
Implicit Arguments mk_ast [ D T ].


Inductive res (A: Set) : Set :=
| OK: A -> res A
| Error: string -> res A.
Implicit Arguments OK [A].
Implicit Arguments Error [A].

Definition bind (A B: Set) (f: res A) (g: A -> res B) : res B :=
  match f with
  | OK x => g x
  | Error msg => Error msg
  end.
Implicit Arguments bind [ A B ].

Definition bind2 (A B C: Set) (f: res (A * B)) (g: A -> B -> res C) : res C :=
  match f with
  | OK (x, y) => g x y
  | Error msg => Error msg
  end.

Notation "'do' X <- A ; B" := (bind A (fun X => B))
 (at level 200, X ident, A at level 100, B at level 200)
 : error_monad_scope.

Notation "'do' ( X , Y ) <- A ; B" := (bind2 A (fun X Y => B))
 (at level 200, X ident, Y ident, A at level 100, B at level 200)
 : error_monad_scope.

Open Local Scope error_monad_scope.

Fixpoint mmap (A B: Set) (f: A -> res B) (l: list A) {struct l} :
  res (list B) :=
  match l with
  | nil => OK nil
  | hd :: tl => 
    do hd' <- f hd;
    do tl' <- mmap A B f tl;
      OK (hd' :: tl')
  end.
Implicit Arguments mmap [A B].