Require Export Bool.
Require Export List.

Implicit Arguments map [A B].

Parameter string : Set.
Parameter string_eq : string -> string -> bool.
Parameter string_prop : string.
Parameter string_basics : string.
Parameter char : Set.