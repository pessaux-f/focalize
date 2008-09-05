Require basics.
Require ZArith.

Open Scope Z_scope.
Definition random_seed (seed : basics.int__t) := coq_builtins.Void.
Definition random_int (foo : basics.int__t) : basics.int__t := 42.
Definition random_self_init (x : basics.unit__t) := coq_builtins.Void.
