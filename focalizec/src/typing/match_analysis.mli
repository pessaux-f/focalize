
(* Exceptions for pattern matching analysis. *)
exception Match_not_exhaustive of Location.t
exception Match_useless_case of Location.t


(* Search for pattern matching expressions in the AST and verify them. *)
val verify_matchings : Env.TypingEnv.t -> Infer.please_compile_me -> unit
