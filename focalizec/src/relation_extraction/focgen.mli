
(* Default case for pattern matching. *)
val default_case : ('p, 'c, 'r, 'co, 'f) Pred.s_pred_spec -> Parsetree.expr

(* Focalize code generation from intermediate langage. *)
val gen_focalize : Pred.l_fun -> Parsetree.expr -> Parsetree.species_field_desc

(* Inject a focalize "let" into the AST. *)
val inject_code : Parsetree.file -> (string * Parsetree.species_field_desc) ->
                  Parsetree.file

