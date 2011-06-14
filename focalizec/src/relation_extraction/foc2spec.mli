
exception PredSyntaxError of Location.t
exception PredPropertyError of Location.t


(* Search for extraction annotations and parse it. *)
val foc2spec : Parsetree.file ->
  (string * ('p, 'c, 'r, 'co, 'f) Pred.s_pred_spec) list

