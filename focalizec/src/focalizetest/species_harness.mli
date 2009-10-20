
val spec_harness : Own_expr.species_name -> Own_expr.species_name;;
(** [spec_harness spec]

 Get the name of the species that inherits of spec and contains the harness. *)

val spec_harness_harness : string -> Own_expr.species_name;;
(** [spec_harness_harness spec]

 Get the name of the species that inherits of spec that contains the harness *)

val coll_harness : Own_expr.species_name -> Own_expr.species_name;;
(** [coll_harness spec]

 Get the name of the collection that implements the species which contains the
 harness of [spec]. *)

val coll_harness_harness : string -> string;;
(** [coll_harness_harness spec]

 Get the name of the collection that inherits of spec that contains the harness *)

val species_harness :
  Context_test.test_context -> Own_types.typ list -> 
    (Own_expr.species_name * Own_expr.parameters list * Own_expr.species_name list) *
    Own_expr.toplevel_def list
(** [species_harness test_context others_types]

Takes a test context and a list of types.
Returns all toplevel definition that harness the species specified by [spec_prm]
(ie: create a species inherited from [spec_rpm] which define some methods for
generating random value of the rep's and types in [typs]. It creates others
species in order to instrumentate the parameters).
*)

