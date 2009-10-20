
val set_test_context : Context_test.test_context -> unit
(** [set_test_context spec] sets the test context the user wants to test, to
[spec]. *)

val get_test_context : unit -> Context_test.test_context option
(** [get_test_context ()] returns the test_context the user wants to test. *)

