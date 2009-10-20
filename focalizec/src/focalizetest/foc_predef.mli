
val top_coverage_coll_name : string
(** Name of the species which calculate the coverage of a test set *)

(** It's the list of functions used by the harness for the submit, verdict calculus and
printed out report.  *)
val top_preambule : Own_expr.toplevel_def list

(** [top_postambule coll_list] takes a list of collection name. Assuming these
collections exist, it returns the list of expressions calling the harness
implemented within the collections. *)
val top_postambule : string list -> Own_expr.toplevel_def list

(** [top_import xml] takes a xml file name [xml] and return the default import
 section concerning this xml file. *)
val top_import : string -> Own_expr.import
