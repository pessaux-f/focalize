
type xml_tree =
  | Node of string * xml_tree list
  | Leave of string * string option;;

type test_case = Own_expr.myexpr list;;
type report_property =
       string * string *
       ((Own_prop.variables * Own_prop.elementaire) * test_case list) list;;
type report = Context_test.test_context * report_property list;;

val string_of_report : report -> string;;

val xml_report_to_report : xml_tree -> report;;
