
(* ********************************* *)

let test_context : Context_test.test_context option ref = ref None;;

let set_test_context tc = test_context := Some tc;;
let get_test_context () = !test_context;;

(* ********************************* *)


