exception Foc_error of string ;;

let foc_error msg = raise (Foc_error msg) ;;
let str_cat x y = x ^ y ;;
let str_lt (x : string) (y : string) = x < y ;;
let str_print s = print_string s ;;
let string_of_int i = string_of_int i ;;
let int_of_string s = int_of_string s ;;
let int_mult x y = x * y ;;
let int_print i = print_int i ;;
let int_mod x y = x mod y ;;
let int_eq (x : int) (y : int) = x = y ;;
let int_div x y = x / y ;;
let int_lt (x : int) (y : int) = x < y ;;
let int_leq (x : int) (y : int) = x <= y ;;
