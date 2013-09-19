type 'a _focty_list_t = 
  | Nil_list
  | Cons_list of ('a * 'a _focty_list_t)
 ;;
type 'a _focty_tree_t = 
  | Nil_tree
  | Cons_tree of (Basics._focty_int * 'a _focty_tree_t _focty_list_t)
 ;;
let rec pr_list (pr_elem_fct : 'a -> Basics._focty_string) (l : 'a
  _focty_list_t) =
  match l with
   | Nil_list ->
       (begin
       "0"
       end)
   | Cons_list (h,
       q) ->
       (begin
       (Basics._hat_ (pr_elem_fct h) (pr_list pr_elem_fct q))
       end)
;;
let rec pr_tree (l : 'a _focty_tree_t) =
  match l with
   | Nil_tree ->
       (begin
       ""
       end)
   | Cons_tree (n,
       l) ->
       (begin
       (Basics._hat_ (Basics.string_of_int n) (lp l))
       end)
and lp (l : 'a _focty_tree_t _focty_list_t) = (pr_list pr_tree l)
;;
