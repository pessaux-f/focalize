open Pred
open Foc2spec
open Focgen


(* Extraction of inductive predicates. *)
let extract file_ast =
  let specs = foc2spec file_ast in
  let defaults = List.map (fun (_, s) -> default_case s) specs in
  let trees = List.map 
    (fun (sp, s) -> (sp, s, tree_from_spec_with_quick_try false s)) specs in
  let codes = List.map 
    (fun (sp, s, t) -> (sp, code_from_tree s t)) trees in
  let focalize_codes = List.map2 
    (fun (sp, c) d -> (sp, gen_focalize c d)) codes defaults in
  let new_ast = List.fold_left inject_code file_ast focalize_codes in
  new_ast

