open Focalize_inter;;
(*
focalize_init "fic2";;

(*
let print_bool b = if b then print_string "true" else print_string "false";;
print_bool (is_concrete_type (TAtom "list"));;
print_newline();;
*)

let print_item s = print_string ("- " ^ s ^ "\n");;

print_string " ***** Liste des modules ouverts *****\n";;
List.iter print_item (focalize_get_all_open ());;
print_string "        ***** Fin de liste *****\n";;

let rec pour_toi t =
  let fvar = fun () -> print_string "type_variables" in
  let farrow t1 t2 = print_string "type_fleches" in
  let ftuple l = print_string "tuple" in
  let fsum l =  print_string "sum" in
  let fconstruct _ tn args = 
    print_string ("ident(" ^ tn ^ ")( ");
    List.iter pour_toi args;
    print_string ")" in
  let frep () =  print_string "Self" in
  let fspecrop s1 s2 =  print_string "Self of a parameter" in
  Types.extract_type_simple fvar farrow ftuple fsum fconstruct frep fspecrop t;;

let y = Env.TypingEnv.find_type Location.none "???" (get_global_lident "basics" "list") (get_env ());;

(* get_type_kind y.ET.type_kind;; *)

(* pour_toi (Types.specialize y.ET.type_identity);; *)
(* List.iter pour_toi (y.ET.type_params);; *)
(* print_int y.ET.type_arity;; *)

(* 
 Faire une fonction qui à partir d'un ensemble de nom d'espèce ?
  retourne tous les modules utilisées dans les types.
  *)

(*
let ff = Env.TypingEnv.find_value locnone "fic2" None (get_dummy_ast (EI_local
(Vlident "t"))) (open_module "basics");;
*)

let print_a_constructor (first, ff) =
(*   let ff = Env.TypingEnv.find_constructor locnone "fic2" first m in *)
  Format.print_string (extract_vname first);
  Format.print_string " in ";
  Types.pp_type_scheme Format.std_formatter ff.ET.cstr_scheme;
  Format.print_newline ();;

Format.print_newline ();;
let l = focalize_get_all_constructors ();;
Format.print_string "**** Liste des constructeurs ****\n";;
List.iter print_a_constructor l;;
Format.print_string "     ***** Fin de liste *****\n";;
Format.print_flush ();;

print_string "**** Liste des types ****\n";;
List.map (fun e -> print_string ((extract_qname e) ^ "\n")) (focalize_get_all_types ());;
print_string "***** Fin de liste *****\n";;

print_string "**** Liste des constructeurs de liste ****\n";;
List.iter (fun (e,r) -> print_string (extract_vname e); print_newline ())
                                     (get_constructors_of_a_type "list");;
print_string "          **** Fin de liste ****\n";;


let l = get_concrete_def (TPrm("mon_type", [TAtom "z"; TAtom "y"; TAtom "x";
TAtom "w"]));;

let pp l = print_string (dbg_string_constructor l ^ "\n");;

print_string "**** Liste des constructeurs de mon_type ****\n";;
List.iter pp l;;
print_string "            **** Fin de liste ****\n";;

print_string (dbg_string_myexpr (get_meth_def ("fic2", "S") "a"));;
print_newline ();;
print_string (string_of_myexpr (get_meth_def ("fic2", "S") "a"));;
print_newline ();;
print_string (string_of_myexpr (get_meth_def ("fic2", "S") "b"));;
print_newline ();;
print_string (string_of_myexpr (get_meth_def ("fic2", "S") "c"));;
print_newline ();;
print_string (string_of_myexpr (get_meth_def ("fic2", "S") "d"));;
print_newline ();;
*)
