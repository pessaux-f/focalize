open Print_foc;;
open MyFormat;;
open Own_basics;;
  
(* let create_species_import n (l :( string * string) list) = n,l;; *)
(*
let import_toplevel xml =
  create_species_import "toplevel"
  ["init_rand","Random.init (int_of_float (Unix.time ()))";
   "seq a b"  ,"fun a b -> (b;a)";
   "print_close_file",
"    let first = ref true in
    let file = ref stdout in
    (fun s ->
       if !first then
         (first := false;
          file := open_out \"" ^ xml ^ "\"
         );
      output_string !file s;
    ), fun () -> close_out !file";
   "catch_raise","fun f -> try false,(f (),\"\") with | " ^ focexception ^ " s -> true,(false,s)";
   "flush_stdout", "fun () -> Pervasives.flush stdout"               
  ]
*)

let print_function_imported (fname,caml) =
  print_string fname;
  print_string "=";
  print_space ();
  print_string "{*";
  print_space ();
  print_string caml;
  print_space ();
  print_string "*};";
  force_newline ();;

let print_import (n,l) =
  print_string "import for ";
  print_string n;
  force_newline ();
  List.iter print_function_imported l;
  force_newline ();
  print_string "end import";
  force_newline ();
  force_newline ();;

let print_fml_file f fichier_fml =
  set_margin 80;
  if not(f = "") then
    set_formatter_out_channel (open_out f)
  else
    set_formatter_out_channel stderr;
  List.iter print_import fichier_fml;
  force_newline ();;


