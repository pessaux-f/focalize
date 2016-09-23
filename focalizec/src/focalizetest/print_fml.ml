open Print_foc ;;

  
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
  MyFormat.print_string fname;
  MyFormat.print_string "=";
  MyFormat.print_space ();
  MyFormat.print_string "{*";
  MyFormat.print_space ();
  MyFormat.print_string caml;
  MyFormat.print_space ();
  MyFormat.print_string "*};";
  MyFormat.force_newline ()
;;

let print_import (n,l) =
  MyFormat.print_string "import for ";
  MyFormat.print_string n;
  MyFormat.force_newline ();
  List.iter print_function_imported l;
  MyFormat.force_newline ();
  MyFormat.print_string "end import";
  MyFormat.force_newline ();
  MyFormat.force_newline ()
;;

let print_fml_file f fichier_fml =
  MyFormat.set_margin 120;
  if not(f = "") then MyFormat.set_formatter_out_channel (open_out f)
  else MyFormat.set_formatter_out_channel stderr ;
  List.iter print_import fichier_fml ;
  MyFormat.force_newline ()
;;


