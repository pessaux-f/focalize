(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: fodump.ml,v 1.1 2008-05-16 14:02:37 pessaux Exp $ *)

let main () =
  let fo_file_name = ref None in
  Arg.parse
    []
    (fun fname -> fo_file_name := Some fname)
    "Usage: fo_dump <options> <.fo file>" ;
  match !fo_file_name with
   | None -> failwith "Euh euh, no file."
   | Some fname ->
       let fo_file = open_in_bin fname in
       let (fo_struct : Env.fo_file_structure) =
         if Files.check_magic fo_file Files.fo_magic then input_value fo_file
         else failwith "Corrupted fo file." in
       close_in fo_file ;
       Env.inspect_fo_structure Format.std_formatter fo_struct ;
       Format.printf "@."
;;

main () ;;
