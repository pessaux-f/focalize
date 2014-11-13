(*  Copyright 2005 INRIA  *)
(*  $Id: options.ml,v 1.12 2009-08-24 12:15:00 doligez Exp $  *)

open Arg;;
open Misc;;

let print_version () =
  Printf.printf "%s\n" Version.version;
  exit 0;
;;

let options = ref [
  "-cime", Arg.Set with_cime,
        "    invoke Cime to treat equational problems";
  "-continue", Arg.Clear stop_on_failure,
            " continue after failed proofs and keep .v file";
  "-debug", Arg.Set keep_temp_files,
         "   do not remove temporary files";
  "-new", Arg.Unit (fun () -> Misc.focal_ext := "focal"),
       "     tell zenon to use the new focalize syntax for booleans (default)";
  "-nocache", Arg.Clear with_cache,
           " do not use nor update the proof cache file";
  "-old", Arg.Unit (fun () -> Misc.focal_ext := "coqbool"),
       "     tell zenon to use the old focal syntax for booleans";
  "-p0", Arg.Unit (fun () -> progress_level := 0),
      "      do not display progress info";
  "-p1", Arg.Unit (fun () -> progress_level := 1),
      "      display progress bar (default)";
  "-p2", Arg.Unit (fun () -> progress_level := 2),
      "      display progress messages";
  "-script", Arg.Clear use_coqterm,
          "  output proofs in script format";
  "-term", Arg.Set use_coqterm,
        "    output proofs in term format (default)";
  "-v", Arg.Unit print_version,
     "       print version string and exit";
  "-verbose", Arg.Set verbose,
           " print out zenon invocations";
  "-zenon", Arg.Set_string zcmd,
     Printf.sprintf "<command>    how to invoke zenon (default: \"%s\")"
                   !zcmd;
  "-zopt", Arg.Set_string zopt,
    Printf.sprintf "<options>     options passed to zenon\n\
         %s(default: \"-x %s %s\")" (String.make 12 ' ') !focal_ext !zopt;
  "-z", Arg.String (fun s -> add_opt := s :: !add_opt),
      "<opts> add <opts> to be passed to zenon";
  "-zz", Arg.String (fun s -> add_opt := s :: !add_opt),
      "<opt> add <opt> to be passed to zenon";
];;

let get_options () = !options;;

let register_option flag action help =
  options := (flag, action, help) :: !options
;;
