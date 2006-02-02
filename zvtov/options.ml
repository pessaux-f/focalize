(*  Copyright 2005 INRIA  *)
(*  $Id: options.ml,v 1.5 2006-02-02 13:30:03 doligez Exp $  *)

open Arg;;

let print_version () =
  Printf.printf "%s\n" Version.version;
  exit 0;
;;

let options = ref [
  "-debug", Arg.Set Invoke.keep_temp_files,
         "   do not remove temporary files";
  "-nocache", Arg.Clear Cache.active,
           " do not use nor update the proof cache file";
  "-p0", Arg.Unit (fun () -> Invoke.progress_level := 0),
      "      do not display progress info";
  "-p1", Arg.Unit (fun () -> Invoke.progress_level := 1),
      "      display progress bar (default)";
  "-p2", Arg.Unit (fun () -> Invoke.progress_level := 2),
      "      display progress messages";
  "-script", Arg.Clear Invoke.use_coqterm,
          "  output proofs in script format";
  "-term", Arg.Set Invoke.use_coqterm,
        "    output proofs in term format (default)";
  "-v", Arg.Unit print_version,
     "       print version string and exit";
  "-verbose", Arg.Set Invoke.verbose,
           " print out zenon invocations";
  "-zenon", Arg.Set_string Invoke.zcmd,
     Printf.sprintf "<command>    how to invoke zenon (default: \"%s\")"
                   !Invoke.zcmd;
  "-zopt", Arg.Set_string Invoke.zopt,
    Printf.sprintf "<options>     options passed to zenon\n%s(default: \"%s\")"
                   (String.make 12 ' ') !Invoke.zopt;
  "-zz", Arg.String (fun s -> Invoke.addopt := s :: !Invoke.addopt),
      "<option>  add <option> to be passed to zenon";
];;

let get_options () = !options;;

let register_option flag action help =
  options := (flag, action, help) :: !options
;;
