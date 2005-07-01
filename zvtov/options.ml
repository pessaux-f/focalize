(* possible options of zvtov. *)
(* $Id: options.ml,v 1.1 2005-07-01 12:26:22 prevosto Exp $ *)
open Arg

let print_version () =
  Printf.printf "zvtov version 0.3.1 [9] 2005-05-24\n";
  exit 0;
;;

let options = ref  [
  "-nocache", Arg.Clear Cache.active,
           " do not use nor update the proof cache file";
  "-p0", Arg.Unit (fun () -> Invoke.progress_level := 0),
      "      do not display progress info";
  "-p1", Arg.Unit (fun () -> Invoke.progress_level := 1),
      "      display progress bar (default)";
  "-p2", Arg.Unit (fun () -> Invoke.progress_level := 2),
      "      display progress messages";
  "-script", Arg.Clear Invoke.use_coqterm,
          "  output proofs in script format (default)";
  "-term", Arg.Set Invoke.use_coqterm,
        "    output proofs in term format";
  "-v", Arg.Unit print_version,
     "       print version string and exit";
  "-zenon", Arg.Set_string Invoke.zcmd,
     Printf.sprintf "<command>    how to invoke zenon (default: \"%s\")"
                   !Invoke.zcmd;
  "-zopt", Arg.Set_string Invoke.zopt,
    Printf.sprintf "<options>     options passed to zenon\n%s(default: \"%s\")"
                   (String.make 12 ' ') !Invoke.zopt;
]

let get_options () = !options

let register_option flag action help = 
  options:= (flag, action, help) :: !options
