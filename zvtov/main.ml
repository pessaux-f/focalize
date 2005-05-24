(*  Copyright 2004 INRIA  *)
(*  $Id: main.ml,v 1.13 2005-05-24 14:15:51 doligez Exp $  *)

let print_version () =
  Printf.printf "zvtov version 0.3.1 [9] 2005-05-24\n";
  exit 0;
;;

let infile = ref None;;

let speclist = [
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
];;

let anon s = infile := Some s;;

let usage = "usage: zvtov {option} <file.zv>\noptions are:"

let main () =
  Arg.parse speclist anon usage;
  match !infile with
  | None ->
      Printf.eprintf "error: no input file specified\n";
      Arg.usage speclist usage;
  | Some inf ->
      let ic = open_in_bin inf in
      let lb = Lexing.from_channel ic in
      let base = try Filename.chop_extension inf with _ -> inf in
      let ouf = base ^ ".v" in
      let oc = open_out_bin ouf in
      Cache.init base (Invoke.signature ());
      Parser.parse inf lb oc;
      Cache.close ();
;;

main ();;
