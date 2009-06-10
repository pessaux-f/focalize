(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*            François Pessaux                                         *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007, 2008 LIP6 and INRIA                                *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: focalizec.ml,v 1.45 2009-06-10 17:57:06 pessaux Exp $ *)


exception Bad_file_suffix of string ;;



let compile_fcl input_file_name =
  (* First, let's lex and parse the input source file.
     Create the current compilation unit "fname". In fact, this is the current
     filename without dirname and extention. *)
  let current_unit =
    Filename.chop_extension (Filename.basename input_file_name) in
  (* Include the installation libraries directory in the search path. *)
  if Configuration.get_use_default_lib () then
    Files.add_lib_path Installation.install_lib_dir ;
  (* Lex and parse the file. *)
  let ast = Parse_file.parse_file input_file_name in
  (* Hard-dump the AST if requested. *)
  if Configuration.get_raw_ast_dump () then
    Dump_ptree.pp_file Format.err_formatter ast ;
  (* Pretty the AST as a new-focalize-syntax source if requested. *)
  (match Configuration.get_pretty_print () with
   | None -> ()
   | Some fname ->
       let out_hd = open_out_bin fname in
       let out_fmt = Format.formatter_of_out_channel out_hd in
       Sourcify.pp_file out_fmt ast ;
       close_out out_hd) ;
  (* Scopes AST. *)
  let (scoped_ast, scoping_toplevel_env) =
    (let tmp = Scoping.scope_file current_unit ast in
    (* Pretty the scoped AST if requested. *)
    (match Configuration.get_pretty_scoped () with
     | None -> ()
     | Some fname ->
         let out_hd = open_out_bin fname in
         let out_fmt = Format.formatter_of_out_channel out_hd in
         Sourcify.pp_file out_fmt (fst tmp) ;
         close_out out_hd) ;
    tmp) in
  (* Typechecks the AST. *)
  let (typing_toplevel_env, stuff_to_compile) =
    Infer.typecheck_file ~current_unit scoped_ast in
  (* Generate the documentation if requested. *)
  if Configuration.get_focalize_doc () then
    Main_docgen.gen_doc_please_compile_me
      input_file_name scoped_ast stuff_to_compile ;
  (* Now go to the OCaml code generation if requested. *)
  let mlgen_toplevel_env =
    if Configuration.get_generate_ocaml () then
      (begin
      let out_file_name = (Filename.chop_extension input_file_name) ^ ".ml" in
      Some
        (Main_ml_generation.root_compile
           ~current_unit ~out_file_name stuff_to_compile)
      end)
    else None in
  (* Finally, go to the Coq code generation if requested and generate the
     .zv file . *)
  let coqgen_toplevel_env =
    if Configuration.get_generate_coq () then
      (begin
      let out_file_name = (Filename.chop_extension input_file_name) ^ ".zv" in
      Some
        (Main_coq_generation.root_compile
           ~current_unit ~out_file_name stuff_to_compile)
      end)
    else None in
  (* Now, generate the persistent interface file. *)
  Env.make_fo_file
    ~source_filename: input_file_name
    scoping_toplevel_env typing_toplevel_env mlgen_toplevel_env
    coqgen_toplevel_env
;;


let make_includes get_libs =
  String.concat " -I " ("" :: List.rev (get_libs ()))
;;

let compile_ml input_file_name =
  (* We include the library search paths for OCaml. *)
  let includes = make_includes Files.get_lib_paths in
  let args = Printf.sprintf "%s -c %s" includes input_file_name in
  match Configuration.get_ml_compiler () with
   | Configuration.OCamlByt ->
       let cmd = Installation.caml_byt_compiler ^ args in
       Format.eprintf "Invoking ocamlc...@\n" ;
       Format.eprintf ">> %s@." cmd ;
       let ret_code = Sys.command cmd in
       if ret_code <> 0 then exit ret_code
   | Configuration.OCamlBin ->
       let cmd = Installation.caml_bin_compiler ^ args in
       Format.eprintf "Invoking ocamlopt...@\n" ;
       Format.eprintf ">> %s@." cmd ;
       let ret_code = Sys.command cmd in
       if ret_code <> 0 then exit ret_code
   | Configuration.OCamlBoth ->
       let cmd = Installation.caml_byt_compiler ^ args in
       Format.eprintf "Invoking ocamlc...@\n" ;
       Format.eprintf ">> %s@." cmd ;
       let ret_code = Sys.command cmd in
       if ret_code <> 0 then exit ret_code ;
       let cmd = Installation.caml_bin_compiler ^ args in
       Format.eprintf "Invoking ocamlopt...@\n" ;
       Format.eprintf ">> %s@." cmd ;
       let ret_code = Sys.command cmd in
       if ret_code <> 0 then exit ret_code
;;



let compile_zv input_file_name =
  let cmd =
    Printf.sprintf "%s -zenon %s -new %s"
      Installation.zvtov_compiler
      Installation.zenon_compiler
      input_file_name in
  Format.eprintf "Invoking zvtov...@\n" ;
  Format.eprintf ">> %s@." cmd ;
  let ret_code = Sys.command cmd in
  if ret_code <> 0 then exit ret_code
;;



let compile_coq input_file_name =
  (* Coq always requires Zenon .v files. *)
  let for_zenon = " -I " ^ Installation.zenon_libdir in
  (* We include the library search paths for Coq. *)
  let includes = make_includes Files.get_lib_paths in
  let cmd =
    Printf.sprintf "%s %s %s %s"
      Installation.coq_compiler includes for_zenon input_file_name in
  Format.eprintf "Invoking coqc...@\n" ;
  Format.eprintf ">> %s@." cmd ;
  let ret_code = Sys.command cmd in
  if ret_code <> 0 then exit ret_code
;;



let dispatch_compilation files =
  List.iter
    (fun input_file_name ->
      (* Check for a FoCaLize source file to compile. *)
      let suffix =
        String.lowercase (Files.get_file_name_suffix input_file_name) in
      match suffix with
       | "fcl" ->
           let input_file_no_suffix = Filename.chop_extension input_file_name in
           (* First, .fcl -> .ml and/or .v. *)
           compile_fcl input_file_name ;
           if Configuration.get_generate_ocaml () then
             (begin
             (* If a .ml file was generated, let's compile it. *)
             compile_ml (input_file_no_suffix ^ ".ml") ;
             end) ;
           if Configuration.get_generate_coq () then
             (begin
             if not (Configuration.get_stop_before_zenon ()) then
               (begin
               (* If a .zv file was generated, let's compile it. *)
               compile_zv (input_file_no_suffix ^ ".zv") ;
               if not (Configuration.get_stop_before_coq ()) then
                 (begin
                 (* Finally, pass it to Coq. *)
                 compile_coq (input_file_no_suffix ^ ".v")
                 end) ;
               end) ;
             end) ;
       | "ml" | "mli" -> compile_ml input_file_name
       | "zv" ->
           compile_zv input_file_name ;
           (* Finally, pass it to Coq. *)
           let input_file_no_suffix = Filename.chop_extension input_file_name in
           compile_coq (input_file_no_suffix ^ ".v")
       | "v" -> compile_coq input_file_name
       | _ -> raise (Bad_file_suffix input_file_name))
    files
;;





(* The main procedure *)
let main () =
  Arg.parse
    [ ("-dot-non-rec-dependencies",
       Arg.String Configuration.set_dotty_dependencies,
       " dumps species non-let-rec- dependencies as dotty\n\tfiles into the \
         argument directory.") ;
      ("--experimental",
       Arg.Unit Configuration.set_experimental,
       " do not use. Fear it! For the development team only!") ;
      ("-focalize-doc",
       Arg.Unit Configuration.set_focalize_doc,
       " generate documentation.") ;
      ("-i",
       Arg.Unit (fun () -> Configuration.set_do_interface_output true),
       " prints the source file interface.") ;
      ("-I",
       Arg.String (fun path -> Files.add_lib_path path),
       " adds the specified path to the path list where to search for \
         compiled\n\tinterfaces.") ;
      ("-impose-termination-proof",
       Arg.Unit Configuration.set_impose_termination_proof,
       " makes termination proofs of recursive functions \n\
         mandatory.") ;
      ("-methods-history-to-text",
       Arg.String Configuration.set_methods_history_to_text,
       " dumps species' methods' inheritance history as plain text\n\tfiles \
         into the argument directory.") ;
      ("-no-ansi-escape",
       Arg.Unit Configuration.unset_fancy_ansi,
       " disables ANSI escape sequences in the error messages.") ;
      ("-no-coq-code",
       Arg.Unit Configuration.unset_generate_coq,
       " disables the Coq code generation.") ;
      ("-no-ocaml-code",
       Arg.Unit Configuration.unset_generate_ocaml,
       " disables the OCaml code generation.") ;
      ("-no-stdlib-path",
       Arg.Unit Configuration.unset_use_default_lib,
       " does not include by default the standard library installation\n\t\
         directory in the search path.") ;
      ("-ocaml-comp-mode",
       Arg.String Configuration.set_ml_compiler,
       " specify the OCaml compiler mode. Can be \"byt\" for bytecode \
         compilation, \"bin\" for native code compilation, or \"both\" \
         for bytecode and native code compilation.") ;
      ("-pretty",
       Arg.String Configuration.set_pretty_print,
       " pretty-prints the parse tree of the focalize file as a focalize\n\
         source into the argument file.") ;
      ("-raw-ast-dump",
       Arg.Unit Configuration.set_raw_ast_dump,
       " (undocumented) prints on stderr the raw AST structure \
         after\n\tthe parsing stage.") ;
      ("-scoped-pretty",
       Arg.String Configuration.set_pretty_scoped,
       " (undocumented) pretty-prints the parse tree of the focalize \
         file\n\tonce scoped as a focalize source into the argument file.") ;
      ("-stop-before-coq",
       Arg.Unit Configuration.set_stop_before_coq,
       " when Coq code generation is activated, stops the compilation process \
         before passing the generated file to Coq. The produced file is \
         ended by the suffix \".v\".") ;
      ("-stop-before-zenon",
       Arg.Unit Configuration.set_stop_before_zenon,
       " when Coq code generation is activated, stops the compilation process \
         before passing the generated file to Zenon. The produced file is \
         ended by the suffix \".zv\".") ;
      ("-verbose",
       Arg.Unit Configuration.set_verbose,
       " be verbose.") ;
      ("-v", Arg.Unit Configuration.print_focalize_short_version,
       " prints the focalize version then exit.") ;
      ("-version",
       Arg.Unit Configuration.print_focalize_full_version,
       " prints the full focalize version, sub-version and release date,\n\t\
         then exit.") ;
       ("-where",
        Arg.Unit Configuration.print_install_dirs,
        " prints the binaries and libraries installation directories then \
          exit.")
     ]
    Configuration.add_input_file_name
    "Usage: focalizec [options] <files>" ;
  let file_names = Configuration.get_input_file_names () in
  dispatch_compilation file_names ;
  exit 0
;;
