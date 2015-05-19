(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            François Pessaux                                                *)
(*            Pierre Weis                                                     *)
(*            Damien Doligez                                                  *)
(*                                                                            *)
(*               LIP6  --  INRIA Rocquencourt -- ENSTA ParisTech              *)
(*                                                                            *)
(*  Copyright 2007 - ... LIP6 and INRIA                                       *)
(*            2012 - ... ENSTA ParisTech                                      *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)

exception Bad_file_suffix of string ;;



let compile_fcl input_file_name =
  (* First, let's lex and parse the input source file.
     Create the current compilation unit "fname". In fact, this is the current
     filename without dirname and extention. *)
  let current_unit =
    Filename.chop_extension (Filename.basename input_file_name) in
  (* Include the installation libraries directory in the search path. At the
     present point, all the user -I options are already processed, so adding
     the stdlib directory now will make it be the least important path, hence
     PREVENTS from a file of the stdlib hidding a file of the same name in the
     user directory ! *)
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
  let plug_ins = Configuration.get_plugins () in
  let plug_in_funs =
    List.map
      (function
       | "relation_extraction" -> Rel_ext.extract
       | s -> failwith (Printf.sprintf "Unknown plugin %s" s))
      (List.rev plug_ins) in
  let ast = List.fold_left (fun ast f -> f ast) ast plug_in_funs in
  (* Scopes AST. *)
  if Configuration.get_verbose () then Format.eprintf "Scoping AST.@." ;
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
  if Configuration.get_verbose () then Format.eprintf "Typechecking AST.@." ;
  let (typing_toplevel_env, stuff_to_compile) =
    Infer.typecheck_file ~current_unit scoped_ast in
  (* Verify pattern-matching soundness. *)
  if Configuration.get_verbose () then
    Format.eprintf "Checking pattern-matching exhaustivity.@." ;
  List.iter
    (Match_analysis.verify_matchings ~current_unit typing_toplevel_env)
    stuff_to_compile ;
  (* Generate the documentation if requested. *)
  if Configuration.get_focalize_doc () then
    Main_docgen.gen_doc_please_compile_me
      input_file_name scoped_ast stuff_to_compile;
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
  (* Go to the Coq code generation if requested and generate the
     .zv file . *)
  let coqgen_toplevel_env =
    if Configuration.get_generate_coq () then (
      let out_file_name = (Filename.chop_extension input_file_name) ^ ".zv" in
      Some
        (Main_coq_generation.root_compile
           ~current_unit ~out_file_name stuff_to_compile)
     )
    else None in
  (* Finally, go to the Dedukti code generation if requested and generate the
     .sk file . *)
  let dkgen_toplevel_env =
    if Configuration.get_generate_dk () then (
      let out_file_name = (Filename.chop_extension input_file_name) ^ ".sk.zv" in
      Some
        (Main_dk_generation.root_compile
           ~current_unit ~out_file_name stuff_to_compile)
     )
    else None in
  (* Generate tests if requested and if testing instructions
     are contained in the file (this avoids the creation of
     testings of testings). *)
  (* if Configuration.get_generate_tests () *)
  (*     && Testing.contains_testing_intructions scoped_ast then *)
  (*   (begin *)
  (*     let output_file_name = *)
  (*       (Testing.add_tests_suffix *)
  (*          (Filename.chop_extension input_file_name)) *)
  (*       ^ ".fcl" in *)
  (*     Testing.gen_tests *)
  (*       ~current_unit ~output_file_name scoped_ast; *)
  (*   end); *)
  (* Now, generate the persistent interface file. *)
  Env.make_fo_file
    ~source_filename: input_file_name
    scoping_toplevel_env typing_toplevel_env mlgen_toplevel_env
    coqgen_toplevel_env dkgen_toplevel_env
;;



let compile_ml input_file_name =
  (* We include the library search paths for OCaml. [Files.get_lib_paths]
     returns the paths already in the order they were given in the options. *)
  let includes = String.concat " -I " ("" :: (Files.get_lib_paths ())) in
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
       if ret_code <> 0 then exit ret_code;
       let cmd = Installation.caml_bin_compiler ^ args in
       Format.eprintf "Invoking ocamlopt...@\n";
       Format.eprintf ">> %s@." cmd;
       let ret_code = Sys.command cmd in
       if ret_code <> 0 then exit ret_code
;;



let compile_zv input_file_name =
  let cmd =
    Printf.sprintf "%s -new %s -z '-x induct' %s"
      Installation.zvtov_compiler (* Installation.zenon_compiler *)
      (Configuration.get_zvtov_extra_opts ())
      input_file_name in
  Format.eprintf "Invoking zvtov...@\n" ;
  Format.eprintf ">> %s@." cmd ;
  let ret_code = Sys.command cmd in
  if ret_code <> 0 then exit ret_code
;;



let compile_coq input_file_name =
  (* Coq always requires Zenon .v files. *)
  let for_zenon = " -I " ^ Installation.zenon_libdir in
  (* We include the library search paths for OCaml. [Files.get_lib_paths]
     returns the paths in the order they were given in the options, but
     Coq searches first in the last -I paths. Since our semantics of the search
     path is not like this (we behave like OCaml and GCC), we must reverse
     this list for Coq.*)
  let includes =
    String.concat " -I " ("" :: (List.rev (Files.get_lib_paths ()))) in
  let cmd =
    Printf.sprintf "%s %s %s %s"
      Installation.coq_compiler includes for_zenon input_file_name in
  Format.eprintf "Invoking coqc...@\n";
  Format.eprintf ">> %s@." cmd;
  let ret_code = Sys.command cmd in
  if ret_code <> 0 then exit ret_code
;;


let compile_zdk input_file_name =
  let cmd =
    Printf.sprintf "%s -verbose -idedukti -zenon %s -new %s %s.zv && mv %s.v %s"
      Installation.zvtov_compiler Installation.zenon_compiler
      (Configuration.get_zvtov_extra_opts ())
      input_file_name
      input_file_name (Filename.basename input_file_name)
  in
  Format.eprintf "Invoking zvtov...@\n" ;
  Format.eprintf ">> %s@." cmd ;
  let ret_code = Sys.command cmd in
  if ret_code <> 0 then exit ret_code
;;



let compile_dk input_file_name =
  let cmd =
    Printf.sprintf "%s %s | %s -e -stdin"
      Installation.sukerujo_compiler input_file_name
      Installation.dedukti_compiler in
  Format.eprintf "Invoking dkcheck...@\n";
  Format.eprintf ">> %s@." cmd;
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
          (* If a .ml file was generated, let's compile it. *)
          if Configuration.get_generate_ocaml () then
            compile_ml (input_file_no_suffix ^ ".ml") ;
          if Configuration.get_generate_coq () then (
            if not (Configuration.get_stop_before_zenon ()) then (
              (* If a .zv file was generated, let's compile it. *)
              compile_zv (input_file_no_suffix ^ ".zv") ;
              (* Finally, pass it to Coq. *)
              if not (Configuration.get_stop_before_coq ()) then
                compile_coq (input_file_no_suffix ^ ".v")
             )
           );
          if Configuration.get_generate_dk () then (
            if not (Configuration.get_stop_before_zenon ()) then (
              (* If a .zdk file was generated, let's compile it. *)
              compile_zdk (input_file_no_suffix ^ ".sk") ;
              (* Finally, pass it to Dedukti. *)
              if not (Configuration.get_stop_before_dk ()) then
                compile_dk (input_file_no_suffix ^ ".sk")
             )
           );
          (* let tests_file_no_suffix = *)
          (*   Testing.add_tests_suffix input_file_no_suffix in *)
          (* let tests_file_fcl = tests_file_no_suffix ^ ".fcl" in *)
          (* let tests_file_ml = tests_file_no_suffix ^ ".ml" in *)
          (* if Configuration.get_generate_tests () *)
          (*     && Configuration.get_perform_tests () *)
          (*     && Sys.file_exists tests_file_fcl then *)
          (*   (begin *)
          (*     compile_fcl tests_file_fcl; *)
          (*     compile_ml tests_file_ml; *)
          (*   end); *)
      | "ml" | "mli" -> compile_ml input_file_name
      | "zv" ->
          compile_zv input_file_name;
          (* Finally, pass it to Coq or Dedukti. *)
          let input_file_no_suffix = Filename.chop_extension input_file_name in
          let suffix2 = String.lowercase
            (Files.get_file_name_suffix input_file_no_suffix) in
          if suffix2 = ".dk" then
            compile_dk (input_file_no_suffix)
          else
            compile_coq (input_file_no_suffix ^ ".v") ;
      | "v" -> compile_coq input_file_name
      | "sk" -> compile_dk input_file_name
      | _ -> raise (Bad_file_suffix input_file_name)
    )
    files
;;




(* The main procedure *)
let main () =
  Arg.parse
    [ ("-coq_older",
       Arg.Unit Configuration.set_use_coq_older,
       "  Enable Coq code generation for versions of Coq < 8.3pl2.");
      ("-dot-non-rec-dependencies",
       Arg.String Configuration.set_dotty_dependencies,
       "  Dump species non-let-rec dependencies as dotty\n\
     \    files into the argument directory.");
      ("--experimental",
       Arg.Unit Configuration.set_experimental,
       "  Do not use. Fear it! For the development team only!");
      ("-focalize-doc",
       Arg.Unit Configuration.set_focalize_doc,
       "  Generate documentation.");
      ("-i",
       Arg.Unit (fun () -> Configuration.set_do_interface_output true),
       "  Print the source file interface.");
      ("-I",
       Arg.String (fun path -> Files.add_lib_path path),
       "<dir>  Add the specified <dir> to the path list where to search for\n\
     \    compiled interfaces.");
      ("-impose-termination-proof",
       Arg.Unit Configuration.set_impose_termination_proof,
       "  Make termination proofs of recursive functions\n\
     \    mandatory.");
      ("-methods-history-to-text",
       Arg.String Configuration.set_methods_history_to_text,
       "  Dump species' methods' inheritance history as plain\n\
     \    text files into the argument directory.");
      ("-methods-history-to-dotty",
       Arg.String Configuration.set_methods_history_to_dotty,
       "  Dump species' methods' inheritance history as\n\
     \    Graphwiz (.dot) files into the argument directory.");
      ("-no-ansi-escape",
       Arg.Unit Configuration.unset_fancy_ansi,
       "  Disable ANSI escape sequences in the error messages.");
      ("-no-coq-code",
       Arg.Unit Configuration.unset_generate_coq,
       "  Disable the Coq code generation.");
      ("-no-dedukti-code",
       Arg.Unit Configuration.unset_generate_dk,
       "  Disable the Dedukti code generation.");
      ("-no-ocaml-code",
       Arg.Unit Configuration.unset_generate_ocaml,
       "  Disable the OCaml code generation.");
      ("-no-test-code",
       Arg.Unit Configuration.unset_perform_tests,
       "  Disable the test code generation.");
      ("-no-tests",
       Arg.Unit Configuration.unset_perform_tests,
       "  Disable the tests.");
      ("-no-stdlib-path",
       Arg.Unit Configuration.unset_use_default_lib,
       "  Do not include by default the standard library installation\n\
     \    directory in the search path.");
      ("-ocaml-comp-mode",
       Arg.String Configuration.set_ml_compiler,
       "  Specify the OCaml compiler mode. Can be \"byt\" for bytecode\n\
     \    compilation, \"bin\" for native code compilation, or \"both\" for bytecode\n\
     \    and native code compilation.");
      ("-pmatch-err-as-warn",
       Arg.Unit Configuration.set_pmatch_err_as_warn,
       "  Consider uncomplete/useless pattern-matching as warnings\n\
     \    instead of errors.");
      ("-pretty",
       Arg.String Configuration.set_pretty_print,
       "<output>  Pretty-print the parse tree of the FoCaLize file as a\n\
     \    FoCaLize source into the <output> file.");
      ("-raw-ast-dump",
       Arg.Unit Configuration.set_raw_ast_dump,
       "  (Undocumented) Print on stderr the raw AST structure after the\n\
     \    parsing stage.");
      ("-require-plugin",
       Arg.String Configuration.require_plugin,
       "<plugin name>  Requires application of plugin <plugin name> on\n\
     \    the FoCaLize source file.");
      ("-scoped-pretty",
       Arg.String Configuration.set_pretty_scoped,
       "  (Undocumented) Pretty-print the parse tree of the FoCaLize\n\
     \    file once scoped as a FoCaLize source into the argument file.");
      ("-stop-before-coq",
       Arg.Unit Configuration.set_stop_before_coq,
       "  When Coq code generation is activated, stop the compilation\n\
     \    process before passing the generated file to Coq. The produced file\
          is ended\n\
     \    by the suffix \".v\".");
      ("-stop-before-dk",
       Arg.Unit Configuration.set_stop_before_dk,
       "  When Dedukti code generation is activated, stop the compilation\n\
     \    process before passing the generated file to Dedukti. The produced file\
          is ended\n\
     \    by the suffix \".dk\".");
      ("-stop-before-zenon",
       Arg.Unit Configuration.set_stop_before_zenon,
       "  When Coq/Dedukti code generation is activated, stop the \n\
     \    compilation process before passing the generated file to Zenon.\n\
     \    The produced file is ended by the suffix \".zv\" or \".zdk\".");
      ("-verbose",
       Arg.Unit Configuration.set_verbose,
       "  Be verbose. Make the compiler jaberring about its real-time life.");
      ("-v", Arg.Unit Configuration.print_focalize_short_version,
       "  Print the focalize version then exit.");
      ("-version",
       Arg.Unit Build_configuration.print_focalize_full_version,
       "  Print the full focalize version, sub-version and release date, then\n\
     \    exit.");
       ("-where",
        Arg.Unit Build_configuration.print_install_dirs,
        "  Print the binaries and libraries installation directories then \
           exit.");
       ("-zvtovopt",
        Arg.String Configuration.set_zvtov_extra_opts,
        "  \"<options>\"  Set options to pass to zvtov. Zvtov is anyway \
          always\n\
     \    called with options \"-zenon (path to Zenon) -new\" in head.")
     ]
    Configuration.add_input_file_name
    "Usage: focalizec [options] <files>";
  let file_names = Configuration.get_input_file_names () in
  dispatch_compilation file_names ;
  exit 0
;;
