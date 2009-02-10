(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                                                                     *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007, 2008 LIP6 and INRIA                                *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: make_depend.ml,v 1.5 2009-02-10 10:53:58 pessaux Exp $ *)


type dep_kind =
  | DK_use_open
  | DK_coq_require
;;


(* *********************************************************************** *)
(** {b Descr}: The name of the file where we store the dependencies in the
    format "make" expects.

    {b Rem}: Not exported outside this module.                             *)
(* *********************************************************************** *)
module CompUnitMod = struct
  type t = (string * dep_kind)
  let compare = compare
end ;;
module CompUnitSet = Set.Make (CompUnitMod) ;;



(* ********************************************************************** *)
(** {b Descr}: Describes for the compilation unit [fd_comp_unit] the set
    of compilation it has compilation (i.e. for make) dependencies it has
    on.

    {b Rem}: Not exported outside this module.                            *)
(* ********************************************************************** *)
type file_dependency = {
  fd_comp_unit : Parsetree.module_name ;  (** Base name without extension of
                                              the file for which we compute the
                                              dependencies. In other words,
                                              what we call a "compilation
                                              unit". *)
  fd_dependencies : CompUnitSet.t (** Full path WITHOUT extension of the files
                                      we (i.e.[fd_comp_unit]) depend on. *)
} ;;



let process_one_file fname =
  let in_hd = open_in_bin fname in
  let lexbuf = Lexing.from_channel in_hd in
  let continue = ref true in
  let comp_units = ref CompUnitSet.empty in
  while !continue do
    match Directive_lexer.start lexbuf with
     | Directive_lexer.D_end -> continue := false
     | Directive_lexer.D_use_open mod_name ->
	 (begin
	 match Files.get_path_from_lib_paths (mod_name ^ ".fcl") with
	  | None -> ()
	  | Some p ->
              let unit = Filename.concat p mod_name in
	      comp_units := CompUnitSet.add (unit, DK_use_open) !comp_units
	 end)
     | Directive_lexer.D_coq_require mod_name ->
	 (begin
	 match Files.get_path_from_lib_paths (mod_name ^ ".fcl") with
	  | None -> ()
	  | Some p ->
              let unit = Filename.concat p mod_name in
              comp_units := CompUnitSet.add (unit, DK_coq_require) !comp_units
	 end)
  done ;
  close_in in_hd ;
  let basename = Filename.chop_suffix (Filename.basename fname) ".fcl" in
  { fd_comp_unit = basename ;
    fd_dependencies = !comp_units }
;;



let make_targets deps =
  let basename = deps.fd_comp_unit in
  (* Handle dependencies for .fo files. *)
  Printf.printf "%s.fo:" basename ;
  CompUnitSet.iter
    (fun (n, k) -> if k <> DK_coq_require then Printf.printf " %s.fo" n)
    deps.fd_dependencies ;
  Printf.printf "\n" ;
  (* Handle dependencies for .ml files. *)
  Printf.printf "%s.ml:" basename ;
  CompUnitSet.iter
    (fun (n, k) -> if k <> DK_coq_require then Printf.printf " %s.ml" n)
    deps.fd_dependencies ;
  Printf.printf "\n" ;
  (* Handle dependencies for .zv files. *)
  Printf.printf "%s.zv:" basename ;
  CompUnitSet.iter
    (fun (n, k) -> if k <> DK_coq_require then Printf.printf " %s.zv" n)
    deps.fd_dependencies ;
  Printf.printf "\n" ;
  (* Handle dependencies for .v files. Dependencies from directives
     "coq_require" must be printed. *)
  Printf.printf "%s.v:" basename ;
  CompUnitSet.iter
    (fun (n, _) -> Printf.printf " %s.v" n)
    deps.fd_dependencies ;
  Printf.printf "\n" ;
  (* No dependencies for .cmi files since we do not generate .mli files. *)
  (* Handle dependencies for .cmo files. *)
  Printf.printf "%s.cmo:" basename ;
  CompUnitSet.iter
    (fun (n, k) -> if k <> DK_coq_require then Printf.printf " %s.cmo" n)
    deps.fd_dependencies ;
  Printf.printf "\n" ;
  (* Handle dependencies for .cmx files. *)
  Printf.printf "%s.cmx:" basename ;
  CompUnitSet.iter
    (fun (n, k) -> if k <> DK_coq_require then Printf.printf " %s.cmx" n)
    deps.fd_dependencies ;
  Printf.printf "\n" ;
  (* Handle dependencies for .vo files. Dependencies from directives
     "coq_require" must be printed. *)
  Printf.printf "%s.vo:" basename ;
  CompUnitSet.iter
    (fun (n, _) -> Printf.printf " %s.vo" n)
    deps.fd_dependencies ;
  Printf.printf "\n"
;;



let main () =
  (* The list of files to process in reverse order for sake of efficiency. *)
  let filenames = ref [] in
  try
    Arg.parse
      [ ("-I",
	 Arg.String (fun path -> Files.add_lib_path path),
	 " adds the specified path to the path list where to search for \
           compiled\n\tinterfaces.") ]
      (fun n -> filenames := n :: !filenames)
      "Usage: focalizec <files>" ;
    (* We don't include the standard lib path so that dependencies on files
       of the standard lib won't appear. *)
    let deps = List.map process_one_file (List.rev !filenames) in
    List.iter make_targets deps ;
    exit 0
  with
    | Sys_error m ->
        Printf.eprintf "System@ error - %s.\n" m ;
        exit (-1) ;
     (* ********************** *)
     (* The ultimate firewall. *)
    | x ->
        Printf.eprintf "Unexpected error: \"%s\".\nPlease report.\n"
          (Printexc.to_string x) ;
        exit (-1)
;;



main () ;;
