(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            François Pessaux                                         *)
(*                                                                     *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007, 2008 LIP6 and INRIA                                *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: focalizeig.ml,v 1.1 2012-02-24 14:37:44 pessaux Exp $ *)

exception No_output_file ;;



let err_header ppf =
  Format.fprintf ppf "%tError:%t@ " Handy.pp_set_bold Handy.pp_reset_effects
;;



let dump_inheritance ppf fo_fname =
  let fo_file = open_in_bin fo_fname in
  let (fo_struct : Env.fo_file_structure) =
    if Files.check_magic fo_file Files.fo_magic then input_value fo_file
    else raise (Files.Corrupted_fo fo_fname) in
  close_in fo_file ;
  let current_unit = Filename.chop_extension (Filename.basename fo_fname) in
  (* It's time to inspect each species and each inherit clause. *)
  Env.iter_on_species_scopped
    (fun (species_name, species_info) ->
      let species_info' =
        (match species_info with
        | Env.BO_absolute x | Env.BO_opened (_, x) -> x) in
      List.iter
        (fun { Parsetree.ast_desc = si } ->
          Format.fprintf ppf "  \"%s#%s\" -> \"%a\"\n"
            current_unit (Parsetree_utils.name_of_vname species_name)
            Sourcify.pp_ident si.Parsetree.se_name
        )
        species_info'.Env.ScopeInformation.spbi_inherits
    )
    fo_struct
;;



let main () =
  let fcl_file_names = ref [] in
  let out_file_name = ref "" in
  Arg.parse
    [ ("-o",
       Arg.String (fun fname -> out_file_name := fname),
       "
     <file> Specify the name of the output file.") ;
      ("-version",
       Arg.Unit Configuration.print_focalize_full_version,
       "
     Prints the full focalize version, sub-version and release date, then exit.") ;
      ("-where",
       Arg.Unit Configuration.print_install_dirs,
       "
     Prints the binaries and libraries installation directories then exit.")
     ]
    (fun fname -> fcl_file_names := fname :: !fcl_file_names)
    "Usage: focalizeig <options> <.fo files>
  Dumps the inheritance graph as a Graphwiz (.dot) text file of all the species\
  found in the FoCaLize object files provided as argument.";
      if !fcl_file_names = [] then raise Configuration.No_input_file ;
      if !out_file_name = "" then raise No_output_file ;
      let out_hd = open_out_bin !out_file_name in
      let ppf = Format.formatter_of_out_channel out_hd in
      (* The Graphwiz header. *)
      Format.fprintf ppf "digraph G {\n  node [shape=box];\n" ;
      List.iter (dump_inheritance ppf) !fcl_file_names ;
      (* The Graphwiz trailer. *)
      Format.fprintf ppf "}\n" ;
      close_out out_hd
;;


try main () with
| Files.Corrupted_fo fo_fname ->
    Format.eprintf
      "@[%tInvalid@ or@ corrupted@ compiled@ interface@ '%t%s%t'. Maybe@ \
      it@ was@ compiled@ with@ another@ version@ of@ the@ compiler.@]@."
      err_header Handy.pp_set_underlined fo_fname Handy.pp_reset_effects
| Sys_error m ->
    Format.eprintf "@[%tSystem@ error -@ %s.@]@." err_header m
| Configuration.No_input_file ->
    Format.eprintf
      "@[%tNo@ input@ file(s).@ focalizeig@ prefers@ sleeping...@]@."
      err_header
| No_output_file ->
    Format.eprintf
      "@[%tNo@ output@ file.@ focalizeig@ does@ not@ know@ where@ to@ store@ \
      the@ result.@ Please@ use@ -o@ option.@]@."
      err_header
;;
