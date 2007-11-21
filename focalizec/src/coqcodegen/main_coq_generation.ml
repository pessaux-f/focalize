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

(* $Id: main_coq_generation.ml,v 1.1 2007-11-21 16:34:15 pessaux Exp $ *)


(* ************************************************************************** *)
(** {b Descr} : This module is the entry point for the compilation from FoCaL
      to Coq. It dispatches the compilation of each possible FoCaL entity
      to the dedicated compilation module.                                    *)
(* ************************************************************************** *)



(* *********************************************************************** *)
(** {b Descr} : Dispatch the Coq code generation of a toplevel structure
              to the various more specialized code generation routines.

    {b Arg} :
      - [current_unit] : The name of the current compilation unit (i.e.
                       the name of the file without extension and not
                       capitalized).
      - [out_fmter] : The out channel where to generate the Coq source
                    code.
      - unnamed : The structure for which the Coq source code has to be
                generated.

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let toplevel_compile ~current_unit out_fmter = function
  | Infer.PCM_no_matter -> ()
  | Infer.PCM_open (_, modname) ->
      (* One must let known that the module is required. In fact it should *)
      (* also be "Import" but because the compiler always generate fully   *)
      (* qualified stuff, the notion of "opended" is not needed anymore    *)
      (* in the code generated for Coq.                                    *)
      Format.fprintf out_fmter "@[<2>Require@ %s@].@\n" modname
  | Infer.PCM_species (species_def, species_descr, dep_graph) ->
      Species_coq_generation.species_compile
        ~current_unit out_fmter species_def species_descr dep_graph
  | Infer.PCM_collection (_, _, _) ->
      Format.fprintf out_fmter "Infer.PCM_collection TODO@."
  | Infer.PCM_type (_, _) ->
      Format.fprintf out_fmter "Infer.PCM_type TODO@."
  | Infer.PCM_let_def (_, _) ->
      Format.fprintf out_fmter "Infer.PCM_let_def TODO@."
  | Infer.PCM_theorem _ ->
      Format.fprintf out_fmter "Infer.PCM_theorem TODO@."
  | Infer.PCM_expr _ ->
      Format.fprintf out_fmter "Infer.PCM_expr TODO@."
 ;;



let root_compile ~current_unit ~out_file_name stuff =
  if Configuration.get_verbose () then
    Format.eprintf "Starting Coq code generation.@." ;
  let out_hd = open_out_bin out_file_name in
  let out_fmter = Format.formatter_of_out_channel out_hd in
  try
    List.iter
      (fun data -> toplevel_compile ~current_unit out_fmter data)
      stuff ;
    (* Flush the pretty-printer. *)
    Format.fprintf out_fmter "@?" ;
    close_out out_hd ;
  with whatever ->
    (* In any error case, flush the pretty-printer and close the outfile. *)
    Format.fprintf out_fmter "@?" ;
    close_out out_hd ;
    (begin
    try
      (* And rename it to prevent an incorrecty Coq source file  *)
      (* from remaining, but to still keep a trace of what could *)
      (* be generated until the error arose.                     *)
      let trace_filename = out_file_name ^ ".mangled" in
      (* If the file of trace already exists, then first *)
      (* discard it to prevent OS file I/O errors.       *)
      if Sys.file_exists trace_filename then Sys.remove trace_filename ;
      Sys.rename out_file_name trace_filename ;
    with second_error ->
      (begin
      (* Here we want to catch errors that can arise during the trace file  *)
      (* stuff. Because we don't want these errors to hide the real initial *)
      (* problem that made the code generation impossible, we first process *)
      (* here I/O errors, then will be raise again the initial error.       *)
      Format.eprintf "Error@ while@ trying@ to@ keep@ trace@ of@ the@ partially@ generated@ Coq@ code:@ %s.@\nInitial@ error@ follows.@."
        (Printexc.to_string second_error)
      end)
    end) ;
    (* Re-reaise the initial error. *)
    raise whatever
;;
