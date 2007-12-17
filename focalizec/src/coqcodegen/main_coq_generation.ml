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

(* $Id: main_coq_generation.ml,v 1.5 2007-12-17 14:31:05 pessaux Exp $ *)


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
let toplevel_compile env ~current_unit out_fmter = function
  | Infer.PCM_no_matter -> env
  | Infer.PCM_open (phrase_loc, modname) ->
      (* One must let known that the module is required. In fact it should *)
      (* also be "Import" but because the compiler always generate fully   *)
      (* qualified stuff, the notion of "opended" is not needed anymore    *)
      (* in the code generated for Coq.                                    *)
      Format.fprintf out_fmter "@[<2>Require@ %s@].@\n" modname ;
      (* One must "open" the coq code generation environment of this module *)
      (* and return the environment extended with these "opened" bindings.  *)
      Env.coqgen_open_module ~loc: phrase_loc modname env
  | Infer.PCM_species (species_def, species_descr, dep_graph) ->
      Species_coq_generation.species_compile
        ~current_unit env out_fmter species_def species_descr dep_graph ;
      (* [Unsure] *) env
  | Infer.PCM_collection (_, _, _) ->
      Format.fprintf out_fmter "Infer.PCM_collection TODO@." ;
      (* [Unsure] *) env
  | Infer.PCM_type (_, _) ->
      Format.fprintf out_fmter "Infer.PCM_type TODO@." ;
      (* [Unsure] *) env
  | Infer.PCM_let_def (_, _) ->
      Format.fprintf out_fmter "Infer.PCM_let_def TODO@." ;
      (* [Unsure] *) env
  | Infer.PCM_theorem _ ->
      Format.fprintf out_fmter "Infer.PCM_theorem TODO@." ;
      (* [Unsure] *) env
  | Infer.PCM_expr _ ->
      Format.fprintf out_fmter "Infer.PCM_expr TODO@." ;
      (* [Unsure] *) env
 ;;



let root_compile ~current_unit ~out_file_name stuff =
  if Configuration.get_verbose () then
    Format.eprintf "Starting Coq code generation.@." ;
  let out_hd = open_out_bin out_file_name in
  let out_fmter = Format.formatter_of_out_channel out_hd in
  let global_env = ref (Env.CoqGenEnv.empty ()) in
  (* Always import Coq booleans and integers. Alias int notation to Z. *)
  Format.fprintf out_fmter
    "Require Export Bool.@\nRequire Export ZArith.@\nOpen Scope Z_scope.@\n@\n" ;
  try
    List.iter
      (fun data ->
        let new_env =
          toplevel_compile !global_env ~current_unit out_fmter data in
        global_env := new_env)
      stuff ;
    (* Flush the pretty-printer. *)
    Format.fprintf out_fmter "@?" ;
    close_out out_hd ;
    !global_env
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
