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


(* $Id: core_ml_generation.ml,v 1.1 2007-09-14 09:22:41 pessaux Exp $ *)


let toplevel_compile out_hd global_env = function
  | Infer.PCM_no_matter -> ()
  | Infer.PCM_external -> Format.eprintf "Infer.PCM_external expr : TODO@."
  | Infer.PCM_species (species_def, fields) ->
      Format.eprintf "Infer.PCM_species expr : TODO@."
  | Infer.PCM_collection (coll_def, fields) ->
      Format.eprintf "Infer.PCM_collection expr : TODO@."
  | Infer.PCM_type -> Format.eprintf "Infer.PCM_type expr : TODO@."
  | Infer.PCM_let_def let_def ->
      Format.eprintf "Infer.PCM_let_def expr : TODO@."
  | Infer.PCM_theorem theorem_def -> 
      Format.eprintf "Infer.PCM_theorem expr : TODO@."
  | Infer.PCM_expr expr -> Format.eprintf "Infer.PCM_expr expr : TODO@."
;;



let root_compile out_file_name global_env stuff =
  let out_hd = open_out_bin out_file_name in
  try
    List.iter (toplevel_compile out_hd global_env) stuff ;
    close_out out_hd
  with whatever ->
    (* In any error case, close the outfile. *)
    close_out out_hd ;
    (begin
    try
      (* And rename it to prevent an incorrecty OCaml source file   *)
      (* from remaining, but to still keep a trace of what could be *)
      (* generated until the error arose.                           *)
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
      Format.eprintf "Error@ while@ trying@ to@ keep@ trace@ of@ the@ partially@ generated@ OCaml@ code:@ %s.@\nInitial error follows.@."
	(Printexc.to_string second_error)
      end)
    end) ;
    (* Re-reaise the initial error. *)
    raise whatever
;;
