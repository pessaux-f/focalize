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

(* $Id: main_ml_generation.ml,v 1.1 2007-10-09 08:37:35 pessaux Exp $ *)


(* ************************************************************************** *)
(** {b Descr} : This module is the entry point for the compilation from FoCaL
      to Ocaml. It dispatches the compilation of each possible FoCaL entity
      to the dedicated compilation module.                                    *)
(* ************************************************************************** *)



(* *********************************************************************** *)
(* current_unit: Types.fname -> Format.formatter ->                        *)
(*  Infer.please_compile_me -> unit                                        *)
(** {b Descr} : Dispatch the OCaml code generation of a toplevel structure
              to the various more specialized code generation routines.

    {b Arg} :
      - [current_unit] : The name of the current compilation unit (i.e.
                       the name of the file without extension and not
                       capitalized).
      - [out_fmter] : The out channel where to generate the OCaml source
                    code.
      - unnamed : The structure for which the OCaml source code has to be
                generated.

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let toplevel_compile ~current_unit out_fmter = function
  | Infer.PCM_no_matter -> ()
  | Infer.PCM_external extern_def ->
      (* Create the initial context for compiling the let-definition. *)
      let ctx = {
	Misc_ml_generation.rcc_current_unit = current_unit ;
	Misc_ml_generation.rcc_out_fmter = out_fmter } in
      Externals_ml_generation.external_def_compile ctx extern_def
  | Infer.PCM_species (species_def, species_descr, dep_graph) ->
      Species_ml_generation.species_compile
	~current_unit out_fmter
	species_def.Parsetree.ast_desc.Parsetree.sd_name species_descr dep_graph
  | Infer.PCM_collection (coll_def, coll_descr, dep_graph) ->
      Species_ml_generation.collection_compile
	~current_unit out_fmter	coll_def coll_descr dep_graph
  | Infer.PCM_type (type_def_name, type_descr) ->
      (* Create the initial context for compiling the type definition. *)
      let ctx = {
	Misc_ml_generation.rcc_current_unit = current_unit ;
	Misc_ml_generation.rcc_out_fmter = out_fmter } in
      Type_ml_generation.type_def_compile ctx type_def_name type_descr
  | Infer.PCM_let_def (let_def, def_schemes) ->
      (* Create the initial context for compiling the let-definition. *)
      let ctx = {
	Misc_ml_generation.rcc_current_unit = current_unit ;
	Misc_ml_generation.rcc_out_fmter = out_fmter } in
      (* We have the schemes under the hand. Then we will be able    *)
      (* to annotate the parameters of the toplevel let-bound idents *)
      (* with type constraints.                                      *)
      let bound_schemes = List.map (fun sch -> Some sch) def_schemes in
      Base_exprs_ml_generation.let_def_compile ctx let_def bound_schemes ;
      Format.fprintf out_fmter "@\n;;@\n"
  | Infer.PCM_theorem _ -> ()  (* Theorems do not lead to OCaml code. *)
  | Infer.PCM_expr expr ->
      let ctx = {
	Misc_ml_generation.rcc_current_unit = current_unit ;
	Misc_ml_generation.rcc_out_fmter = out_fmter
      } in
      Base_exprs_ml_generation.generate_expr ctx expr ;
      (* Generate the final double-semis. *)
      Format.fprintf out_fmter "@ ;;@\n"
;;



let root_compile ~current_unit ~out_file_name stuff =
  let out_hd = open_out_bin out_file_name in
  let out_fmter = Format.formatter_of_out_channel out_hd in
  try
    List.iter (toplevel_compile ~current_unit out_fmter) stuff ;
    (* Flush the pretty-printer. *)
    Format.fprintf out_fmter "@?" ;
    close_out out_hd
  with whatever ->
    (* In any error case, flush the pretty-printer and close the outfile. *)
    Format.fprintf out_fmter "@?" ;
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
