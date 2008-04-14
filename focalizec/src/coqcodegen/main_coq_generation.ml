(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            Fran�ois Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: main_coq_generation.ml,v 1.10 2008-04-14 09:20:49 pessaux Exp $ *)


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
  | Infer.PCM_coq_require fname ->
      Format.fprintf out_fmter "@[<2>Require@ %s@].@\n" fname ;
      env
  | Infer.PCM_species (species_def, species_descr, dep_graph) ->
      let spe_binding_info =
        Species_coq_generation.species_compile
          ~current_unit env out_fmter species_def species_descr dep_graph in
      (* Return the coq code generation environment extended *)
      (* by the current species's information.               *)
      Env.CoqGenEnv.add_species
        ~loc: species_def.Parsetree.ast_loc
        species_def.Parsetree.ast_desc.Parsetree.sd_name
        spe_binding_info env
  | Infer.PCM_collection (_, _, _) ->
      Format.fprintf out_fmter "Infer.PCM_collection TODO@." ;
      (* [Unsure] *) env
  | Infer.PCM_type (type_def_name, type_descr) ->
      (* Create the initial context for compiling the type definition. *)
      let ctx = {
        Context.rcc_current_unit = current_unit ;
        (* Not under a species, hence no species parameter. *)
        Context.rcc_species_parameters_names = [] ;
        (* Not under a species, hence empty carriers mapping. *)
        Context.rcc_collections_carrier_mapping = [] ;
        (* Not in the context of generating a method's body code, then empty. *)
        Context.rcc_lambda_lift_params_mapping = [] ;
        Context.rcc_out_fmter = out_fmter } in
      Type_coq_generation.type_def_compile ctx env type_def_name type_descr
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
  (* Always import Coq booleans and integers and floats. *)
  (* Alias int notation to Z.                            *)
  Format.fprintf out_fmter
    "Require Export Bool.@\n\
     Require Export ZArith.@\n\
     Open Scope Z_scope.@\n\
     Require Export Reals.@\n\
     Require Export Ascii.@\n\
     Require Export String.@\n\
     Require Export List.@\n\
     Require Export coq_builtins.@\n@\n" ;
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
      Format.eprintf
        "Error@ while@ trying@ to@ keep@ trace@ of@ the@ partially@ \
         generated@ Coq@ code:@ %s.@\nInitial@ error@ follows.@."
        (Printexc.to_string second_error)
      end)
    end) ;
    (* Re-reaise the initial error. *)
    raise whatever
;;
