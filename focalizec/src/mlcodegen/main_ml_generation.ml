(***********************************************************************)
(*                                                                     *)
(*                        FoCaLiZe compiler                            *)
(*                                                                     *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                                                                     *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: main_ml_generation.ml,v 1.22 2009-06-16 10:08:19 weis Exp $ *)


(* ************************************************************************** *)
(** {b Descr} : This module is the entry point for the compilation from FoCaL
      to Ocaml. It dispatches the compilation of each possible FoCaL entity
      to the dedicated compilation module.                                    *)
(* ************************************************************************** *)



(* *********************************************************************** *)
(* Env.MlGenEnv.t -> current_unit: Types.fname -> Format.formatter ->      *)
(*  Infer.please_compile_me -> Env.MlGenEnv.t                              *)
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
let toplevel_compile env ~current_unit out_fmter = function
  | Infer.PCM_annotation_title -> env
  | Infer.PCM_use (_, _) -> env
  | Infer.PCM_open (phrase_loc, fname) ->
      (* One must "open" the ml code generation environment of this module and
         return the environment extended with these "opened" bindings. *)
      Env.mlgen_open_module ~loc: phrase_loc fname env
  | Infer.PCM_coq_require _ ->
      (* Really nothing to do... *)
      env
  | Infer.PCM_species (species_def, species_descr, dep_graph) ->
      let species_binding_info =
        Species_ml_generation.species_compile
          env ~current_unit out_fmter species_def species_descr dep_graph in
      (* Return the ml code generation environment extended by the current
         species's collection generator information. *)
      Env.MlGenEnv.add_species
        ~loc: species_def.Parsetree.ast_loc
        species_def.Parsetree.ast_desc.Parsetree.sd_name
        species_binding_info env
  | Infer.PCM_collection (coll_def, coll_descr, dep_graph) ->
      Species_ml_generation.collection_compile
        env ~current_unit out_fmter coll_def coll_descr dep_graph;
      (* Collections don't have parameters or any remaining abstraction.
         Moreover, since we can never inherit of a collection, just forget all
         methods information, it will never be used.
         Collections do not have collection generator, then simply add them in
         the environment with None. *)
      Env.MlGenEnv.add_species
        ~loc: coll_def.Parsetree.ast_loc
        coll_def.Parsetree.ast_desc.Parsetree.cd_name
        ([], [], None, Env.COS_collection) env
  | Infer.PCM_type (type_def_name, type_descr) ->
      (* Create the initial context for compiling the type definition. *)
      let ctx = {
        Context.rcc_current_unit = current_unit;
        (* Not under a species, hence no species parameter. *)
        Context.rcc_species_parameters_names = [];
        (* Not under a species, hence empty carriers mapping. *)
        Context.rcc_collections_carrier_mapping = [];
        (* Not in the context of generating a method's body code, then empty. *)
        Context.rcc_lambda_lift_params_mapping = [];
        Context.rcc_out_fmter = out_fmter } in
      Type_ml_generation.type_def_compile ctx env type_def_name type_descr
  | Infer.PCM_let_def (let_def, def_schemes) ->
      (* Create the initial context for compiling the let-definition. *)
      let ctx = {
        Context.rcc_current_unit = current_unit;
        (* Not under a species, hence no species parameter. *)
        Context.rcc_species_parameters_names = [];
        (* Not under a species, hence empty carriers mapping. *)
        Context.rcc_collections_carrier_mapping = [];
        (* Not in the context of generating a method's body code, so, empty. *)
        Context.rcc_lambda_lift_params_mapping = [];
        Context.rcc_out_fmter = out_fmter } in
      (* We have the schemes under the hand. Then we will be able to annotate
         the parameters of the toplevel let-bound idents with type
         constraints. *)
      let bound_schemes = List.map (fun sch -> Some sch) def_schemes in
      (* No local idents in the scope because we are at toplevel. *)
      Base_exprs_ml_generation.let_def_compile
        ctx ~local_idents: [] env let_def bound_schemes;
      Format.fprintf out_fmter "@.;;@.";
      env
  | Infer.PCM_theorem _ -> env  (* Theorems do not lead to OCaml code. *)
  | Infer.PCM_expr expr ->
      let ctx = {
        Context.rcc_current_unit = current_unit;
        (* Not under a species, hence no species parameter. *)
        Context.rcc_species_parameters_names = [];
        (* Not under a species, hence empty carriers mapping. *)
        Context.rcc_collections_carrier_mapping = [];
        (* Not in the context of generating a method's body code, so, empty. *)
        Context.rcc_lambda_lift_params_mapping = [];
        Context.rcc_out_fmter = out_fmter
      } in
      (* No local idents in the scope because we are at toplevel. *)
      Base_exprs_ml_generation.generate_expr ctx env ~local_idents: [] expr;
      (* Generate the final double-semis. *)
      Format.fprintf out_fmter "@.;;@.";
      env
;;



let root_compile ~current_unit ~out_file_name stuff =
  if Configuration.get_verbose () then
    Format.eprintf "Starting OCaml code generation.@.";
  let out_hd = open_out_bin out_file_name in
  let out_fmter = Format.formatter_of_out_channel out_hd in
  let global_env = ref (Env.MlGenEnv.empty ()) in
  try
    List.iter
      (fun data ->
        let new_env =
          toplevel_compile !global_env ~current_unit out_fmter data in
        global_env := new_env)
      stuff;
    (* Flush the pretty-printer. *)
    Format.fprintf out_fmter "@?";
    close_out out_hd;
    !global_env
  with whatever ->
    (* In any error case, flush the pretty-printer and close the outfile. *)
    Format.fprintf out_fmter "@?";
    close_out out_hd;
    (begin
    try
      (* And rename it to prevent an incorrecty OCaml source file from
         remaining, but to still keep a trace of what could be generated until
         the error arose. *)
      let trace_filename = out_file_name ^ ".mangled" in
      (* If the file of trace already exists, then first *)
      (* discard it to prevent OS file I/O errors.       *)
      if Sys.file_exists trace_filename then Sys.remove trace_filename;
      Sys.rename out_file_name trace_filename;
    with second_error ->
      (begin
      (* Here we want to catch errors that can arise during the trace file
         stuff. Because we don't want these errors to hide the real initial
         problem that made the code generation impossible, we first process
         here I/O errors, then will be raise again the initial error. *)
      Format.eprintf
        "Error@ while@ trying@ to@ keep@ trace@ of@ the@ \
         partially@ generated@ OCaml@ code:@ %s.@.\
         Initial@ error@ follows.@."
        (Printexc.to_string second_error)
      end)
    end);
    (* Re-reaise the initial error. *)
    raise whatever
;;
