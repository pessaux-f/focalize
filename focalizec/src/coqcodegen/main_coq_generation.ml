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

(* $Id: main_coq_generation.ml,v 1.24 2008-09-12 09:56:19 pessaux Exp $ *)


(* ******************************************************************** *)
(** {b Descr} Exception raised when a toplevel let-definition is tagged
    "logical".

    {b Rem} : Exported outside this module.                             *)
(* ******************************************************************** *)
exception Logical_methods_only_inside_species of Location.t ;;



(* ************************************************************************** *)
(** {b Descr} : This module is the entry point for the compilation from FoCaL
    to Coq. It dispatches the compilation of each possible FoCaL entity to
    the dedicated compilation module.
    It also contains the seed of toplevel let definitions code generation.  *)
(* ************************************************************************** *)



let toplevel_let_def_compile ctx env let_def =
  if let_def.Parsetree.ast_desc.Parsetree.ld_logical = Parsetree.LF_logical then
    raise
      (Logical_methods_only_inside_species let_def.Parsetree.ast_loc) ;
  let out_fmter = ctx.Context.scc_out_fmter in
  let is_rec =
    (match let_def.Parsetree.ast_desc.Parsetree.ld_rec with
     | Parsetree.RF_no_rec -> false
     | Parsetree.RF_rec -> true) in
  (* Generates the binder ("Let" or "Fixpoint"). *)
  (match is_rec with
   | false -> Format.fprintf out_fmter "@[<2>Let@ "
   | true -> Format.fprintf out_fmter "@[<2>Fixpoint@ ") ;
  (* Now generate each bound definition. Remark that there is no local idents
     in the scope because we are at toplevel. In the same way, because we are
     not under the scope of a species, the way "Self" must be printed is
     non-relevant. We use [SMS_from_species] by default. *)
  let env' =
    (match let_def.Parsetree.ast_desc.Parsetree.ld_bindings with
     | [] ->
         (* The "let" construct should always at least bind one identifier ! *)
         assert false
     | [one_bnd] ->
         Species_record_type_generation.let_binding_compile
           ctx ~local_idents: []
           (* Or whatever since "Self" does not exist anymore. *)
           ~self_methods_status: Species_record_type_generation.SMS_from_record
           ~is_rec env one_bnd
     | first_bnd :: next_bnds ->
         let accu_env =
           ref
             (Species_record_type_generation.let_binding_compile
                ctx ~local_idents: []
                (* Or whatever since "Self" does not exist anymore. *)
                ~self_methods_status:
                  Species_record_type_generation.SMS_from_record
                ~is_rec env first_bnd) in
         List.iter
           (fun binding ->
             Format.fprintf out_fmter "@]@\n@[<2>with " ;
             accu_env :=
               Species_record_type_generation.let_binding_compile
                 ctx ~local_idents: []
                 (* Or whatever since "Self" does not exist anymore. *)
                 ~self_methods_status:
                   Species_record_type_generation.SMS_from_record
                 ~is_rec !accu_env binding)
           next_bnds ;
         !accu_env) in
  Format.fprintf out_fmter "@]" ;
  env'
;;



(* ********************************************************************* *)
(** {b Descr} : Dispatch the Coq code generation of a toplevel structure
    to the various more specialized code generation routines.

    {b Arg} :
      - [current_unit] : The name of the current compilation unit (i.e.
        the name of the file without extension and not capitalized).
      - [out_fmter] : The out channel where to generate the Coq source
        code.
      - unnamed : The structure for which the Coq source code has to be
        generated.

    {b Rem} : Not exported outside this module.                          *)
(* ********************************************************************* *)
let toplevel_compile env ~current_unit out_fmter = function
  | Infer.PCM_use (_, modname) ->
      (* One must let known that the module is required. *)
      Format.fprintf out_fmter "@[<2>Require@ %s@].@\n" modname ;
      env
  | Infer.PCM_open (phrase_loc, modname) ->
      (* One must "open" the coq code generation environment of this module
         and return the environment extended with these "opened" bindings.
         In fact it should also be "Import" but because the compiler always
         generate fully qualified stuff, the notion of "opended" is not
         needed anymore in the code generated for Coq. *)
      Env.coqgen_open_module ~loc: phrase_loc modname env
  | Infer.PCM_coq_require fname ->
      Format.fprintf out_fmter "@[<2>Require@ %s@].@\n" fname ;
      env
  | Infer.PCM_species (species_def, species_descr, dep_graph) ->
      let spe_binding_info =
        Species_coq_generation.species_compile
          ~current_unit env out_fmter species_def species_descr dep_graph in
      (* Return the coq code generation environment extended by the current
         species's information. *)
      Env.CoqGenEnv.add_species
        ~loc: species_def.Parsetree.ast_loc
        species_def.Parsetree.ast_desc.Parsetree.sd_name
        spe_binding_info env
  | Infer.PCM_collection (collection_def, collection_descr, dep_graph) ->
      (* Collections don't have parameters or any remaining abstraction.
         Moreover, since we can never inherit of a collection, just forget all
         methods information, it will never be used.
         Collections do not have collection generator, then simply add them in
         the environment with None.
         Finally, collections do not have any parameters, so empty list! *)
      Species_coq_generation.collection_compile
        env ~current_unit out_fmter collection_def collection_descr dep_graph ;
      Env.CoqGenEnv.add_species
        ~loc: collection_def.Parsetree.ast_loc
        collection_def.Parsetree.ast_desc.Parsetree.cd_name
        ([], [], None, Env.COS_collection) env
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
  | Infer.PCM_let_def (let_def, _) ->
      (* Create the initial context for compiling the let definition.
         We would not need a "full" context, a "reduced" one would be
         sufficient, but via [let_binding_compile], the function
         [toplevel_let_def_compile] needs a "full". So... *)
      let ctx = {
        Context.scc_current_unit = current_unit ;
        (* Dummy, since not under a species. *)
        Context.scc_current_species = ("(**)", (Parsetree.Vuident "(**)")) ;
        (* Not under a species, hence no species parameter. *)
        Context.scc_species_parameters_names = [] ;
        (* Not under a species, hence empty carriers mapping. *)
        Context.scc_collections_carrier_mapping = [] ;
        (* Not in the context of generating a method's body code, then empty. *)
        Context.scc_lambda_lift_params_mapping = [] ;
        (* Empty, since not under a species. *)
        Context.scc_dependency_graph_nodes = [] ;
        Context.scc_out_fmter = out_fmter } in
      let env' = toplevel_let_def_compile ctx env let_def in
      Format.fprintf out_fmter ".@\n@\n" ;
      env'
  | Infer.PCM_theorem (theorem_def, _) ->
      let ctx = {
        Context.scc_current_unit = current_unit ;
        (* Dummy, since not under a species. *)
        Context.scc_current_species = ("(**)", (Parsetree.Vuident "(**)")) ;
        (* Not under a species, hence no species parameter. *)
        Context.scc_species_parameters_names = [] ;
        (* Not under a species, hence empty carriers mapping. *)
        Context.scc_collections_carrier_mapping = [] ;
        (* Not in the context of generating a method's body code, then empty. *)
        Context.scc_lambda_lift_params_mapping = [] ;
        (* Empty, since not under a species. *)
        Context.scc_dependency_graph_nodes = [] ;
        Context.scc_out_fmter = out_fmter } in
      let _ =
        Species_coq_generation.toplevel_theorem_compile ctx env theorem_def in
      (* Be careful, the ending . is generated by the proof code. *)
      Format.fprintf out_fmter "@\n@\n" ;
      env  (* [Unsure] *)
  | Infer.PCM_expr expr ->
      (* We compile toplevel expressions as "Check" orders under Coq. *)
      Format.fprintf out_fmter "@[<1>Check@ (" ;
      let ctx = {
        Context.scc_current_unit = current_unit ;
        (* Dummy, since not under a species. *)
        Context.scc_current_species = ("(**)", (Parsetree.Vuident "(**)")) ;
        (* Not under a species, hence no species parameter. *)
        Context.scc_species_parameters_names = [] ;
        (* Not under a species, hence empty carriers mapping. *)
        Context.scc_collections_carrier_mapping = [] ;
        (* Not in the context of generating a method's body code, then empty. *)
        Context.scc_lambda_lift_params_mapping = [] ;
        (* Empty, since not under a species. *)
        Context.scc_dependency_graph_nodes = [] ;
        Context.scc_out_fmter = out_fmter } in
      Species_record_type_generation.generate_expr
        ctx ~local_idents: []
        ~self_methods_status: Species_record_type_generation.SMS_from_record
        env expr ;
      Format.fprintf out_fmter ").@]@\n@\n" ;
      env
 ;;



let root_compile ~current_unit ~out_file_name stuff =
  if Configuration.get_verbose () then
    Format.eprintf "Starting Coq code generation.@." ;
  let out_hd = open_out_bin out_file_name in
  let out_fmter = Format.formatter_of_out_channel out_hd in
  let global_env = ref (Env.CoqGenEnv.empty ()) in
  (* Always import Coq booleans and integers and floats.
     Alias int notation to Z. *)
  Format.fprintf out_fmter
    "Require Export Bool.@\n\
     Require Export ZArith.@\n\
     Open Scope Z_scope.@\n\
     Require Export Reals.@\n\
     Require Export Ascii.@\n\
     Require Export String.@\n\
     Require Export List.@\n\
     Require Export Recdef.@\n\
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
      (* And rename it to prevent an incorrecty Coq source file from
         remaining, but to still keep a trace of what could be generated until
         the error arose. *)
      let trace_filename = out_file_name ^ ".mangled" in
      (* If the file of trace already exists, then first discard it to prevent
         OS file I/O errors. *)
      if Sys.file_exists trace_filename then Sys.remove trace_filename ;
      Sys.rename out_file_name trace_filename ;
    with second_error ->
      (begin
      (* Here we want to catch errors that can arise during the trace file
         stuff. Because we don't want these errors to hide the real initial
         problem that made the code generation impossible, we first process
         here I/O errors, then will be raise again the initial error. *)
      Format.eprintf
        "Error@ while@ trying@ to@ keep@ trace@ of@ the@ partially@ \
         generated@ Coq@ code:@ %s.@\nInitial@ error@ follows.@."
        (Printexc.to_string second_error)
      end)
    end) ;
    (* Re-reaise the initial error. *)
    raise whatever
;;
