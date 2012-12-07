(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            François Pessaux                                                *)
(*            Pierre Weis                                                     *)
(*            Damien Doligez                                                  *)
(*                                                                            *)
(*                               LIP6  --  INRIA Rocquencourt                 *)
(*                                                                            *)
(*  Copyright 2007 - 2012 LIP6 and INRIA                                      *)
(*            2012 ENSTA ParisTech                                            *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)

(* $Id: main_coq_generation.ml,v 1.48 2012-10-30 11:55:07 pessaux Exp $ *)

(* ************************************************************************** *)
(** {b Descr} : This module is the entry point for the compilation from FoCaL
    to Coq. It dispatches the compilation of each possible FoCaL entity to
    the dedicated compilation module.
    It also contains the seed of toplevel let definitions code generation.    *)
(* ************************************************************************** *)



(* ************************************************************************** *)
(** {b Descr}: Exception raised when a toplevel let-definition is tagged
    "logical".

    {b Rem}: Exported outside this module.                                    *)
(* ************************************************************************** *)
exception Logical_methods_only_inside_species of Location.t ;;



(* ************************************************************************** *)
(** {b Descr}: Generates code for a toplevel recursive or not function.
    Currently, toplevel recursive functions are always generated with
    "Fixpoint"                                                                *)
(* ************************************************************************** *)
let toplevel_let_def_compile ctx env let_def =
  if let_def.Parsetree.ast_desc.Parsetree.ld_logical = Parsetree.LF_logical then
    raise (Logical_methods_only_inside_species let_def.Parsetree.ast_loc) ;
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Currently, toplevel recursive functions are generated with "Fixpoint". *)
  let is_rec =
    (match let_def.Parsetree.ast_desc.Parsetree.ld_rec with
     | Parsetree.RF_no_rec -> false | Parsetree.RF_rec -> true) in
  let in_recursive_let_section_of =
    if is_rec then
      List.map
        (fun b -> b.Parsetree.ast_desc.Parsetree.b_name)
        let_def.Parsetree.ast_desc.Parsetree.ld_bindings
    else [] in
  Format.fprintf out_fmter "@[<2>" ;
  let opt_term_proof =
    let_def.Parsetree.ast_desc.Parsetree.ld_termination_proof in
  (* Recover pre-compilation info and extended environment in case of
     recursivity for all the bindings. *)
  let (env, pre_comp_infos) =
    Species_record_type_generation.pre_compute_let_bindings_infos_for_rec
      ~is_rec ~toplevel: true env
      let_def.Parsetree.ast_desc.Parsetree.ld_bindings in
  (* Now generate each bound definition. Remark that there is no local idents
     in the scope because we are at toplevel. In the same way, because we are
     not under the scope of a species, the way "Self" must be printed is
     non-relevant. We use [SMS_from_species] by default. *)
  let env' =
    (match (let_def.Parsetree.ast_desc.Parsetree.ld_bindings, pre_comp_infos)
    with
     | ([], _) ->
         (* The "let" construct should always at least bind one identifier ! *)
         assert false
     | ([one_bnd], [one_pre_comp_info]) ->
         let binder = if is_rec then "Fixpoint" else "Let" in
         Species_record_type_generation.let_binding_compile
           ctx ~binder ~opt_term_proof ~local_idents: []
           ~in_recursive_let_section_of
           (* Or whatever since "Self" does not exist anymore. *)
           ~self_methods_status: Species_record_type_generation.SMS_from_record
           ~recursive_methods_status: Species_record_type_generation.RMS_regular
           ~toplevel: true ~is_rec env one_bnd one_pre_comp_info
     | ((first_bnd :: next_bnds),
        (first_pre_comp_info :: next_pre_comp_infos)) ->
         let first_binder = if is_rec then "Fixpoint" else "Let" in
         let accu_env =
           ref
             (Species_record_type_generation.let_binding_compile
                ctx ~binder: first_binder ~opt_term_proof ~local_idents: []
                ~in_recursive_let_section_of
                (* Or whatever since "Self" does not exist anymore. *)
                ~self_methods_status:
                  Species_record_type_generation.SMS_from_record
                ~recursive_methods_status:
                  Species_record_type_generation.RMS_regular
                ~toplevel: true ~is_rec env first_bnd first_pre_comp_info) in
         List.iter2
           (fun binding pre_comp_info ->
             Format.fprintf out_fmter "@]@\n@[<2>" ;
             accu_env :=
               Species_record_type_generation.let_binding_compile
                 ctx ~binder: "with" ~opt_term_proof ~local_idents: []
                 ~in_recursive_let_section_of
                 (* Or whatever since "Self" does not exist anymore. *)
                 ~self_methods_status:
                   Species_record_type_generation.SMS_from_record
                 ~recursive_methods_status:
                   Species_record_type_generation.RMS_regular
                 ~toplevel: true ~is_rec !accu_env binding pre_comp_info)
           next_bnds next_pre_comp_infos ;
         !accu_env
     | (_, _) ->
         (* Case where we would not have the same number og pre-compiled infos
            and of bindings. Should never happen. *)
         assert false) in
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
  | Infer.PCM_annotation_title -> env
  | Infer.PCM_use (_, modname) ->
      (* One must let known that the module is required. *)
      Format.fprintf out_fmter "@[<2>Require@ %s@].@\n" modname ;
      env
  | Infer.PCM_open (phrase_loc, modname) ->
      (* One must "open" the coq code generation environment of this module
         and return the environment extended with these "opened" bindings.
         We must also generate a "Require" for this Coq module. *)
      Format.fprintf out_fmter "@[<2>Require@ %s@].@\n" modname ;
      Env.coqgen_open_module ~loc: phrase_loc modname env
  | Infer.PCM_coq_require fname ->
      Format.fprintf out_fmter "@[<2>Require@ %s@].@\n" fname ;
      env
  | Infer.PCM_species (species_def, species_descr, dep_graph) ->
      Types.purge_type_simple_to_coq_variable_mapping () ;
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
      Types.purge_type_simple_to_coq_variable_mapping () ;
      (* Collections don't have parameters or any remaining abstraction.
         Collections do not have collection generator, then simply add them in
         the environment with None.
         Finally, collections do not have any parameters, so empty list! *)
      let collection_methods =
        Species_coq_generation.collection_compile
          env ~current_unit out_fmter collection_def collection_descr
          dep_graph in
      Env.CoqGenEnv.add_species
        ~loc: collection_def.Parsetree.ast_loc
        collection_def.Parsetree.ast_desc.Parsetree.cd_name
        ([], collection_methods, None, Env.COS_collection) env
  | Infer.PCM_testing _ ->
      (* Testing definition are discarded at code generation. When
         testing is activated, testing instructions are compiled
         separatly. *)
      env
  | Infer.PCM_type (type_def_name, type_descr) ->
      Types.purge_type_simple_to_coq_variable_mapping () ;
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
      (* Since we are on the definition of the type, this type doesn't already
         exists normally. Hence, we want the code generation to enrich the
         environment with the components (record labels, sum value
         constructors) and the type the definition induces. *)
      Type_coq_generation.type_def_compile
        ~record_in_env: true ctx env type_def_name type_descr
  | Infer.PCM_let_def (let_def, _) ->
      Types.purge_type_simple_to_coq_variable_mapping () ;
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
  | Infer.PCM_theorem (theorem_def, found_type_variables) ->
      Types.purge_type_simple_to_coq_variable_mapping () ;
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
      Format.fprintf out_fmter "@\n@\n";
      (* Must now extend the value environment. Since the theorem is toplevel,
         it has to dependencies, so we ignore the returned value of
         [toplevel_theorem_compile].
         We need to bind the theorem's ident to a [VB_toplevel_property] and
         determine the number of polymorphic type variables it has. *)
      let num_vars = List.length found_type_variables in
      let env_binding =
        (num_vars,
         Env.CoqGenInformation.VB_toplevel_property
           theorem_def.Parsetree.ast_desc.Parsetree.th_stmt) in
      (* Return the extended environmenent. *)
      Env.CoqGenEnv.add_value
        ~toplevel: (Some theorem_def.Parsetree.ast_loc)
        theorem_def.Parsetree.ast_desc.Parsetree.th_name env_binding env
  | Infer.PCM_expr expr ->
      Types.purge_type_simple_to_coq_variable_mapping () ;
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
        ctx ~local_idents: [] ~in_recursive_let_section_of: []
        ~self_methods_status: Species_record_type_generation.SMS_from_record
        ~recursive_methods_status: Species_record_type_generation.RMS_regular
        env expr ;
      Format.fprintf out_fmter ").@]@\n@\n" ;
      (* Nothing to extend the environment. *)
      env
;;



let root_compile ~current_unit ~out_file_name stuff =
  if Configuration.get_verbose () then
    Format.eprintf "Starting Coq code generation.@.";
  let out_hd = open_out_bin out_file_name in
  let out_fmter = Format.formatter_of_out_channel out_hd in
  let global_env = ref (Env.CoqGenEnv.empty ()) in
  (* Since Coq 8.3pl2 bound entities do no more appear as terms sequenced as
     "imply" (i.e. ->) but are now directly introduce as hypothesis. This breaks
     the code generation model for Zenon proofs. If using Coq 8.3pl2, the we
     force Coq getting back to previous proof goals, unsetting the "Automatic
     Introduction" globally. Otherwise, if using Coq version < 8.3pl2, we do not
     unset. *)
 if not (Configuration.get_use_coq_older ()) then
     Format.fprintf out_fmter "Global Unset Automatic Introduction.@\n" ;
  (* Always import Coq booleans and integers and floats. Alias int notation to
     Z. *)
  Format.fprintf out_fmter
    "Require Export Bool.@\n\
     Require Export ZArith.@\n\
     Open Scope Z_scope.@\n\
     Require Export Reals.@\n\
     Require Export Ascii.@\n\
     Require Export String.@\n\
     Require Export List.@\n\
     Require Export Recdef.@\n\
     Require Export coq_builtins.@\n@\n";
  if Configuration.get_experimental () then
    Format.fprintf out_fmter
      "(* Below: to prevent Function to apply heuristics that would@\n\
          the expected aim in recursive functions termination proofs. *)@\n@\n\
       Set Function_raw_tcc.@\n@\n";
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
      (* And rename it to prevent an incorrecty Coq source file from
         remaining, but to still keep a trace of what could be generated until
         the error arose. *)
      let trace_filename = out_file_name ^ ".mangled" in
      (* If the file of trace already exists, then first discard it to prevent
         OS file I/O errors. *)
      if Sys.file_exists trace_filename then Sys.remove trace_filename;
      Sys.rename out_file_name trace_filename;
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
    end);
    (* Re-reaise the initial error. *)
    raise whatever
;;
