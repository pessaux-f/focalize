(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            François Pessaux                                                *)
(*            Pierre Weis                                                     *)
(*            Damien Doligez                                                  *)
(*                                                                            *)
(*               LIP6  --  INRIA Rocquencourt -- ENSTA ParisTech              *)
(*                                                                            *)
(*  Copyright 2007 - ...  LIP6 and INRIA                                      *)
(*            2012 - ... ENSTA ParisTech                                      *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)


(* ************************************************************************** *)
(** {b Descr} : This module is the entry point for the compilation from FoCaL
    to Dedukti. It dispatches the compilation of each possible FoCaL entity to
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
  let rec_status =
    (match let_def.Parsetree.ast_desc.Parsetree.ld_rec with
     | Parsetree.RF_no_rec -> Env.DkGenInformation.RC_non_rec
     | Parsetree.RF_rec -> (
         match let_def.Parsetree.ast_desc.Parsetree.ld_termination_proof with
         | None -> Env.DkGenInformation.RC_rec Env.DkGenInformation.RPK_other
         | Some term_pr -> (
             match term_pr.Parsetree.ast_desc with
             | Parsetree.TP_structural decr_arg ->
                 Env.DkGenInformation.RC_rec
                   (Env.DkGenInformation.RPK_struct decr_arg)
             | _ ->
                 Env.DkGenInformation.RC_rec Env.DkGenInformation.RPK_other))
    ) in
  let in_recursive_let_section_of =
    if rec_status <> Env.DkGenInformation.RC_non_rec then  (* Is rec. *)
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
    Species_record_type_dk_generation.pre_compute_let_bindings_infos_for_rec
      ~rec_status ~toplevel: true env
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
         let binder = "" in
         Species_record_type_dk_generation.let_binding_compile
           ctx ~binder ~opt_term_proof ~local_idents: []
           ~in_recursive_let_section_of
           (* Or whatever since "Self" does not exist anymore. *)
           ~self_methods_status: Species_record_type_dk_generation.SMS_from_record
           ~recursive_methods_status: Species_record_type_dk_generation.RMS_regular
           ~toplevel: true ~rec_status env one_bnd one_pre_comp_info
     | ((first_bnd :: next_bnds),
        (first_pre_comp_info :: next_pre_comp_infos)) ->
         let first_binder =
           if rec_status <> Env.DkGenInformation.RC_non_rec then "Fixpoint"
           else "Let" in
         let accu_env =
           ref
             (Species_record_type_dk_generation.let_binding_compile
                ctx ~binder: first_binder ~opt_term_proof ~local_idents: []
                ~in_recursive_let_section_of
                (* Or whatever since "Self" does not exist anymore. *)
                ~self_methods_status:
                  Species_record_type_dk_generation.SMS_from_record
                ~recursive_methods_status:
                  Species_record_type_dk_generation.RMS_regular
                ~toplevel: true ~rec_status env first_bnd
                first_pre_comp_info) in
         List.iter2
           (fun binding pre_comp_info ->
             Format.fprintf out_fmter "@]@\n@[<2>" ;
             accu_env :=
               Species_record_type_dk_generation.let_binding_compile
                 ctx ~binder: "with" ~opt_term_proof ~local_idents: []
                 ~in_recursive_let_section_of
                 (* Or whatever since "Self" does not exist anymore. *)
                 ~self_methods_status:
                   Species_record_type_dk_generation.SMS_from_record
                 ~recursive_methods_status:
                   Species_record_type_dk_generation.RMS_regular
                 ~toplevel: true ~rec_status !accu_env binding pre_comp_info)
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
(** {b Descr} : Dispatch the Dedukti code generation of a toplevel structure
    to the various more specialized code generation routines.

    {b Arg} :
      - [current_unit] : The name of the current compilation unit (i.e.
        the name of the file without extension and not capitalized).
      - [out_fmter] : The out channel where to generate the Dedukti source
        code.
      - unnamed : The structure for which the Dedukti source code has to be
        generated.

    {b Rem} : Not exported outside this module.                          *)
(* ********************************************************************* *)
let toplevel_compile env ~current_unit out_fmter = function
  | Infer.PCM_annotation_title -> env
  | Infer.PCM_use _ ->
      (* Nothing to do for Dedukti. *)
      env
  | Infer.PCM_open (phrase_loc, modname) ->
      (* One must "open" the dedukti code generation environment of this module
         and return the environment extended with these "opened" bindings. *)
      Env.dkgen_open_module ~loc: phrase_loc modname env
  | Infer.PCM_coq_require _ ->
      env
  | Infer.PCM_species (species_def, species_descr, dep_graph) ->
      Types.purge_type_simple_to_dk_variable_mapping () ;
      let spe_binding_info =
        Species_dk_generation.species_compile
          ~current_unit env out_fmter species_def species_descr dep_graph in
      (* Return the Dedukti code generation environment extended by the current
         species's information. *)
      Env.DkGenEnv.add_species
        ~loc: species_def.Parsetree.ast_loc
        species_def.Parsetree.ast_desc.Parsetree.sd_name
        spe_binding_info env
  | Infer.PCM_collection (collection_def, collection_descr, dep_graph) ->
      Types.purge_type_simple_to_dk_variable_mapping () ;
      (* Collections don't have parameters or any remaining abstraction.
         Collections do not have collection generator, then simply add them in
         the environment with None.
         Finally, collections do not have any parameters, so empty list! *)
      let collection_methods =
        Species_dk_generation.collection_compile
          env ~current_unit out_fmter collection_def collection_descr
          dep_graph in
      Env.DkGenEnv.add_species
        ~loc: collection_def.Parsetree.ast_loc
        collection_def.Parsetree.ast_desc.Parsetree.cd_name
        ([], collection_methods, None, Env.COS_collection) env
  | Infer.PCM_testing _ ->
      (* Testing definition are discarded at code generation. When
         testing is activated, testing instructions are compiled
         separatly. *)
      env
  | Infer.PCM_type (type_def_name, type_descr) ->
      Types.purge_type_simple_to_dk_variable_mapping () ;
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
         constructors) and the type the definition induces. In effect, this is
         not a "fake" type definition provided to Zenon as a fact. *)
      Type_dk_generation.type_def_compile
        ~as_zenon_fact: false ctx env type_def_name type_descr
  | Infer.PCM_let_def (let_def, _) ->
      Types.purge_type_simple_to_dk_variable_mapping () ;
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
      Types.purge_type_simple_to_dk_variable_mapping () ;
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
        Species_dk_generation.toplevel_theorem_compile ctx env theorem_def in
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
         Env.DkGenInformation.VB_toplevel_property
           theorem_def.Parsetree.ast_desc.Parsetree.th_stmt) in
      (* Return the extended environmenent. *)
      Env.DkGenEnv.add_value
        ~toplevel: (Some theorem_def.Parsetree.ast_loc)
        theorem_def.Parsetree.ast_desc.Parsetree.th_name env_binding env
  | Infer.PCM_expr expr ->
      Types.purge_type_simple_to_dk_variable_mapping () ;
      (* We compile toplevel expressions as "Strong normal form" orders under
         Dedukti. *)
      Format.fprintf out_fmter "@[<1>#SNF@ (" ;
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
      Species_record_type_dk_generation.generate_expr
        ctx ~local_idents: [] ~in_recursive_let_section_of: []
        ~self_methods_status: Species_record_type_dk_generation.SMS_from_record
        ~recursive_methods_status: Species_record_type_dk_generation.RMS_regular
        env expr ;
      Format.fprintf out_fmter ").@]@\n@\n" ;
      (* Nothing to extend the environment. *)
      env
;;



let root_compile ~current_unit ~out_file_name stuff =
  if Configuration.get_verbose () then
    Format.eprintf "Starting Dedukti code generation.@.";
  let out_hd = open_out_bin out_file_name in
  let out_fmter = Format.formatter_of_out_channel out_hd in
  let global_env = ref (Env.DkGenEnv.empty ()) in
  Format.fprintf out_fmter "#NAME %s.@\n" current_unit;
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
      (* And rename it to prevent an incorrecty Dedukti source file from
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
         generated@ Dedukti@ code:@ %s.@\nInitial@ error@ follows.@."
        (Printexc.to_string second_error)
      end)
    end);
    (* Re-reaise the initial error. *)
    raise whatever
;;
