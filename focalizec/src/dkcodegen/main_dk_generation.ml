(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            Raphaël Cauderlier                                              *)
(*                                                                            *)
(*               LIP6  --  INRIA Rocquencourt -- ENSTA ParisTech              *)
(*                                                                            *)
(*  Copyright 2007 - ... LIP6 and INRIA                                       *)
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
  | Infer.PCM_species (species_def, species_descr, dep_graph, abstr_info) ->
      Dk_pprint.purge_type_simple_to_dk_variable_mapping () ;
      let spe_binding_info =
        Species_dk_generation.species_compile
          ~current_unit env out_fmter species_def species_descr dep_graph
      abstr_info in
      (* Return the Dedukti code generation environment extended by the current
         species's information. *)
      Env.DkGenEnv.add_species
        ~loc: species_def.Parsetree.ast_loc
        species_def.Parsetree.ast_desc.Parsetree.sd_name
        spe_binding_info env
  | Infer.PCM_collection (collection_def, collection_descr, dep_graph) ->
      Dk_pprint.purge_type_simple_to_dk_variable_mapping () ;
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
  | Infer.PCM_type ty_descrs ->
(* [Unsure][WIP] *)
let (type_def_name, type_descr) = List.hd ty_descrs in
      Dk_pprint.purge_type_simple_to_dk_variable_mapping () ;
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
      Dk_pprint.purge_type_simple_to_dk_variable_mapping () ;
      (* Create the initial context for compiling the let definition.
         We would not need a "full" context, a "reduced" one would be
         sufficient, but via [let_binding_compile], the function
         [toplevel_let_def_compile] needs a "full". So... *)
      let ctx = {
        Context.scc_current_unit = current_unit ;
        (* Dummy, since not under a species. *)
        Context.scc_current_species = ("(;;)", (Parsetree.Vuident "(;;)")) ;
        (* Not under a species, hence no species parameter. *)
        Context.scc_species_parameters_names = [] ;
        (* Not under a species, hence empty carriers mapping. *)
        Context.scc_collections_carrier_mapping = [] ;
        (* Not in the context of generating a method's body code, then empty. *)
        Context.scc_lambda_lift_params_mapping = [] ;
        (* Empty, since not under a species. *)
        Context.scc_dependency_graph_nodes = [] ;
        Context.scc_out_fmter = out_fmter } in
      let env' = Let_dk_generation.toplevel_let_def_compile ctx env let_def in
      Format.fprintf out_fmter ".@\n@\n" ;
      env'
  | Infer.PCM_theorem (theorem_def, _) ->
      Dk_pprint.purge_type_simple_to_dk_variable_mapping () ;
      let ctx = {
        Context.scc_current_unit = current_unit ;
        (* Dummy, since not under a species. *)
        Context.scc_current_species = ("(;;)", (Parsetree.Vuident "(;;)")) ;
        (* Not under a species, hence no species parameter. *)
        Context.scc_species_parameters_names = [] ;
        (* Not under a species, hence empty carriers mapping. *)
        Context.scc_collections_carrier_mapping = [] ;
        (* Not in the context of generating a method's body code, then empty. *)
        Context.scc_lambda_lift_params_mapping = [] ;
        (* Empty, since not under a species. *)
        Context.scc_dependency_graph_nodes = [] ;
        Context.scc_out_fmter = out_fmter } in
      Proof_dk_generation.toplevel_theorem_compile ctx env theorem_def;
      (* Be careful, the ending . is generated by the proof code. *)
      Format.fprintf out_fmter "@\n@\n";
      (* Must now extend the value environment. Since the theorem is toplevel,
         it has to dependencies, so we ignore the returned value of
         [toplevel_theorem_compile].
         We need to bind the theorem's ident to a [VB_toplevel_property] and
         determine the number of polymorphic type variables it has. *)
      let env_binding =
        Env.DkGenInformation.VB_toplevel_property
          theorem_def.Parsetree.ast_desc.Parsetree.th_stmt
      in
      (* Return the extended environmenent. *)
      Env.DkGenEnv.add_value
        ~toplevel: (Some theorem_def.Parsetree.ast_loc)
        theorem_def.Parsetree.ast_desc.Parsetree.th_name env_binding env
  | Infer.PCM_expr expr ->
      Dk_pprint.purge_type_simple_to_dk_variable_mapping () ;
      (* We compile toplevel expressions as "Strong normal form" orders under
         Dedukti. *)
      Format.fprintf out_fmter "@[<1>#SNF@ (" ;
      let ctx = {
        Context.scc_current_unit = current_unit ;
        (* Dummy, since not under a species. *)
        Context.scc_current_species = ("(;;)", (Parsetree.Vuident "(;;)")) ;
        (* Not under a species, hence no species parameter. *)
        Context.scc_species_parameters_names = [] ;
        (* Not under a species, hence empty carriers mapping. *)
        Context.scc_collections_carrier_mapping = [] ;
        (* Not in the context of generating a method's body code, then empty. *)
        Context.scc_lambda_lift_params_mapping = [] ;
        (* Empty, since not under a species. *)
        Context.scc_dependency_graph_nodes = [] ;
        Context.scc_out_fmter = out_fmter } in
      Expr_dk_generation.generate_expr
        ctx ~local_idents: [] ~in_recursive_let_section_of: []
        ~self_methods_status: Expr_dk_generation.SMS_from_record
        ~recursive_methods_status: Expr_dk_generation.RMS_regular
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
