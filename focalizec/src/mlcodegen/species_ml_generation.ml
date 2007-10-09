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

(* $Id: species_ml_generation.ml,v 1.1 2007-10-09 08:37:35 pessaux Exp $ *)


(* ********************************************************************* *)
(** {b Descr} : Data structure to record the various stuff needed to
          generate the OCaml code for a species definition. Passing this
          structure prevents from recursively passing a bunch of
          parameters to the functions. Instead, one pass only one and
          functions use the fields they need. This is mostly to preserve
          the stack and to make the code more readable. In fact,
          information recorded in this structure is semantically pretty
          un-interesting to understand the compilation process: it is
           more utilities.

    {b Rem} Not exported outside this module.                            *)
(* ********************************************************************* *)
type species_compil_context = {
  (** The name of the currently analysed compilation unit. *)
  scc_current_unit : Types.fname ;
  (** The name of the current species. *)
  scc_current_species : Parsetree.qualified_vname ;
  (** The nodes of the current species's dependency graph. *)
  scc_dependency_graph_nodes : Dep_analysis.name_node list ;
  (** The current correspondance between collection types and type variable
      names representing the carrier of a species type in the OCaml code. *)
  scc_collections_carrier_mapping : (Types.type_collection * string) list ;
  (** The current output formatter where to send the generated code. *)
  scc_out_fmter : Format.formatter
} ;;




(* *********************************************************************** *)
(* current_unit: Types.fname -> Env.TypeInformation.species_description -> *)
(*   (Types.type_collection * string) list                                 *)
(** {b Descr} : Create the correspondance between the collection type of
              the species definition parameters and the type variables
              names to be used later during the OCaml translation.
              For a species parameter [A is/in ... ], the type variable
              that will be used is "'" + lowercased name of the species
              parameter + "_as_carrier".

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let build_collections_carrier_mapping ~current_unit species_descr =
    List.map
      (function
	| Env.TypeInformation.SPAR_is (n, _) ->
	    let n_as_string = Parsetree_utils.name_of_vname n in
	    (* Build the name of the type variable that will represent *)
	    (* this parameter's carrier type seen from OCaml. Just     *)
            (* lowerize the parameter name because collection names    *)
            (* are always syntactically capitalized and OCaml type     *)
            (* variable are always syntactically lowercase.            *)
	    let carrier_type_variable_name =
	      "'" ^ (String.uncapitalize n_as_string) ^ "_as_carrier" in
	    (* Now, build the "collection type" this name will be bound to. *)
	    (* According to how the "collection type" of parameters are     *)
	    (* built, this will be the couple of the current compilation    *)
            (* unit and the name of the parameter.                          *)
	    let type_coll = (current_unit, n_as_string) in
	    (* And now create the binding... *)
	    (type_coll, carrier_type_variable_name)
	| Env.TypeInformation.SPAR_in (n, type_coll) ->
	    (* Build the name of the type variable that will represent *)
	    (* this parameter's carrier type seen from OCaml. Same     *)
            (* remark than above for lowercase/uppercase.              *)
	    let carrier_type_variable_name =
	      "' " ^ (String.uncapitalize (Parsetree_utils.name_of_vname n)) ^
	      "_as_carrier" in
	    (type_coll, carrier_type_variable_name))
      species_descr.Env.TypeInformation.spe_sig_params
;;



(* ******************************************************************** *)
(* ('a * string) list -> unit                                           *)
(** {b Descr} : Helper to print the list of known variables names in
              a collections carrier mapping as a legal OCaml list of
              type parameters, i.e, comma separated except for the last
              one.

    {b Args} :
      - [out_fmter] : The out channel where to generate the OCaml
          source code.
      - [vars_list] : The list of couples whose second component is the
          variable name. (We are not interested in the first component
          because we only want to print tha variables names).

    {b Rem} : Not exported outside this module.                         *)
(* ******************************************************************** *)
let print_comma_separated_vars_list_from_mapping out_fmter vars_list =
  let rec rec_print = function
    | [] -> ()
    | [(_, last_name)] -> Format.fprintf out_fmter "%s" last_name
    | (_, h) :: q ->
	Format.fprintf out_fmter "%s,@ " h ;
	rec_print q in
  rec_print vars_list
;;



(* ************************************************************************* *)
(* species_compil_context -> Env.TypeInformation.species_field list -> unit  *)
(** {b Descr} : Checks if "rep" is defined. If so, then generate the type
              constraint reflecting its effective structure. We loosely
              iterate on the list of fields, stopping at the first occurence
              of "rep" because, by construction if "rep" is present then it
              is only once. Hence it is safe to ignore the remaining fields.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let generate_rep_constraint_in_record_type ctx fields =
  let rec rec_search = function
    | [] -> ()
    | h :: q ->
	(begin
	match h with
	 | Env.TypeInformation.SF_sig (_, n, sch) ->
	     (* Check if the sig is "rep". *)
	     if (Parsetree_utils.name_of_vname n) = "rep" then
	       (begin
	       Format.fprintf ctx.scc_out_fmter
		 "(* Carrier's structure explicitly given by \"rep\". *)@\n" ;
	       Format.fprintf ctx.scc_out_fmter "@[<2>type " ;
	       (* First, output the type parameters if some, and enclose *)
	       (* them by parentheses if there are several.              *)
	       (match ctx.scc_collections_carrier_mapping with
		| [] -> ()
		| [ (_, only_var_name) ] ->
		    Format.fprintf ctx.scc_out_fmter "%s@ " only_var_name
		| _ ->
		    (* More than one, then suround by parentheses. *)
		    Format.fprintf ctx.scc_out_fmter "@[<1>(" ;
		    (* Print the variables names... *)
		    print_comma_separated_vars_list_from_mapping
		      ctx.scc_out_fmter ctx.scc_collections_carrier_mapping ;
		    Format.fprintf ctx.scc_out_fmter ")@]@ ") ;
	       (* Now, output the type's name and body. *)
	       let ty = Types.specialize sch in
	       Format.fprintf ctx.scc_out_fmter
		 "me_as_carrier =@ %a@]@\n"
		 (Types.pp_type_simple_to_ml
		    ~current_unit: ctx.scc_current_unit
		    ~reuse_mapping: false
		    ctx.scc_collections_carrier_mapping) ty
	       end)
	     else rec_search q
	 | _ -> rec_search q
	end) in
  rec_search fields
;;



(* ************************************************************************ *)
(* species_compil_context -> Parsetree.vname ->                             *)
(*   Env.TypeInformation.species_description -> unit                        *)
(** {b Descr} : Generate the record type representing a species. This type
          contains a field per method. This type is named "me_as_species"
          to reflect the point that it represents the ML structure
          representing the FoCaL species.
          Depending on whether the species has parameters, this record
          type also has parameters. In any case, it at least has a
          parameter representing "self as it will be once instanciated"
          once "we" (i.e. the species) will be really living as a
          collection.

    {b Rem} : Not exported outside this module.                             *)
(* ************************************************************************ *)
let generate_record_type ctx species_name species_descr =
  let out_fmter = ctx.scc_out_fmter in
  let collections_carrier_mapping = ctx.scc_collections_carrier_mapping in
  (* First, check if "rep" is defined. If so, then generate  *)
  (* the type constraint reflecting its effective structure. *)
  generate_rep_constraint_in_record_type
    ctx species_descr.Env.TypeInformation.spe_sig_methods ;
  let field_prefix =
    String.uncapitalize (Parsetree_utils.name_of_vname species_name) in
  (* The header of the OCaml type definition for the species record. *)
  Format.fprintf out_fmter "@[<2>type " ;
  (* Process parameters and "self" type variables names. *)
  if collections_carrier_mapping = [] then
    Format.fprintf out_fmter "'me_as_carrier "
  else
    (begin
    (* If there are several parameters, then enclose them by parentheses. *)
    Format.fprintf out_fmter "(@[<1>" ;
    List.iter
      (fun (_, type_variable_name) ->
	Format.fprintf out_fmter "%s,@ " type_variable_name)
      collections_carrier_mapping ;
    Format.fprintf out_fmter "'me_as_carrier)@] "
    end) ;
  (* The name of the type. *)
  Format.fprintf out_fmter "me_as_species = {@\n" ;
  (* The record's fields types. *)
  List.iter
    (function
      | Env.TypeInformation.SF_sig (from, n, sch)
      | Env.TypeInformation.SF_let (from, n, _, sch, _) ->
	  (begin
	  (* Skip "rep", because it is a bit different and processed above *)
	  (* c.f. function [generate_rep_constraint_in_record_type].       *)
	  if (Parsetree_utils.name_of_vname n) <> "rep" then
	    (begin
	    let ty = Types.specialize sch in
            Format.fprintf out_fmter "(* From species %a. *)@\n"
	      Sourcify.pp_qualified_vname from ;
	    (* Since we are printing a whole type scheme, it is stand-alone *)
            (* and we don't need to keep name sharing with anythin else.    *)
	    Format.fprintf out_fmter "@[<2>%s_%a : %a ;@]@\n"
	      field_prefix Misc_ml_generation.pp_to_ocaml_vname n
	      (Types.pp_type_simple_to_ml
		 ~current_unit: ctx.scc_current_unit
		 ~reuse_mapping: false collections_carrier_mapping) ty
	    end)
	  end)
      | Env.TypeInformation.SF_let_rec l ->
	  List.iter
	    (fun (from, n, _, sch, _) ->
	      let ty = Types.specialize sch in
	      Format.fprintf out_fmter "(* From species %a. *)@\n"
		Sourcify.pp_qualified_vname from ;
	      (* Since we are printing a whole type scheme, it is stand-alone *)
              (* and we don't need to keep name sharing with anythin else.    *)
	      Format.fprintf out_fmter "%s_%a : %a ;@\n"
		field_prefix Misc_ml_generation.pp_to_ocaml_vname n
		(Types.pp_type_simple_to_ml
		   ~current_unit: ctx.scc_current_unit
		   ~reuse_mapping: false collections_carrier_mapping) ty)
	    l
      | Env.TypeInformation.SF_theorem  (_, _, _, _, _)
      | Env.TypeInformation.SF_property (_, _, _, _) ->
	  (* Properties and theorems are purely  *)
          (* discarded in the Ocaml translation. *)
	  ())
    species_descr.Env.TypeInformation.spe_sig_methods ;
  Format.fprintf out_fmter "@]}@\n"
;;



(* ************************************************************************** *)
(** {b Descr} : Lower-level species field (relevant for collection generator)
        description recording information about dependency and extra
        parameters already computed while generating the methods and that
        will be re-used while generating the collection generator.
        This avoids computing several the same things and ensure that the
        information is formated in the same way everywhere (in other words
        that the extra parameters discovered will appear in the same order
        between method declaration and method application).
    {b Rem} : Not exported outside this module.                               *)
(* ************************************************************************** *)
type compiled_field_memory ={
  (** Where the method comes from (the most recent in inheritance). *)
  cfm_from_species : Parsetree.qualified_vname ;
  (** The method's name. *)
  cfm_method_name : Parsetree.vname ;
  (** The method's body. *)
  cfm_method_body  : Parsetree.expr ;
  (** The list mapping for each species parameter, the methods the current
      method depends on. By lambda-lifting, these methods induce extra
      parameters named as "_p_" +  species parameter name + "_" + called
      method's name we depend on. The first component of each couple is the
      parameter's name and the second is the set of methods the current
      method depends on from this species parameter.*)
  cfm_dependencies_from_parameters :
    (Parsetree.vname * Dep_analysis.VnameSet.t) list ;
  (** The methods of our inheritance tree the method depends on. *)
  cfm_decl_children :
    (Dep_analysis.name_node * Dep_analysis.dependency_kind) list ;
} ;;


(* ************************************************************************ *)
(** {b Descr} : Type of the fields significant for the collection generator
       creation, once the methods of the species have been generated. It
       remove all the fields that are not relevant for the collection
       generator.

    {b Rem} : Not exported outside this module.                             *)
(* ************************************************************************ *)
type compiled_species_fields =
  | CSF_let of compiled_field_memory
  | CSF_let_rec of compiled_field_memory list
;;


(* *********************************************************************** *)
(* species_compil_context -> Parsetree.vname list ->                       *)
(*   Env.TypeInformation.species_field -> unit                             *)
(** {b Desc} : Generates the OCaml code for ONE method field (i.e. for one
             let-bound construct or for one item of let-rec-bound items.

    {b Args} :
      - [ctx] : The species-compilation-context merging the various
          stuffs sometimes needed during the compilation pass.
      - [species_parameters_names] : The list of the vnames of parameters
          the currently compiled species as.
      - [field] : The species's field to compile (i.e. a "let", "let rec",
                "sig", "theorem" or "property").

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let generate_methods ctx species_parameters_names field =
  let out_fmter = ctx.scc_out_fmter in
  let collections_carrier_mapping = ctx.scc_collections_carrier_mapping in
  (* Local function to handle one binding. Will be directly used in case *)
  (* of [SF_let] field, or be iterated in case of [SF_let_rec] field.    *)
  (* The [~is_first] boolean tells whether we must start de function     *)
  (* binding with "let" or "and".                                        *)
  let generate_one_binding ~is_first (from, name, params, scheme, body) =
    (* Get all the methods we directly decl-depend on. They will   *)
    (* lead each to an extra parameter of the final OCaml function *)
    (* (lambda-lifing).                                            *)
    let decl_children =
      (try
	let my_node =
	  List.find
	    (fun { Dep_analysis.nn_name = n } -> n = name)
	    ctx.scc_dependency_graph_nodes in
	(* Only keep "decl-dependencies" . *)
	List.filter
	  (function
	    | (_, Dep_analysis.DK_decl) -> true
	    | (_, Dep_analysis.DK_def) -> false)
	  my_node.Dep_analysis.nn_children
      with Not_found -> []  (* No children at all. *)) in
    (* Get the list of the methods from the species parameters the current *)
    (* method depends on. Do not [fold_left] to keep the extra parameters  *)
    (* in the same order than the species parameters order. I.e. for a    *)
    (* species [Foo (A ..., B) ...] we want to have the extra parameters  *)
    (* due to lambda-lifting in the OCaml function ordered such as those  *)
    (* coming from [A] are first, then come those from [B].               *)
    let dependencies_from_params =
      List.fold_right
	(fun species_param_name accu ->
	  let meths_from_param =
	    Param_dep_analysis.param_deps_expr
	      ~current_species: ctx.scc_current_species
	      species_param_name body in
	  (* Return a couple binding the species parameter's name with the *)
	  (* methods of it we found as required for the current method.    *)
	  (species_param_name, meths_from_param) :: accu)
	species_parameters_names
	[] in
    (* First of all, only methods defined in the current species must *)
    (* be generated. Inherited methods ARE NOT generated again !      *)
    if from = ctx.scc_current_species then
      (begin
      (* Just a bit of debug. *)
      if Configuration.get_verbose () then
	Format.eprintf "Generating OCaml code for field '%a'.@."
	  Misc_ml_generation.pp_to_ocaml_vname name ;
      (* Start the OCaml function definition. *)
      if is_first then
	Format.fprintf out_fmter "@[<2>let %a"
	  Misc_ml_generation.pp_to_ocaml_vname name
      else
	Format.fprintf out_fmter "@[<2>and %a"
	  Misc_ml_generation.pp_to_ocaml_vname name ;
      (* First, abstract according to the species's parameters the current  *)
      (* method depends on.                                                 *)
      List.iter
	(fun (species_param_name, meths) ->
	  (* Each abstracted method will be named like "_p_", followed by *)
	  (* the species parameter name, followed by "_", followed by the *)
          (* method's name.                                               *)
	  let prefix =
	    "_p_" ^
	    (String.uncapitalize
	       (Parsetree_utils.name_of_vname species_param_name)) ^
	    "_" in
	  Dep_analysis.VnameSet.iter
	    (fun meth ->
	      Format.fprintf out_fmter "@ %s%a"
		prefix Misc_ml_generation.pp_to_ocaml_vname meth)
	    meths)
	dependencies_from_params ;
      (* Now, lambda-lift all the dependencies from our inheritance tree *)
      (* (i.e methods we depend on) that are only declared.              *)
      List.iter
	(fun ({ Dep_analysis.nn_name = dep_name }, _) ->
	  Format.fprintf out_fmter "@ abst_%a"
	    Misc_ml_generation.pp_to_ocaml_vname dep_name)
	decl_children ;
      (* Add the parameters of the let-binding with their type. *)
      let params_with_type =
	Misc_ml_generation.bind_parameters_to_types_from_type_scheme
	  scheme params in
      (* We are printing each parameter's type. These types in fact belong *)
      (* to a same type scheme. Hence, they may share variables together.  *)
      (* For this reason, we first purge the printing variable mapping and *)
      (* after, activate its persistence between each parameter printing.  *)
      Types.purge_type_simple_to_ml_variable_mapping () ;
      List.iter
	(fun (param_vname, opt_param_ty) ->
	  match opt_param_ty with
	   | Some param_ty ->
	       Format.fprintf out_fmter "@ (%a : %a)"
		 Misc_ml_generation.pp_to_ocaml_vname param_vname
		 (Types.pp_type_simple_to_ml
		    ~current_unit: ctx.scc_current_unit
		    ~reuse_mapping: true collections_carrier_mapping) param_ty
	   | None ->
	       Format.fprintf out_fmter "@ %a"
		 Misc_ml_generation.pp_to_ocaml_vname param_vname)
	params_with_type ;
      (* Now we don't need anymore the sharing. Hence, clean it. This should *)
      (* not be useful because the other guys usign printing should manage   *)
      (* this themselves (as we did just above by cleaning before activating *)
      (* the sharing), but anyway, it is safer an not costly. So...          *)
      Types.purge_type_simple_to_ml_variable_mapping () ;
      (* The "=" sign ending the OCaml function's "header". With a *)
      (* NON-breakable space to prevent uggly hyphenation !        *)
      Format.fprintf out_fmter " =@ " ;
      (* Generates the body's code of the method. *)
      let expr_ctx = {
	Misc_ml_generation.rcc_current_unit = ctx.scc_current_unit ;
	Misc_ml_generation.rcc_out_fmter = out_fmter } in
      Base_exprs_ml_generation.generate_expr expr_ctx body ;
      (* Done... Then, final carriage return. *)
      Format.fprintf out_fmter "@]@\n"
      end)
    else
      (begin
      (* Just a bit of debug/information if requested. *)
      if Configuration.get_verbose () then
	Format.eprintf
          "Field '%a' inherited but not (re)-declared is not generated again.@."
	  Misc_ml_generation.pp_to_ocaml_vname name
      end) ;
    (* Now, build the [compiled_field_memory], even if the method  *)
    (* was not really generated because it was inherited.          *)
    { cfm_from_species = from ;
      cfm_method_name = name ;
      cfm_method_body = body ;
      cfm_dependencies_from_parameters = dependencies_from_params ;
      cfm_decl_children = decl_children } in

  (* ****************************** *)
  (* Now, really process the field. *)
  match field with
   | Env.TypeInformation.SF_sig (_, name, _) ->
       (* Only declared, hence, no code to generate yet ! *)
       if Configuration.get_verbose () then
	 Format.eprintf "OCaml code for signature '%a' leads to void code.@."
	   Misc_ml_generation.pp_to_ocaml_vname name ;
       (* Nothing to keep for the collection generator. *)
       None
   | Env.TypeInformation.SF_let (from, name, params, scheme, body) ->
       Some
	 (CSF_let
	    (generate_one_binding
	       ~is_first: true (from, name, params, (Some scheme), body)))
   | Env.TypeInformation.SF_let_rec l ->
       (begin
       match l with
	| [] ->
	    (* A "let", then a fortiori "let rec" construct *)
	    (* must at least bind one identifier !          *)
	    assert false
	| (from, name, params, scheme, body) :: q ->
	    let first_compiled =
	      generate_one_binding
		~is_first: true (from, name, params, (Some scheme), body) in
	    let rem_compiled =
	      List.map
		(fun (from, name, params, scheme, body) ->
		  generate_one_binding
		    ~is_first: false
		    (from, name, params, (Some scheme), body))
		q in
	    Some (CSF_let_rec (first_compiled :: rem_compiled))
       end)
   | Env.TypeInformation.SF_theorem (_, name, _, _, _)
   | Env.TypeInformation.SF_property (_, name, _, _) ->
       (* Properties and theorems are purely  *)
       (* discarded in the Ocaml translation. *)
       if Configuration.get_verbose () then
	 Format.eprintf
	   "OCaml code for theorem/property '%a' leads to void code.@."
	   Misc_ml_generation.pp_to_ocaml_vname name ;
       (* Nothing to keep for the collection generator. *)
       None
;;



(* *********************************************************************** *)
(** {b Descr} : Dumps as OCaml code the parameters required to the
         collection generator in order to make them bound in the
         collection generator's body. These parameters come from
         the methods of the species parameters that some of our methods
         depend on. This means that a closed species with no species
         parameters will have NO extra parameters in its collection
         generator.
         This function must UNIQUELY find the names of all the extra
         parameters the methods will need to make them arguments of the
         collection generator and record then in a precise order that must
         be made public for the guys who want to instanciate the collection.

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let dump_collection_generator_arguments out_fmter compiled_species_fields =
  (* Let's create an assoc list mapping for each species paramater name *)
  (* the set of methods names from it that needed to be lambda-lifted,  *)
  (* hence that will lead to parameters of the collection generator.    *)
  let species_param_names_and_methods = ref [] in
  (* ************************************************************************ *)
  (** {b Descr} :  Local function to process only one [compiled_field_memory].
         Handy to factorize the code in both the cases of [CSF_let] and
         [CSF_let_rec]. This function effectivly accumulates by side effect
         for each species parameter the set of methods we depend on.

      { b Rem} : Local to the enclosing [dump_collection_generator_arguments]
               function. Not exported.                                        *)
  (* ************************************************************************ *)
  let rec process_one_field_memory field_memory =
    List.iter
      (fun (spe_param_name, meths_set) ->
	(* Get or create for this species parameter name, the bucket *)
	(* recording all the methods someone depends on.             *)
	let spe_param_bucket =
	  (try List.assoc spe_param_name !species_param_names_and_methods
	  with Not_found ->
	    let bucket = ref Dep_analysis.VnameSet.empty in
	    species_param_names_and_methods :=
	      (spe_param_name, bucket) :: !species_param_names_and_methods ;
	    bucket) in
	(* And now, union the current methods we depend on with *)
	(* the already previously recorded.                     *)
	spe_param_bucket :=
	  Dep_analysis.VnameSet.union meths_set !spe_param_bucket)
      field_memory.cfm_dependencies_from_parameters in

  (* ********************************************************** *)
  (* Now, really work, building by side effect for each species *)
  (* parameter the set of methods we depend on.                 *)
  List.iter
    (function
      | None -> ()
      | Some (CSF_let field_memory) -> process_one_field_memory field_memory
      | Some (CSF_let_rec l) -> List.iter process_one_field_memory l)
    compiled_species_fields ;
  (* Now we get the assoc list complete, we can dump the parameters of the  *)
  (* collection generator. To make them correct with their usage inside the *)
  (* local functions of the collection generator, we must give them a name  *)
  (* shaped in the same way, i.e:                                           *)
  (* "_p_" + species parameter name + "_" + called method name.             *)
  List.iter
    (fun (species_param_name, meths_set) ->
      let prefix =
	"_p_" ^
	(String.uncapitalize
	   (Parsetree_utils.name_of_vname species_param_name)) ^
	"_" in
      Dep_analysis.VnameSet.iter
	(fun meth ->
	  Format.fprintf out_fmter "@ %s%a"
	    prefix Misc_ml_generation.pp_to_ocaml_vname meth)
	!meths_set)
  !species_param_names_and_methods ;
  (* Finally, make this parameters information public by returning it. *)
  !species_param_names_and_methods
;;



let generate_collection_generator ctx compiled_species_fields =
  let current_species_name = snd ctx.scc_current_species in
  let out_fmter = ctx.scc_out_fmter in
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf
      "@\nSpecies %a is fully defined. Generating its collection generator@."
      Sourcify.pp_vname current_species_name ;
 
  (* ******************************************************************* *)
  (** {b Descr} : A local function to process one field. This allows to
                factorize the processing for both [Let] and [Let_rec]
                definitions.

      {b Rem} : Local to the [generate_collection_generator] function.
               Not exported.                                             *)
  (* ******************************************************************* *)
  let process_one_field field_memory =
    let from = field_memory.cfm_from_species in
    Format.fprintf out_fmter "(* From species %a. *)@\n"
      Sourcify.pp_qualified_vname from ;
    Format.fprintf out_fmter "@[<2>let local_%a =@ "
      Misc_ml_generation.pp_to_ocaml_vname field_memory.cfm_method_name ;
    (* Find the method generator to use depending on if it belongs to this *)
    (* inheritance level or if it was inherited from another species.      *)
    if from = ctx.scc_current_species then
      (begin
      (* It comes from the current inheritance level.   *)
      (* Then its name is simply the the method's name. *)
      Format.fprintf out_fmter "%a"
	Misc_ml_generation.pp_to_ocaml_vname field_memory.cfm_method_name
      end)
    else
      (begin
      (* It comes from a previous inheritance level. Then its name is *)
      (* the the module where the species inhabits if not the same    *)
      (* compilation unit than the current + "." + species name as    *)
      (* module + "." + the method's name.                            *)
      if (fst from) <> ctx.scc_current_unit then
	Format.fprintf out_fmter "%s.@," (String.capitalize (fst from)) ;
      Format.fprintf out_fmter "%a.@,%a"
	Misc_ml_generation.pp_to_ocaml_vname (snd from)
	Misc_ml_generation.pp_to_ocaml_vname field_memory.cfm_method_name
      end) ;
    (* Now, apply the method generator to each of the extra arguments *)
    (* induced by the various lambda-lifting we previously performed. *)
    (* First, the extra arguments due to the species parameters methods we  *)
    (* depends of. Here we will not use them to lambda-lift them this time, *)
    (* but to apply them !  The name used for application is formed         *)
    (* according to the same scheme we used at lambda-lifting time:         *)
    (* "_p_" + species parameter name + "_" + called method name.           *)
    List.iter
      (fun (species_param_name, meths_from_param) ->
	let prefix =
	  "_p_" ^
	  (String.uncapitalize
	     (Parsetree_utils.name_of_vname species_param_name)) ^
	  "_" in
	Dep_analysis.VnameSet.iter
	  (fun meth ->
	    Format.fprintf out_fmter "@ %s%a"
	      prefix Misc_ml_generation.pp_to_ocaml_vname meth)
	  meths_from_param)
      field_memory.cfm_dependencies_from_parameters ;
    (* Second, the methods of our inheritance tree we depend on and that are *)
    (* only declared. These methods leaded to "local" functions defined      *)
    (* above. Hence, for each  method only declared of ourselves we depend   *)
    (* on, its name is "local_" + the method's name.                         *)
    List.iter
      (fun ({ Dep_analysis.nn_name = dep_name }, _) ->
	  Format.fprintf out_fmter "@ local_%a"
	    Misc_ml_generation.pp_to_ocaml_vname dep_name)
      field_memory.cfm_decl_children ;
    (* That's it for this field code generation. *)
    Format.fprintf out_fmter "@ in@]@\n" in    

  (* *********************** *)
  (* Now, let's really work. *)
  (* A little comment in the generated OCaml code. *)
  Format.fprintf out_fmter
    "(* Fully defined '%a' species's collection generator. *)@\n"
    Sourcify.pp_vname current_species_name ;
  (* The generic name of the collection generator: "collection_create". *)
  Format.fprintf out_fmter "@[<2>let collection_create" ;
  (* Generate the parameters the collection generator needs to build the   *)
  (* each of the current species's local function (functions corresponding *)
  (* to the actuall method stored in the collection record).               *)
  let _public_stuff_to_use_later =
    dump_collection_generator_arguments out_fmter compiled_species_fields in
  Format.fprintf out_fmter " =@ " ;
  (* Generate the local functions that will be used to fill the record value. *)
  List.iter
    (function
      | None -> ()
      | Some (CSF_let field_memory) -> process_one_field field_memory
      | Some (CSF_let_rec l) -> List.iter (fun fm -> process_one_field fm) l)
    compiled_species_fields ;
  (* And now, the record value. We just assign each record fields    *)
  (* corresponding to the current species's method the corresponding *)
  (* local function we defined just above. Remind that the record    *)
  (* field's is the species's name + "_" + the method's name.        *)
  (* The local function corresponding to the method is "local_" +    *)
  (* the method's name.                                              *)
  Format.fprintf ctx.scc_out_fmter "@[<2>{ " ;
  (* Factorize the prefix to put in from of each species's field *)
  (* to get the corresponding record field name on OCaml's side. *)
  let field_prefix =
    String.uncapitalize (Parsetree_utils.name_of_vname current_species_name) in
  List.iter
      (function
      | None -> ()
      | Some (CSF_let field_memory) ->
	  Format.fprintf ctx.scc_out_fmter "%s_%a =@ local_%a ;@\n"
	    field_prefix
	    Misc_ml_generation.pp_to_ocaml_vname field_memory.cfm_method_name
	    Misc_ml_generation.pp_to_ocaml_vname field_memory.cfm_method_name
      | Some (CSF_let_rec l) ->
	  List.iter
	    (fun field_memory ->
	      Format.fprintf ctx.scc_out_fmter "%s_%a =@ local_%a ;@\n"
		field_prefix
		Misc_ml_generation.pp_to_ocaml_vname
		field_memory.cfm_method_name
		Misc_ml_generation.pp_to_ocaml_vname
		field_memory.cfm_method_name)
	    l)
    compiled_species_fields ;
  (* Close the record expression. *)
  Format.fprintf ctx.scc_out_fmter "@ }@]@\n" ;
  (* Close the pretty-print box of the "let collection_create =". *)
  Format.fprintf ctx.scc_out_fmter "@]@\n" ;
;;


       
let species_compile ~current_unit out_fmter species_name species_descr
    dep_graph =
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf "Generating OCaml code for species %a@."
      Sourcify.pp_vname species_name ;
  (* Start the module encapsulating the species representation. *)
  Format.fprintf out_fmter "@[<2>module %a =@\nstruct@\n"
    Sourcify.pp_vname species_name ;
  (* Now, establish the mapping between collections available *)
  (* and the type variable names representing their carrier.  *)
  let collections_carrier_mapping =
    build_collections_carrier_mapping ~current_unit species_descr in
  (* Create the initial compilation context for this species. *)
  let ctx = {
    scc_current_unit = current_unit ;
    scc_current_species = (current_unit, species_name) ;
    scc_dependency_graph_nodes = dep_graph ;
    scc_collections_carrier_mapping = collections_carrier_mapping ;
    scc_out_fmter = out_fmter } in
  (* The record type representing the species' type. *)
  generate_record_type ctx species_name species_descr ;
  (* Compute the list of names of parameters of the species. This   *)
  (* will be use to compute for each method the set of methods from *)
  (* the parameters the method depends on.                          *)
  let species_parameters_names =
    List.map
      (function
	| Env.TypeInformation.SPAR_in (n, _)
	| Env.TypeInformation.SPAR_is (n, _) -> n)
      species_descr.Env.TypeInformation.spe_sig_params in
  (* Now, the methods of the species. *)
  let compiled_fields =
    List.map
      (generate_methods ctx species_parameters_names)
      species_descr.Env.TypeInformation.spe_sig_methods in
  (* Now check if the species supports a collection *)
  (* generator because fully defined.               *)
  if species_descr.Env.TypeInformation.spe_is_closed then
    generate_collection_generator ctx compiled_fields ;
  Format.fprintf out_fmter "end ;;@]@\n@."
;;



let collection_compile ~current_unit out_fmter coll_def coll_descr dep_graph =
  let coll_name = coll_def.Parsetree.ast_desc.Parsetree.cd_name in
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf "Generating OCaml code for collection %a@."
      Sourcify.pp_vname coll_name ;
  (* Start the module encapsulating the collection representation. *)
  Format.fprintf out_fmter "@[<2>module %a =@\nstruct@\n"
    Sourcify.pp_vname coll_name ;
  (* Now, establish the mapping between collections available *)
  (* and the type variable names representing their carrier.  *)
  let collections_carrier_mapping =
    build_collections_carrier_mapping ~current_unit coll_descr in
  (* Create the initial compilation context for this collection. *)
  let ctx = {
    scc_current_unit = current_unit ;
    scc_current_species = (current_unit, coll_name) ;
    scc_dependency_graph_nodes = dep_graph ;
    scc_collections_carrier_mapping = collections_carrier_mapping ;
    scc_out_fmter = out_fmter } in
  (* First, because "rep" is (it really must otherwise we bugged in our *)
  (* analyses before !) defined. then we generate the type constraint   *)
  (* reflecting its effective structure.                                *)
  generate_rep_constraint_in_record_type
    ctx coll_descr.Env.TypeInformation.spe_sig_methods ;
  (* Now generate the value representing the effective instance of the *)
  (* collection. We always name it by "effective_collection".          *)
  Format.fprintf out_fmter "@[<2>let effective_collection =@ " ;
  (* Now, get the collection generator from the closed species we implement. *)
  let implemented_species_name =
    coll_def.Parsetree.ast_desc.Parsetree.
      cd_body.Parsetree.ast_desc.Parsetree.se_name in
  (match implemented_species_name.Parsetree.ast_desc with
   | Parsetree.I_local vname
   | Parsetree.I_global (None, vname) ->
       (* Local species, so no need to find it in another ML "file-module". *)
       Format.fprintf out_fmter "%s.collection_create"
	 (String.capitalize (Parsetree_utils.name_of_vname vname))
   | Parsetree.I_global ((Some fname), vname) ->
       (* If the specified module name is the current compilation unit, *)
       (* then again no need to find the species's module in another ML *)
       (* "file-module" otherwise we explicitely prefix by the module   *)
       (* name corresponding to the filename.                           *)
       if fname <> current_unit then
	 Format.fprintf out_fmter "%s." (String.capitalize fname) ;
       Format.fprintf out_fmter "%s.collection_create"
	 (String.capitalize (Parsetree_utils.name_of_vname vname))) ;
  (* Finally, we must recover the arguments to apply to this collection    *)
  (* generator. These arguments of course come from the species parameters *)
  (* the closed species we implement has (if it has some). We must         *)
  (* make this application WITH THE RIGHT EFFECTIVE FUNCTIONS and IN THE   *)
  (* RIGHT ORDER !                                                         *)

  (* End the definition of the value representing the effective instance. *)
  Format.fprintf out_fmter "@]@\n" ;
  (* End the module representing the collection. *)
  Format.fprintf out_fmter "end ;;@]@\n@."
;;
