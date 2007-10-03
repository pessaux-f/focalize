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


(* $Id: core_ml_generation.ml,v 1.14 2007-10-03 15:49:05 pessaux Exp $ *)



(* ************************************************************************ *)
(** {b Descr} : Exception raised when an external value definition does not
              not provides any correspondance with OCaml.

    {b Rem} : Exported outside this module.                                 *)
(* ************************************************************************ *)
exception No_external_value_caml_def of (Parsetree.vname * Location.t) ;;



(* ************************************************************************ *)
(** {b Descr} : Exception raised when an external type definition does not
              not provides any correspondance with OCaml.

    {b Rem} : Exported outside this module.                                 *)
(* ************************************************************************ *)
exception No_external_type_caml_def of (Parsetree.vname * Location.t) ;;


(* ********************************************************************* *)
(* string -> string                                                      *)
(** {b Descr} : Translate a FoC operator name to a legal OCaml function
              name, preventing the versatile FoC operators names from
              being lexically incorrect if straighforwardly converted
              into OCaml identifiers.
	      The transformation is pretty stupid, replacing all the
              legal "symbolic" characters available for FoC prefix/infix
              idents (extracted from the lexer definitions) by a regular
              string.

    {b Rem} : Not exported outsidethis module.                           *)
(* ********************************************************************* *)
let parse_operator_string op_string =
  let renamed_operator = ref "" in
  String.iter
    (fun character ->
      let str_tail =
	(match character with
	 | '`' -> "_focop_bquote_"
	 | '~' -> "_focop_tilda_"
	 | '?' -> "_focop_question_"
	 | '$' -> "_focop_dollar_"
	 | '!' -> "_focop_bang_"
	 | '#' -> "_focop_sharp_"
	 | '+' -> "_focop_plus_"
	 | '-' -> "_focop_minus_"
	 | '*' -> "_focop_star_"
	 | '/' -> "_focop_slash_"
	 | '%' -> "_focop_percent_"
	 | '&' -> "_focop_ampers_"
	 | '|' -> "_focop_pipe_"
	 | ',' -> "_focop_comma_"
	 | ':' -> "_focop_colon_"
	 | ';' -> "_focop_semi_"
	 | '<' -> "_focop_lt_"
	 | '=' -> "_focop_eq_"
	 | '>' -> "_focop_gt_"
	 | '@' -> "_focop_at_"
	 | '^' -> "_focop_hat_"
	 | '\\' -> "_focop_bslash"
	 | whatever ->
	     (* For any other character, keep it unchanged. *)
	     let s = " " in
	     s.[0] <- whatever ;
	     s) in
      (* Appending on string is not very efficient, but *)
      (* this should not be a real matter here ! *)
      renamed_operator := !renamed_operator ^ str_tail)
    op_string ;
  (* Just return the "translated" identifier name. *)
  !renamed_operator
;;



(* ******************************************************************** *)
(* Format.formatter -> Parsetree.vname -> unit                          *)
(** {b Descr} : Pretty prints a [vname] value as OCaml source. Because
              FoC allows more infix/prefix operators then OCaml syntaxe
              it's impossible to crudely translate the string of the
              [vname] to OCaml.
              For instance, a FoC infix operator "( **+ )" has no
              equivalent in OCaml syntaxe : "( **+ )" is not a correct
              operator identifier according to OCaml.
              Then, instead of havign particular cases for operators
              that can be straighforward translated (like "( +)") and
              the others, we adopt a uniform mapping for infix and
              prefix operators using the [parse_operator_string]
              function to transform infix/prefix operators names
              before printing and straighforwardly print other
              operators names.

    {b Rem} : Exported ouside this module.                              *)
(* ******************************************************************** *)
let pp_to_ocaml_vname ppf = function
  | Parsetree.Vlident s
  | Parsetree.Vuident s
  | Parsetree.Vqident s -> Format.fprintf ppf "%s" s
  | Parsetree.Vpident s
  | Parsetree.Viident s -> Format.fprintf ppf "%s" (parse_operator_string s)
;;



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



(* ********************************************************************* *)
(** {b Descr} : Data structure to record the various stuff needed to
          generate the OCaml code for various constructs. Passing this
          structure prevents from recursively passing a bunch of
          parameters to the functions. Instead, one pass only one and
          functions use the fields they need. This is mostly to preserve
          the stack and to make the code more readable. In fact,
          information recorded in this structure is semantically pretty
          uninteresting to understand the compilation process: it is more
          utilities.

    {b Rem} Not exported outside this module.                            *)
(* ********************************************************************* *)
type reduced_compil_context = {
  (** The name of the currently analysed compilation unit. *)
  rcc_current_unit : Types.fname ;
   (** The current output formatter where to send the generated code. *)
  rcc_out_fmter : Format.formatter
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
	      "'" ^ (String.lowercase n_as_string) ^ "_as_carrier" in
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
	      "' " ^ (String.lowercase (Parsetree_utils.name_of_vname n)) ^
	      "_as_carrier" in
	    (type_coll, carrier_type_variable_name))
      species_descr.Env.TypeInformation.spe_sig_params
;;



(* ************************************************************************ *)
(** {b Descr} : Because methods parameters do not have their type with them
              in the [species_description]s, this function establish the
              mapping between the parameters names and their related type.
              It dissecates method's the type scheme (more accurately, an
              instance of it), "removing" arrows parameter after parameter.
              Because the typechecking pass is already done, the FoCaL
              program is well-typed, hence, the type of the method must
              have "as many arrows as" the method has parameters. If this
              is not the case, then we have a bug somewhere else in the
              previous processes in the compiler.
              This function hence returns the list giving for each
              parameter name its type and the "result" type of the method
              (i.e. the type remaining after having "removed all the
              arrows" induced by the parameters).

    {b Rem} : Not exported outside this module.                             *)
(* ************************************************************************ *)
let bind_parameters_to_types_from_type_scheme opt_scheme params_names =
  match opt_scheme with
   | None ->
       (* Since we are not given any type information, the binding will *)
       (* be trivially void and no type constraint will be printed.     *)
       (List.map (fun p_name -> (p_name, None)) params_names)
   | Some scheme ->
       (begin
       try
	 let type_from_scheme = Types.specialize scheme in
	 (* Be careful, the bindings list is built reversed ! We must finally *)
	 (* reverse it again to keep the right order (i.e. first argument in  *)
	 (* head of the list.                                                 *)
	 let rec rec_bind accu_bindings ty = function
	   | [] -> accu_bindings
	   | h :: q ->
	       (* We split the functionnal type thanks to the unification. *)
	       (* First, get a variable to hold the argument type. *)
 	       let h_type = Types.type_variable () in
	       (* Next, get a variable to hold the result type. *)
	       let q_type = Types.type_variable () in
	       (* And now assign by the unification's *)
               (* side effects these 2 variables.     *)
	       Types.unify
		 ~loc: Location.none ~self_manifest: None
		 (Types.type_arrow h_type q_type) ty ;
	       (* We bind the current parameter to the "head-type" *)
               (* and continue with the remaining parameters using *)
               (* the "tail-type".                                 *)
	       rec_bind ((h, (Some h_type)) :: accu_bindings) q_type q in

	 (* ********************** *)
	 (* Now, let's do the job. *)
	 let revd_mapping = rec_bind [] type_from_scheme params_names in
	 (* Put the resulting mapping in the right order. *)
	 List.rev revd_mapping
       with _ ->
	 (* Because the typechecking was done in the previous passes, the   *)
	 (* program must be well-typed at this point. Then unification must *)
	 (* always be successfull. If it fails, then there is a bug         *)
         (* somewhere else before !                                         *)
	 assert false
       end)
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
(* species_compil_context -> Parsetree.species_def ->                       *)
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
let generate_record_type ctx species_def species_descr =
  let out_fmter = ctx.scc_out_fmter in
  let collections_carrier_mapping = ctx.scc_collections_carrier_mapping in
  (* First, check if "rep" is defined. If so, then generate  *)
  (* the type constraint reflecting its effective structure. *)
  generate_rep_constraint_in_record_type
    ctx species_descr.Env.TypeInformation.spe_sig_methods ;
  let field_prefix =
    String.lowercase
      (Parsetree_utils.name_of_vname
	 species_def.Parsetree.ast_desc.Parsetree.sd_name) in
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
	      field_prefix pp_to_ocaml_vname n
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
		field_prefix pp_to_ocaml_vname n
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



(* ************************************************************************* *)
(* Format.formatter -> Parsetree.constant -> unit                            *)
(** {b Descr} : Generate the OCaml code from a FoCaL constant expression.

    {b Args} :
      - [out_fmter] : The out channel where to generate the OCaml source
                    code.
      - [constant] : The constant expression to translate into OCaml source
          code.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let generate_constant out_fmter constant =
  match constant.Parsetree.ast_desc with
   | Parsetree.C_int i -> Format.fprintf out_fmter "%s" i
   | Parsetree.C_float fl -> Format.fprintf out_fmter "%s" fl
   | Parsetree.C_bool b -> Format.fprintf out_fmter "%s" b
   | Parsetree.C_string s ->
       Format.fprintf out_fmter "\"%s\"" (String.escaped s)
   | Parsetree.C_char c -> Format.fprintf out_fmter "'%c'" c
;;



(* **************************************************************** *)
(* reduced_compil_context -> Parsetree.ident -> unit                *)
(** {b Descr} : Generate the OCaml code from a FoCaL [ident] in the
              context of method generator generation.

    {b Rem} : Not exported outside this module.                     *)
(* **************************************************************** *)
let generate_ident_for_method_generator ctx ident =
  match ident.Parsetree.ast_desc with
   | Parsetree.I_local vname ->
       (* Thanks to the scoping pass, identifiers remaining "local" are *)
       (* really let-bound in the contect of the expression, hence have *)
       (* a direrct mapping between FoCaL and OCaml code.               *)
       Format.fprintf ctx.rcc_out_fmter "%a" pp_to_ocaml_vname vname
   | Parsetree. I_global (fname_opt, vname) ->
       (begin
       match fname_opt with
	| None ->
	    (* Thanks to the scoping pass, [I_global] with a [None] module *)
            (* name are toplevel definitions of the current compilation    *)
            (* unit. Then hence can be straightforwardly called in the     *)
            (* OCaml code.                                                 *)
	    Format.fprintf ctx.rcc_out_fmter "%a" pp_to_ocaml_vname vname
	| Some mod_name ->
	    (* Call the OCaml corresponding identifier in the corresponding *)
            (* module (i.e. the capitalized [mod_name]). If the module is   *)
	    (* the currently compiled one, then do not qualify the          *)
	    (* identifier.                                                  *)
	    if mod_name <> ctx.rcc_current_unit then
	      Format.fprintf ctx.rcc_out_fmter "%s.%a"
		(String.capitalize mod_name) pp_to_ocaml_vname vname
	    else
	      Format.fprintf ctx.rcc_out_fmter "%a" pp_to_ocaml_vname vname
       end)
;;



(* **************************************************************** *)
(* reduced_compil_context -> Parsetree.expr_ident -> unit           *)
(** {b Descr} : Generate the OCaml code from a FoCaL [ident] in the
              context of method generator generation.

    {b Rem} : Not exported outside this module.                     *)
(* **************************************************************** *)
let generate_expr_ident_for_method_generator ctx ident =
  match ident.Parsetree.ast_desc with
   | Parsetree.EI_local vname ->
       (* Thanks to the scoping pass, identifiers remaining "local" are *)
       (* really let-bound in the contect of the expression, hence have *)
       (* a direrct mapping between FoCaL and OCaml code.               *)
       Format.fprintf ctx.rcc_out_fmter "%a" pp_to_ocaml_vname vname
   | Parsetree.EI_global (fname_opt, vname) ->
       (begin
       match fname_opt with
	| None ->
	    (* In this case, may be there is some scoping process missing. *)
	    assert false
	| Some mod_name ->
	    (* Call the OCaml corresponding identifier in the corresponding *)
            (* module (i.e. the capitalized [mod_name]). If the module is   *)
	    (* the currently compiled one, then do not qualify the          *)
	    (* identifier.                                                  *)
	    if mod_name <> ctx.rcc_current_unit then
	      Format.fprintf ctx.rcc_out_fmter "%s.%a"
		(String.capitalize mod_name) pp_to_ocaml_vname vname
	    else
	      Format.fprintf ctx.rcc_out_fmter "%a" pp_to_ocaml_vname vname
       end)
   | Parsetree.EI_method (coll_specifier_opt, vname) ->
       (begin
       match coll_specifier_opt with
	| None ->
	    (* Method call from the current species. This corresponds to *)
	    (* a call to the corresponding lambda-lifted method that is  *)
	    (* represented as an extra parameter of the OCaml function.  *)
	    Format.fprintf ctx.rcc_out_fmter "abst_%a" pp_to_ocaml_vname vname
	| Some coll_specifier ->
	    (begin
	    match coll_specifier with
	     | (None, coll_name) ->
		 (begin
		 (* Method call from a species that is not the current but  *)
		 (* is implicitely in the current compilation unit. May be  *)
		 (* either a paramater or a toplevel defined collection. To *)
		 (* retrieve the related method name we build it the same   *)
		 (* way we built it while generating the extra OCaml        *)
		 (* function's parameters due to depdencencies coming from  *)
                 (* the species parameter. I.e: "_p_", followed by the      *)
		 (* species parameter name, followed by "_", followed by    *)
		 (* the method's name.                                      *)
		 let prefix =
		   "_p_" ^
		   (String.lowercase
		      (Parsetree_utils.name_of_vname coll_name)) ^
		   "_" in
		 Format.fprintf ctx.rcc_out_fmter
		   "%s%a" prefix pp_to_ocaml_vname vname
		 end)
	     | (Some module_name, coll_name) ->
		 (begin
		 if module_name = ctx.rcc_current_unit then
		   (begin
		   (* Exactly like when it is method call from a species that *)
                   (* is not the current but is implicitely in the current    *)
		   (* compilation unit : the call is performed to a method    *)
                   (* a species that is EXPLICITELY in the current            *)
                   (* compilation unit.                                       *)
		   let prefix =
		     "_p_" ^
		     (String.lowercase
			(Parsetree_utils.name_of_vname coll_name)) ^
		     "_" in
		   Format.fprintf ctx.rcc_out_fmter
		     "%s%a" prefix pp_to_ocaml_vname vname
		   end)
		 else
		   (begin
		   (* The called method belongs to a species that is not    *)
		   (* ourselves and moreover belongs to another compilation *)
		   (* unit. May be a species from the toplevel of another   *)
		   (* FoCaL source file.                                    *)
		   failwith "generate_expr_ident_for_method_generator foreign species's module TODO"
		   end)
		 end)
	    end)
       end)
;;



(* ******************************************************************** *)
(* reduced_compil_context ->  Parsetree.constructor_ident -> unit       *)
(** {b Descr} : Generate the OCaml code from a FoCaL [constructor_expr]
              in the context of method generator generation.

    {b Rem} : Not exported outside this module.                         *)
(* ******************************************************************** *)
let generate_constructor_ident_for_method_generator ctx cstr_expr =
  match cstr_expr.Parsetree.ast_desc with
   | Parsetree.CI (fname_opt, name) ->
       (begin
       match fname_opt with
	| None -> Format.fprintf ctx.rcc_out_fmter "%a" pp_to_ocaml_vname name
	| Some fname ->
	    (* If the constructor belongs to the current      *)
	    (* compilation unit then one must not qualify it. *)
	    if fname <> ctx.rcc_current_unit then
	      Format.fprintf ctx.rcc_out_fmter "%s.%a"
		(String.capitalize fname) pp_to_ocaml_vname name
	    else
	      Format.fprintf ctx.rcc_out_fmter "%a" pp_to_ocaml_vname name
       end)
;;



let generate_pattern ctx pattern =
  let out_fmter = ctx.rcc_out_fmter in
  let rec rec_gen_pat pat =
    match pat.Parsetree.ast_desc with
     | Parsetree.P_const constant -> generate_constant out_fmter constant
     | Parsetree.P_var name ->
	 Format.fprintf out_fmter "%a" pp_to_ocaml_vname name
     | Parsetree.P_as (p, name) ->
	 Format.fprintf out_fmter "(" ;
	 rec_gen_pat p ;
	 Format.fprintf out_fmter "as@ %a)" pp_to_ocaml_vname name
     | Parsetree.P_wild -> Format.fprintf out_fmter "_"
     | Parsetree.P_constr (ident, pats) ->
	 (begin
	 generate_constructor_ident_for_method_generator ctx ident ;
	 (* Discriminate on the umber of arguments *)
         (* to know if parens are needed.          *)
	 match pats with
	  | [] -> ()
	  | _ ->
	      Format.fprintf out_fmter " (" ;
	      rec_generate_pats_list pats ;
	      Format.fprintf out_fmter ")"
	 end)
     | Parsetree.P_record _labs_pats ->
	 Format.eprintf "generate_pattern P_record TODO@."
     | Parsetree.P_tuple pats ->
	 Format.fprintf out_fmter "(@[<1>" ;
	 rec_generate_pats_list pats ;
	 Format.fprintf out_fmter ")@]"
     | Parsetree.P_paren p ->
	 Format.fprintf out_fmter "(@[<1>" ;
	 rec_gen_pat p ;
	 Format.fprintf out_fmter ")@]"


  and rec_generate_pats_list = function
    | [] -> ()
    | [last] -> rec_gen_pat last
    | h :: q ->
	rec_gen_pat h ;
	Format.fprintf out_fmter ",@ " ;
	rec_generate_pats_list q in
  (* ********************** *)
  (* Now, let's do the job. *)
  rec_gen_pat pattern
;;



let rec let_binding_compile ctx collections_carrier_mapping bd opt_sch =
  let out_fmter = ctx.rcc_out_fmter in
  (* Generate the bound name. *)
  Format.fprintf out_fmter "%a"
    pp_to_ocaml_vname bd.Parsetree.ast_desc.Parsetree.b_name ;
  (* Generate the parameters if some, with their type constraints. *)
  let params_names = List.map fst bd.Parsetree.ast_desc.Parsetree.b_params in
  let params_with_type =
    bind_parameters_to_types_from_type_scheme opt_sch params_names in
  (* We are printing each parameter's type. These types in fact belong *)
  (* to a same type scheme. Hence, they may share variables together.  *)
  (* For this reason, we first purge the printing variable mapping and *)
  (* after, activate its persistence between each parameter printing.  *)
  Types.purge_type_simple_to_ml_variable_mapping () ;
  List.iter
    (fun (param_vname, pot_param_ty) ->
      match pot_param_ty with
       | Some param_ty ->
	   Format.fprintf out_fmter "@ (%a : %a)"
	     pp_to_ocaml_vname param_vname
	     (Types.pp_type_simple_to_ml
		~current_unit: ctx.rcc_current_unit
		~reuse_mapping: true collections_carrier_mapping) param_ty
       | None ->
	   Format.fprintf out_fmter "@ %a" pp_to_ocaml_vname param_vname)
    params_with_type ;
  (* Now we don't need anymore the sharing. Hence, clean it. This should not *)
  (* be useful because the other guys usign printing should manage this      *)
  (* themselves (as we did just above by cleaning before activating the      *)
  (* sharing), but anyway, it is safer an not costly. So...                  *)
  Types.purge_type_simple_to_ml_variable_mapping () ;
  (* Output now the "=" sign ending the OCaml function's "header".    *)
  (* With a NON-breakable space before to prevent uggly hyphenation ! *)
  Format.fprintf out_fmter " =@ " ;
  (* Now, let's generate the bound body. *)
  generate_expr ctx bd.Parsetree.ast_desc.Parsetree.b_body



(* ************************************************************************ *)
(* reduced_compil_context -> Parsetree.let_def -> Types.type_scheme list -> *)
(*   unit                                                                   *)
(** {b Desrc} : Generates the OCaml code for a FoCaL "let"-definition.

    {b Args} :
      - [out_fmter] : The out channel where to generate the OCaml source
              code.
      - [let_def] : The [Parsetree.let_def] structure representing the
              "let-definition" for which th generate the OCaml source
               code.
      - [bound_schemes] : The list of types schemes of the identifiers
              bound to the "let-definition" (i.e. several if the
              definition is a "rec", hence binds several identifiers).
              In effect, because we do not have directly inside the
              [Parsetree.let_def] these schemes, in order to be able to
	      generate the type constraints of each components of the
              "let-definition", we must take these schemes aside.
              It is sometimes impossible yet to have this information.
              In this case, no type constraint will be added to the
              parameter of the bound identifiers.

    {b Rem} : Not exported outside this module.                             *)
(* ************************************************************************ *)
and let_def_compile ctx let_def opt_bound_schemes =
  let out_fmter = ctx.rcc_out_fmter in
  (* Generates the binder ("rec" or non-"rec"). *)
  Format.fprintf out_fmter "@[<2>let%s@ "
    (match let_def.Parsetree.ast_desc.Parsetree.ld_rec with
     | Parsetree.RF_no_rec -> ""
     | Parsetree.RF_rec -> " rec"   (* NON-breakable space in front. *)) ;
  (* Now generate each bound definition. *)
  (* Because we are in a toplevel [let_def], we have no species       *)
  (* parameter in the scope. Hence the collections carrier mapping is *)
  (* empty.                                                           *)
  (* [Unsure]. What should happen in term of generated code and what is the *)
  (* meaning of a method call from a toplevel defined collection ????????   *)
  (match (let_def.Parsetree.ast_desc.Parsetree.ld_bindings,
	  opt_bound_schemes) with
   | ([], []) ->
       (* The let construct should always at least bind one identifier ! *)
       assert false
   | ([one_bnd], [one_scheme]) ->
       let_binding_compile ctx [] one_bnd one_scheme
   | ((first_bnd :: next_bnds), (first_scheme :: next_schemes)) ->
       let_binding_compile ctx [] first_bnd first_scheme ;
       List.iter2
	 (fun binding scheme ->
	   Format.fprintf out_fmter "@]@\n@[<2>and " ;
	   let_binding_compile ctx [] binding scheme)
	 next_bnds
	 next_schemes
   | (_, _) ->
       (* Because the FoCaL has been parsed and typechecked, we must never    *)
       (* have a different number of bound identifiers and bound-identifiers' *)
       (* type schemes. If this arise, then we have a serious bug somewhere.  *)
       assert false) ;
  Format.fprintf out_fmter "@]"



(* ************************************************************* *)
(* reduced_compil_context -> Parsetree.expr -> unit              *)
(** {b Descr} : Generate the OCaml code from a FoCaL expression.

    {b Rem} : Not exported outside this module.                  *)
(* ************************************************************* *)
and generate_expr ctx initial_expression =
  let out_fmter = ctx.rcc_out_fmter in
  let rec rec_generate expr =
    (* Generate the source code for the expression. *)
    match expr.Parsetree.ast_desc with
     | Parsetree.E_self ->
	 Format.eprintf "generate_expr E_self TODO@." (* D'ailleurs, est-ce possible en fait ? *)
     | Parsetree.E_const cst -> generate_constant out_fmter cst
     | Parsetree.E_fun (args_names, body) ->
	 List.iter
	   (fun n ->
	     Format.fprintf out_fmter "@[<2>fun@ %a@ ->@ " pp_to_ocaml_vname n)
	   args_names ;
	 rec_generate body ;
         Format.fprintf out_fmter "@]"
     | Parsetree.E_var ident ->
	 generate_expr_ident_for_method_generator ctx ident
     | Parsetree.E_app (expr, exprs) ->
	 Format.fprintf out_fmter "@[<2>(" ;
	 rec_generate expr ;
	 Format.fprintf out_fmter "@ " ;
	 rec_generate_exprs_list ~comma: false exprs ;
	 Format.fprintf out_fmter ")@]"
     | Parsetree.E_constr (cstr_expr, exprs) ->
	 (begin
	 generate_constructor_ident_for_method_generator ctx cstr_expr ;
	 match exprs with
	  | [] -> ()
	  | _ ->
	      (* If argument(s), enclose by parens to possibly make a tuple. *)
	      Format.fprintf out_fmter "@ @[<1>(" ;
	      rec_generate_exprs_list ~comma: true exprs ;
	      Format.fprintf out_fmter ")@]"
	 end)
     | Parsetree.E_match (matched_expr, pats_exprs) ->
	 (begin
	 Format.fprintf out_fmter "@[<1>match " ;
	 rec_generate matched_expr ;
	 Format.fprintf out_fmter " with" ;
	 List.iter
	   (fun (pattern, expr) ->
	     (* My indentation style: indent of 4 between *)
	     (* the pattern and its related processing.   *)
	     Format.fprintf out_fmter "@\n@[<4>| " ;
	     generate_pattern ctx pattern ;
	     (* Enclose each match-case by begin/end to ensure no confusion. *)
             Format.fprintf out_fmter " ->@\n(begin@\n" ;
	     rec_generate expr ;
	     Format.fprintf out_fmter "@\nend)@]")
	   pats_exprs ;
	 Format.fprintf out_fmter "@]"
	 end)
     | Parsetree.E_if (expr1, expr2, expr3) ->
	 Format.fprintf out_fmter "@[<2>if@ " ;
	 rec_generate expr1 ;
	 Format.fprintf out_fmter "@ @[<2>then@ @]" ;
	 rec_generate expr2 ;
         Format.fprintf out_fmter "@ @[<2>else@ @]" ;
	 rec_generate expr3 ;
         Format.fprintf out_fmter "@]"
     | Parsetree.E_let (let_def, in_expr) ->
	 (* Here we do not have trype contraints under the hand. So give-up *)
         (* generating such constraints. Just generate the raw code for the *)
         (* "let-definition".                                               *)
	 let bound_schemes =
	   List.map
	     (fun _ -> None)
	     let_def.Parsetree.ast_desc.Parsetree.ld_bindings in
	 let_def_compile ctx let_def bound_schemes ;
	 Format.fprintf out_fmter "@ in@\n" ;
	 rec_generate in_expr
     | Parsetree.E_record labs_exprs ->
	 (begin
	 Format.fprintf out_fmter "@[<1>{@ " ;
	 rec_generate_record_field_exprs_list labs_exprs ;
	 Format.fprintf out_fmter "@ }@]"
	 end)
     | Parsetree.E_record_access (expr, label_name) ->
	 (begin
	 Format.fprintf out_fmter "@[<2>" ;
	 rec_generate expr ;
	 Format.fprintf out_fmter ".@,%s@]" label_name
	 end)
     | Parsetree.E_record_with (expr, labs_exprs) ->
	 (begin
	 (* Because in OCaml the with construct only starts by an ident, we *)
	 (* create a temporary ident to bind the expression to an ident.    *)
	 Format.fprintf out_fmter "@[<2>let __foc_tmp_with_ =@ " ;
	 rec_generate expr ;
	 Format.fprintf out_fmter "@ in@] " ;
	 (* Now really generate the "with"-construct. *)
	 Format.fprintf out_fmter "@[<2>{ __foc_tmp_with_ with@\n" ;
	 List.iter
	   (fun (label_name, field_expr) ->
	     Format.fprintf out_fmter "%s =@ " label_name ;
	     rec_generate field_expr ;
	     Format.fprintf out_fmter " ;")
	   labs_exprs ;
	 Format.fprintf out_fmter "@ }@]"
	 end)
     | Parsetree.E_tuple exprs ->
	 (begin
	 match exprs with
	  | [] -> assert false
	  | [one] -> rec_generate one
	  | _ ->
	      Format.fprintf out_fmter "@[<1>(" ;
	      rec_generate_exprs_list ~comma: true exprs ;
	      Format.fprintf out_fmter ")@]"
	 end)
     | Parsetree.E_external external_expr ->
	 (begin
	 try
	   (* Simply a somewhat of verbatim stuff of the OCaml translation. *)
	   let (_, ocaml_binding) =
	     List.find
	       (function
		 | (Parsetree.EL_Caml, _) -> true
		 | (Parsetree.EL_Coq, _)
		 | ((Parsetree.EL_external _), _) -> false)
	       external_expr.Parsetree.ast_desc in
	   Format.fprintf out_fmter "%s" ocaml_binding
	 with Not_found ->
	   (* No OCam mapping found. *)
	   raise
	     (No_external_value_caml_def
		((Parsetree.Vlident "<expr>"), expr.Parsetree.ast_loc))
	 end)
     | Parsetree.E_paren e -> rec_generate e



  and rec_generate_exprs_list ~comma = function
    | [] -> ()
    | [last] -> rec_generate last
    | h :: q ->
	rec_generate h ;
	if comma then Format.fprintf out_fmter ",@ "
	else Format.fprintf out_fmter "@ " ;
	rec_generate_exprs_list ~comma q


  and rec_generate_record_field_exprs_list = function
    | [] -> ()
    | [(label, last)] ->
	Format.fprintf out_fmter "%s =@ " label ;
	rec_generate last
    | (h_label, h_expr) :: q ->
	Format.fprintf out_fmter "%s =@ " h_label ;
	rec_generate h_expr ;
	Format.fprintf out_fmter " ;@ " ;
	rec_generate_record_field_exprs_list q in
  (* ********************** *)
  (* Now, let's do the job. *)
  rec_generate initial_expression
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
	(* Only keep "decl-dependencies". *)
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
	  pp_to_ocaml_vname name ;
      (* Start the OCaml function definition. *)
      if is_first then
	Format.fprintf out_fmter "@[<2>let %a" pp_to_ocaml_vname name
      else
	Format.fprintf out_fmter "@[<2>and %a" pp_to_ocaml_vname name ;
      (* First, abstract according to the species's parameters the current  *)
      (* method depends on.                                                 *)
      List.iter
	(fun (species_param_name, meths) ->
	  (* Each abstracted method will be named like "_p_", followed by *)
	  (* the species parameter name, followed by "_", followed by the *)
          (* method's name.                                               *)
	  let prefix =
	    "_p_" ^
	    (String.lowercase
	       (Parsetree_utils.name_of_vname species_param_name)) ^
	    "_" in
	  Dep_analysis.VnameSet.iter
	    (fun meth ->
	      Format.fprintf out_fmter "@ %s%a" prefix pp_to_ocaml_vname meth)
	    meths)
	dependencies_from_params ;
      (* Now, lambda-lift all the dependencies from our inheritance tree *)
      (* (i.e methods we depend on).                                     *)
      List.iter
	(fun ({ Dep_analysis.nn_name = dep_name }, _) ->
	  Format.fprintf out_fmter "@ abst_%a" pp_to_ocaml_vname dep_name)
	decl_children ;
      (* Add the parameters of the let-binding with their type. *)
      let params_with_type =
	bind_parameters_to_types_from_type_scheme scheme params in
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
		 pp_to_ocaml_vname param_vname
		 (Types.pp_type_simple_to_ml
		    ~current_unit: ctx.scc_current_unit
		    ~reuse_mapping: true collections_carrier_mapping) param_ty
	   | None ->
	       Format.fprintf out_fmter "@ %a" pp_to_ocaml_vname param_vname)
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
	rcc_current_unit = ctx.scc_current_unit ;
	rcc_out_fmter = out_fmter } in
      generate_expr expr_ctx body ;
      (* Done... Then, final carriage return. *)
      Format.fprintf out_fmter "@]@\n"
      end)
    else
      (begin
      (* Just a bit of debug/information if requested. *)
      if Configuration.get_verbose () then
	Format.eprintf
          "Field '%a' inherited but not (re)-declared is not generated again.@."
	  pp_to_ocaml_vname name
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
	   pp_to_ocaml_vname name ;
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
	   pp_to_ocaml_vname name ;
       (* Nothing to keep for the collection generator. *)
       None
;;


(* *********************************************************************** *)
(** {b Descr} : Dumps as OCaml code the parameters required to the
         collection generator in order to make them bound in the
         collection generator's body. Hence, this function must UNIQUELY
         find the names of all the extra parameters the methods will need
         to make them arguments of the collection generator and record
         then in a precise order that must be made public for the guys who
         want to instanciate the collection.

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
	(String.lowercase
	   (Parsetree_utils.name_of_vname species_param_name)) ^
	"_" in
      Dep_analysis.VnameSet.iter
	(fun meth ->
	  Format.fprintf out_fmter "@ %s%a" prefix pp_to_ocaml_vname meth)
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
  (* Factorize the prefix to put in from of each species's field *)
  (* to get the corresponding record field name on OCaml's side. *)
  let _field_prefix =
    String.lowercase (Parsetree_utils.name_of_vname current_species_name) in
 
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
      pp_to_ocaml_vname field_memory.cfm_method_name ;
    (* Find the method generator to use depending on if it belongs to this *)
    (* inheritance level or if it was inherited from another species.      *)
    if from = ctx.scc_current_species then
      (begin
      (* It comes from the current inheritance level.   *)
      (* Then its name is simply the the method's name. *)
      Format.fprintf out_fmter "%a"
	pp_to_ocaml_vname field_memory.cfm_method_name
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
	pp_to_ocaml_vname (snd from)
	pp_to_ocaml_vname field_memory.cfm_method_name
      end) ;
    (* Now, apply the method generator to each of the extra arguments *)
    (* induced by the variosu lambda-lifting we previously performed. *)
    (* First, the extra arguments due to the species parameters methods we  *)
    (* depends of. Here we will not use them to lambda-lift them this time, *)
    (* but to apply them !  The name used for application is formed         *)
    (* according to the same scheme we used at lambda-lifting time:         *)
    (* "_p_" + species parameter name + "_" + called method name.           *)
    List.iter
      (fun (species_param_name, meths_from_param) ->
	let prefix =
	  "_p_" ^
	  (String.lowercase
	     (Parsetree_utils.name_of_vname species_param_name)) ^
	  "_" in
	Dep_analysis.VnameSet.iter
	  (fun meth ->
	    Format.fprintf out_fmter "@ %s%a" prefix pp_to_ocaml_vname meth)
	  meths_from_param)
      field_memory.cfm_dependencies_from_parameters ;
    (* Second, the method of our inheritance tree we depend on because these *)
    (* methods are now defined, we apply using these defined methods, i.e.   *)
    (* the "local" function defined here for this method. Hence, for each    *)
    (* method of ourselves we depend on, its name is "local_" + the method's *)
    (* name.                                                                 *)
    List.iter
      (fun ({ Dep_analysis.nn_name = dep_name }, _) ->
	Format.fprintf out_fmter "@ local_%a" pp_to_ocaml_vname dep_name)
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
  Format.fprintf ctx.scc_out_fmter "@ in@]@\n" ;
  (* The record value. *)
  Format.fprintf ctx.scc_out_fmter "{ @[<2>" ;
  (* Close the record expression. *)
  Format.fprintf ctx.scc_out_fmter "@] }@\n" ;
;;

       

let species_compile ~current_unit out_fmter species_def species_descr
    dep_graph =
  let species_def_desc = species_def.Parsetree.ast_desc in
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf "Generating OCaml code for species %a@."
      Sourcify.pp_vname species_def_desc.Parsetree.sd_name ;
  (* Start the module encapsulating the species representation. *)
  Format.fprintf out_fmter "@[<2>module %a =@\nstruct@\n"
    Sourcify.pp_vname species_def_desc.Parsetree.sd_name ;
  (* Now, establish the mapping between collections available *)
  (* and the type variable names representing their carrier.  *)
  let collections_carrier_mapping =
    build_collections_carrier_mapping ~current_unit species_descr in
  (* Create the initial compilation context for this species. *)
  let ctx = {
    scc_current_unit = current_unit ;
    scc_current_species = (current_unit, species_def_desc.Parsetree.sd_name) ;
    scc_dependency_graph_nodes = dep_graph ;
    scc_collections_carrier_mapping = collections_carrier_mapping ;
    scc_out_fmter = out_fmter } in
  (* The record type representing the species' type. *)
  generate_record_type ctx species_def species_descr ;
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



(* ************************************************************************* *)
(* reduced_compil_context -> Parsetree.external_type_def_body -> unit        *)
(** {b Descr} : Generates the OCaml code to bind a FoCaL type onto an OCaml
        existing type. If the FoCaL type name is the same than the OCaml
        one, then we silently ignore the type definition to avoid OCaml type
        definitions like [type int = int] that would lead to a cyclic type
        abbreviation and would be rejected by OCaml.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let external_type_def_compile ctx external_type_def =
  let external_type_def_desc = external_type_def.Parsetree.ast_desc in
  let out_fmter = ctx.rcc_out_fmter in
  (* Type definition header. *)
  Format.fprintf out_fmter "@[<2>type" ;
  (* Now, generate the type parameters if some. *)
  (match external_type_def_desc.Parsetree.etd_params with
   | [] -> ()
   | [one] ->
       (* Do not put parentheses because only one parameter. *)
       Format.fprintf out_fmter " %a" pp_to_ocaml_vname one
   | several ->
       (begin
       (* Enclose by parentheses and separate by commas. *)
       let rec rec_print_params = function
	 | [] -> ()
	 | [last] -> Format.fprintf out_fmter "%a" pp_to_ocaml_vname last
	 | first :: rem ->
	     Format.fprintf out_fmter "%a,@ " pp_to_ocaml_vname first ;
	     rec_print_params rem in
       Format.fprintf out_fmter " (@[<1>" ;
       rec_print_params several ;
       Format.fprintf out_fmter "@])"
       end)) ;
  (* Now, the type name, renamed as "_focty_" followed by the original name. *)
  Format.fprintf out_fmter " _focty_%a =@ "
    pp_to_ocaml_vname external_type_def_desc.Parsetree.etd_name ;
  (* And now, bind the FoCaL identifier to the OCaml one. *)
  try
    let (_, ocaml_binding) =
      List.find
	(function
	  | (Parsetree.EL_Caml, _) -> true
	  | (Parsetree.EL_Coq, _)
	  | ((Parsetree.EL_external _), _) -> false)
	external_type_def_desc.Parsetree.etd_body.Parsetree.ast_desc  in
    Format.fprintf out_fmter "%s@]@ ;;@\n" ocaml_binding
  with Not_found ->
    (* We didn't find any correspondance for OCaml. *)
    raise
      (No_external_type_caml_def
	 (external_type_def_desc.Parsetree.etd_name,
	  external_type_def.Parsetree.ast_loc))
;;



(* ************************************************************************ *)
(* reduced_compil_context -> Types.type_simple list -> unit                 *)
(** {b Descr} : Just an helper to print a list of types separated by commas
       and sharing a same variables mapping and an empty collection carrier
       mapping. If the list has only 1 element then it is NOT enclosed
       between parens.
       If it a several elements, then it IS enclosed between parens.
       If is has no element (degenerated case) then nothing gets printed.
       This is espercially used to print the parameters of a type
       definition.

    {b Rem} : Not exported outside this module.                             *)
(* ************************************************************************ *)
let print_types_comma_with_same_vmapping_and_empty_carrier_mapping ctx tys =
  let current_unit = ctx.rcc_current_unit in
  let out_fmter = ctx.rcc_out_fmter in
  match tys with
   | [] -> ()
   | [one] ->
       Format.fprintf out_fmter " %a"
	 (Types.pp_type_simple_to_ml ~current_unit ~reuse_mapping: true []) one
   | several ->
       (begin
       (* Enclose by parentheses and separate by commas. *)
       let rec rec_print_params = function
	 | [] -> ()
	 | [last] ->
	     Format.fprintf out_fmter "%a"
	       (Types.pp_type_simple_to_ml
		  ~current_unit ~reuse_mapping: true [])
	       last
	 | first :: rem ->
	     Format.fprintf out_fmter "%a,@ "
	       (Types.pp_type_simple_to_ml
		  ~current_unit ~reuse_mapping: true [])
	       first ;
	     rec_print_params rem in
       Format.fprintf out_fmter " (@[<1>" ;
       rec_print_params several ;
       Format.fprintf out_fmter "@])"
       end)
;;



(* ***************************************************************** *)
(* reduced_compil_context -> Parsetree.vname ->                      *)
(*   Env.TypeInformation.type_description -> unit                    *)
(** {b Descr} : Generates the OCaml code for a "regular" (i.e. non 
          "external" FoCaL type definition.

    {b Rem} : Not exported outside this module.                      *)
(* ***************************************************************** *)
let type_def_compile ctx type_def_name type_descr =
  let out_fmter = ctx.rcc_out_fmter in
  (* Type definition header. *)
  Format.fprintf out_fmter "@[<2>type" ;
  (* Get a fresh instance of the type's identity scheme. *)
  let (instanciated_body, params) =
    Types.specialize2
      type_descr.Env.TypeInformation.type_identity
      type_descr.Env.TypeInformation.type_params in
  (* Useless, but just in case.... This does not hurt ! *)
  Types.purge_type_simple_to_ml_variable_mapping () ;
  (* Now, generates the type definition's body. *)
  match type_descr.Env.TypeInformation.type_kind with
   | Env.TypeInformation.TK_abstract ->
       (* Print the parameter(s) stuff if any. *)
       print_types_comma_with_same_vmapping_and_empty_carrier_mapping
	 ctx params ;
       (* Now print the type constructor's name. *)
       Format.fprintf out_fmter " _focty_%a =@ "
	 pp_to_ocaml_vname type_def_name ;
       (* Type abbreviation: the body is the abbreviated type. *)
       Format.fprintf out_fmter "%a@] ;;@\n "
	 (Types.pp_type_simple_to_ml
	    ~current_unit: ctx.rcc_current_unit ~reuse_mapping: true [])
	 instanciated_body
   | Env.TypeInformation.TK_variant cstrs ->
       (begin
       (* To ensure variables names sharing, we will unify an instance of   *)
       (* each constructor result type (remind they have a functional type  *)
       (* whose arguments are the sum constructor's arguments and result is *)
       (* the same type that the hosting type itself) with the instance of  *)
       (* the defined type identity.                                        *)
       let sum_constructors_to_print =
	 List.map
	   (fun (sum_cstr_name, sum_cstr_arity, sum_cstr_scheme) ->
	     if sum_cstr_arity = Env.TypeInformation.CA_one then
	       (begin
	       try
		 let sum_cstr_ty = Types.specialize sum_cstr_scheme in
		 let sum_cstr_args = Types.type_variable () in
		 Types.unify
		   ~loc: Location.none ~self_manifest: None
		   (Types.type_arrow sum_cstr_args instanciated_body)
		   sum_cstr_ty ;
		 (sum_cstr_name, (Some sum_cstr_args))
	       with _ ->
		 (* Because program is already well-typed, this *)
		 (* should always succeed.                      *)
		 assert false
	       end)
	     else (sum_cstr_name, None))
	   cstrs in
       (* Print the parameter(s) stuff if any. Do it only now the  *)
       (* unifications have been done with the sum constructors to *)
       (* be sure that thanks to unifications, "sames" variables   *)
       (* will have the "same" name everywhere (i.e. in the        *)
       (* the parameters enumeration of the type and in the sum    *)
       (* constructors definitions).                               *)
       print_types_comma_with_same_vmapping_and_empty_carrier_mapping
	 ctx params ;
       (* Now print the type constructor's name. *)
       Format.fprintf out_fmter " _focty_%a =@ "
	 pp_to_ocaml_vname type_def_name ;
       (* And finally really print the constructors definitions. *)
       List.iter
	 (fun (sum_cstr_name, opt_args) ->
	   (* The sum constructor name. *)
	   Format.fprintf out_fmter "@\n| %a" pp_to_ocaml_vname sum_cstr_name ;
	   match opt_args with
	    | None -> ()
	    | Some sum_cstr_args ->
		(* The argument(s) of the constructor. *)
		Format.fprintf out_fmter " of@ (@[<1>%a@])"
		  (Types.pp_type_simple_to_ml
		     ~current_unit: ctx.rcc_current_unit
		     ~reuse_mapping: true [])
		  sum_cstr_args)
	 sum_constructors_to_print ;
       Format.fprintf out_fmter "@]@\n ;;@\n "
       end)
   | Env.TypeInformation.TK_record fields ->
       (begin
       (* Like for the sum types, we make use of unification to ensure the *)
       (* sharing of variables names. We proceed exactly the same way,     *)
       (* delaying the whole print until we unified into each record-field *)
       (* type.                                                            *)
       let record_fields_to_print =
	 List.map
	   (fun (field_name, field_mut, field_scheme) ->
	     try
	       let field_ty = Types.specialize field_scheme in
	       let field_args = Types.type_variable () in
	       Types.unify
		 ~loc: Location.none ~self_manifest: None
		 (Types.type_arrow field_args instanciated_body)
		 field_ty ;
	       (field_name, field_mut, field_args)
	     with _ ->
	       (* Because program is already well-typed, this *)
	       (* should always succeed.                      *)
	       assert false)
	   fields in
       (* Print the parameter(s) stuff if any. *)
       print_types_comma_with_same_vmapping_and_empty_carrier_mapping
	 ctx params ;
       (* Now print the type constructor's name. *)
       Format.fprintf out_fmter " _focty_%a = {@ "
	 pp_to_ocaml_vname type_def_name ;
       (* And finally really print the fields definitions. *)
       List.iter
	 (fun (field_name, field_mut, field_ty) ->
	   Format.fprintf out_fmter "@\n " ;
	   (* Generate the mutability flag. *)
	   if field_mut = Env.TypeInformation.FM_mutable then
	     Format.fprintf out_fmter "mutable " ;
	   Format.fprintf out_fmter "%s :@ %a ;"
	     field_name
	     (Types.pp_type_simple_to_ml
		~current_unit: ctx.rcc_current_unit ~reuse_mapping: true [])
	     field_ty)
	 record_fields_to_print ;
       Format.fprintf out_fmter " }@] ;;@\n "
       end)
;;



(* ********************************************************************** *)
(* reduced_compil_context -> Parsetree.external_value_def_body -> unit    *)
(** {b Descr} : Generate the OCaml source code for a FoCaL external value
              definition.

    {b Args} :
      - [ctx] : The structure recording the various utilities information
              for the code generation of the external definition.
      - [external_value_def] : The external type definition to compile
           to OCaml source code.

    {b Rem} : Not exported outside this module.                           *)
(* ********************************************************************** *)
let external_value_def_compile ctx external_value_def =
  let external_value_def_desc = external_value_def.Parsetree.ast_desc in
  let out_fmter = ctx.rcc_out_fmter in
  (* Prints the name of the defined identifier *)
  (* and prepares for its type constraint.     *)
  Format.fprintf out_fmter "@[<2>let (%a : "
    pp_to_ocaml_vname external_value_def_desc.Parsetree.evd_name ;
  let extern_val_type =
    (match external_value_def_desc.Parsetree.evd_type.Parsetree.ast_type with
     | None ->
	 (* This means that during the typechecking pass, we forgot to record *)
	 (* the infered type inside the AST node. If arises, then fix it !    *)
	 assert false
     | Some ty -> ty) in
  (* Prints the defined identifier's type constraint. Because external *)
  (* definitions are always at toplevel, no species parameter is in the *)
  (* scope, hence the collections carrier mapping is trivially empty.   *)
  Format.fprintf out_fmter "%a) =@ "
    (Types.pp_type_simple_to_ml
       ~current_unit: ctx.rcc_current_unit
       ~reuse_mapping: false []) extern_val_type ;
  (* And now, bind the FoCaL identifier to the OCaml one. *)
  try
    let (_, ocaml_binding) =
      List.find
	(function
	  | (Parsetree.EL_Caml, _) -> true
	  | (Parsetree.EL_Coq, _)
	  | ((Parsetree.EL_external _), _) -> false)
	external_value_def_desc.Parsetree.evd_body.Parsetree.ast_desc  in
    Format.fprintf out_fmter "%s@]@ ;;@\n" ocaml_binding
  with Not_found ->
    (* We didn't find any correspondance for OCaml. *)
    raise
      (No_external_value_caml_def
	 (external_value_def_desc.Parsetree.evd_name,
	  external_value_def.Parsetree.ast_loc))
;;



let external_def_compile ctx extern_def =
  match extern_def.Parsetree.ast_desc with
   | Parsetree.ED_type external_type_def ->
       external_type_def_compile ctx external_type_def
   | Parsetree.ED_value external_value_def ->
       external_value_def_compile ctx external_value_def
;;



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
	rcc_current_unit = current_unit ;
	rcc_out_fmter = out_fmter } in
      external_def_compile ctx extern_def
  | Infer.PCM_species (species_def, species_descr, dep_graph) ->
      species_compile
	~current_unit out_fmter species_def species_descr dep_graph
  | Infer.PCM_collection (_coll_def, _fields) ->
      Format.eprintf "Infer.PCM_collection expr : TODO@."
  | Infer.PCM_type (type_def_name, type_descr) ->
      (* Create the initial context for compiling the type definition. *)
      let ctx = {
	rcc_current_unit = current_unit ;
	rcc_out_fmter = out_fmter } in
      type_def_compile ctx type_def_name type_descr
  | Infer.PCM_let_def (let_def, def_schemes) ->
      (* Create the initial context for compiling the let-definition. *)
      let ctx = {
	rcc_current_unit = current_unit ;
	rcc_out_fmter = out_fmter } in
      (* We have the schemes under the hand. Then we will be able    *)
      (* to annotate the parameters of the toplevel let-bound idents *)
      (* with type constraints.                                      *)
      let bound_schemes = List.map (fun sch -> Some sch) def_schemes in
      let_def_compile ctx let_def bound_schemes ;
      Format.fprintf out_fmter "@\n;;@\n"
  | Infer.PCM_theorem _ -> ()  (* Theorems do not lead to OCaml code. *)
  | Infer.PCM_expr expr ->
      let ctx = {
	rcc_current_unit = current_unit ;
	rcc_out_fmter = out_fmter
      } in
      generate_expr ctx expr ;
      (* Generate the final double-semis. *)
      Format.fprintf out_fmter "@ ;;@\n"
;;



let root_compile ~current_unit ~out_file_name stuff =
  let out_hd = open_out_bin out_file_name in
  let out_fmter = Format.formatter_of_out_channel out_hd in
  try
    List.iter (toplevel_compile ~current_unit out_fmter) stuff ;
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
