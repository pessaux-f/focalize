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


(* $Id: core_ml_generation.ml,v 1.8 2007-09-25 11:15:59 pessaux Exp $ *)



(* ************************************************************************ *)
(** {b Descr} : Exception raised when an external value definition does not
              not provides any correspondance with OCaml.

    {b Rem} : Exported outside this module.                                 *)
(* ************************************************************************ *)
exception No_external_value_caml_def of (Parsetree.vname * Location.t) ;;



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



type species_compil_context = {
  (** The name of the currently analysed compilation unit. *)
  scc_current_unit : Types.fname ;
  (** The name of the current species. *)
  scc_current_species : Types.species_name ;
  (** The nodes of the current species's dependency graph. *)
  scc_dependency_graph_nodes : Dep_analysis.name_node list ;
  (** The current correspondance between collection types and type variable
      names representing the carrier of a species type in the OCaml code. *)
  scc_collections_carrier_mapping : (Types.type_collection * string) list ;
  (** The current output formatter where to send the generated code. *)
  scc_out_fmter : Format.formatter
} ;;



type expr_compil_context = {
  (** The name of the currently analysed compilation unit. *)
  ecc_current_unit : Types.fname ;
   (** The current output formatter where to send the generated code. *)
  ecc_out_fmter : Format.formatter
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
		 "(* Carrier's structure explicitely given by \"rep\". *)@\n" ;
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
		    ~reuse_mapping: false
		    ctx.scc_collections_carrier_mapping) ty
	       end)
	     else rec_search q
	 | _ -> rec_search q
	end) in
  rec_search fields
;;



(* ************************************************************************** *)
(* species_compil_context -> Parsetree.species_def ->                         *)
(*   Env.TypeInformation.species_description -> unit                          *)
(** {b Descr} : Generate the record type representing a species. This type
              contains a field per method. This type is named "me_as_species"
              to reflect the point that it represents the ML structure
              representing the FoCaL species.
              Depending on whether the species has parameters, this record
              type also has parameters. In any case, it at least has a
              parameter representing "self as it will be once instanciated"
              once "we" (i.e. the species) will be really living as a
              collection.

    {b Rem} : Not exported outside this module.                               *)
(* ************************************************************************** *)
let generate_record_type ctx species_def species_descr =
  let out_fmter = ctx.scc_out_fmter in
  let collections_carrier_mapping = ctx.scc_collections_carrier_mapping in
  (* First, check if "rep" is defined. If so, then generate  *)
  (* the type constraint reflecting its effective structure. *)
  generate_rep_constraint_in_record_type
    ctx species_descr.Env.TypeInformation.spe_sig_methods ;
  let field_prefix =
    String.lowercase species_def.Parsetree.ast_desc.Parsetree.sd_name in
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
            Format.fprintf out_fmter "(* From species %s. *)@\n" from ;
	    (* Since we are printing a whole type scheme, it is stand-alone *)
            (* and we don't need to keep name sharing with anythin else.    *)
	    Format.fprintf out_fmter "@[<2>%s_%a : %a ;@]@\n"
	      field_prefix pp_to_ocaml_vname n
	      (Types.pp_type_simple_to_ml
		 ~reuse_mapping: false collections_carrier_mapping) ty
	    end)
	  end)
      | Env.TypeInformation.SF_let_rec l ->
	  List.iter
	    (fun (from, n, _, sch, _) ->
	      let ty = Types.specialize sch in
	      Format.fprintf out_fmter "(* From species %s. *)@\n" from ;
	      (* Since we are printing a whole type scheme, it is stand-alone *)
              (* and we don't need to keep name sharing with anythin else.    *)
	      Format.fprintf out_fmter "%s_%a : %a ;@\n"
		field_prefix pp_to_ocaml_vname n
		(Types.pp_type_simple_to_ml
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
(* expr_compil_context -> Parsetree.ident -> unit                   *)
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
       Format.fprintf ctx.ecc_out_fmter "%a" pp_to_ocaml_vname vname
   | Parsetree. I_global (fname_opt, vname) ->
       (begin
       match fname_opt with
	| None ->
	    (* Thanks to the scoping pass, [I_global] with a [None] module *)
            (* name are toplevel definitions of the current compilation    *)
            (* unit. Then hence can be straightforwardly called in the     *)
            (* OCaml code.                                                 *)
	    Format.fprintf ctx.ecc_out_fmter "%a" pp_to_ocaml_vname vname
	| Some mod_name ->
	    (* Call the OCaml corresponding identifier in the corresponding *)
            (* module (i.e. the capitalized [mod_name]). If the module is   *)
	    (* the currently compiled one, then do not qualify the          *)
	    (* identifier.                                                  *)
	    if mod_name <> ctx.ecc_current_unit then
	      Format.fprintf ctx.ecc_out_fmter "%s.%a"
		(String.capitalize mod_name) pp_to_ocaml_vname vname
	    else
	      Format.fprintf ctx.ecc_out_fmter "%a" pp_to_ocaml_vname vname
       end)
   | Parsetree.I_method (coll_name_opt, vname) ->
       (begin
       match coll_name_opt with
	| None ->
	    (* Method call from the current species. This corresponds to *)
	    (* a call to the corresponding lambda-lifted method that is  *)
	    (* represented as an extra parameter of the OCaml function.  *)
	    Format.fprintf ctx.ecc_out_fmter "abst_%a" pp_to_ocaml_vname vname
	| Some coll_name ->
	    (* [Unsure]. What should happen in term of generated code and     *)
            (* what is the meaning of a method call from a toplevel defined   *)
            (* collection ????????                                            *)
	    (* Method call from a species that is not the current. May be *)
	    (* either a paramater or a toplevel defined collection. To    *)
	    (* retrieve the related method name we build it the same way  *)
            (* we built it while generating the extra OCaml function's    *)
            (* parameters due to depdencencies coming from the species    *)
            (* parameter. I.e: "_p_", followed by the species parameter   *)
            (* name, followed by "_", followed by the method's name.      *)
            let prefix = "_p_" ^ (String.lowercase coll_name) ^ "_" in
            Format.fprintf ctx.ecc_out_fmter
	      "%s%a" prefix pp_to_ocaml_vname vname
       end)
;;



(* ******************************************************************** *)
(* expr_compil_context ->  Parsetree.constructor_expr -> unit           *)
(** {b Descr} : Generate the OCaml code from a FoCaL [constructor_expr]
              in the context of method generator generation.

    {b Rem} : Not exported outside this module.                         *)
(* ******************************************************************** *)
let generate_constructor_expr_for_method_generator ctx cstr_expr =
  match cstr_expr.Parsetree.ast_desc with
   | Parsetree.CE (fname_opt, name) ->
       (begin
       match fname_opt with
	| None -> Format.fprintf ctx.ecc_out_fmter "%a" pp_to_ocaml_vname name
	| Some fname ->
	    (* If the constructor belongs to the current      *)
	    (* compilation unit then one must not qualify it. *)
	    if fname <> ctx.ecc_current_unit then
	      Format.fprintf ctx.ecc_out_fmter "%s.%a"
		(String.capitalize fname) pp_to_ocaml_vname name
	    else
	      Format.fprintf ctx.ecc_out_fmter "%a" pp_to_ocaml_vname name
       end)
;;



let generate_pattern ctx pattern =
  let out_fmter = ctx.ecc_out_fmter in
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
     | Parsetree.P_app (ident, pats) ->
	 (begin
	 generate_ident_for_method_generator ctx ident ;
	 (* Discriminate on the umber of arguments *)
         (* to know if parens are needed.          *)
	 match pats with
	  | [] -> ()
	  | [one] -> rec_gen_pat one
	  | _ ->
	      Format.fprintf out_fmter " (" ;
	      rec_generate_pats_list pats ;
	      Format.fprintf out_fmter ")"
	 end)
     | Parsetree.P_record labs_pats ->
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
  let out_fmter = ctx.ecc_out_fmter in
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



(* ********************************************************************* *)
(* expr_compil_context -> Parsetree.let_def -> Types.type_scheme list -> *)
(*   unit                                                                *)
(** {b Desrc} : Generates the OCaml code for a FoCaL "let"-definition.

    {b Args} :
      - [out_fmter] : The out channel where to generate the OCaml source
                    code.
      - [let_def] : The [Parsetree.let_def] structure representing the
                  "let-definition" for which th generate the OCaml
                  source code.
      - [bound_schemes] : The list of types schemes of the identifiers
                        bound to the "let-definition" (i.e. several if
                        the definition is a "rec", hence binds sevzral
                        identifiers). In effect, because we do not have
                        directly inside the [Parsetree.let_def] these
                        schemes, in order to be able to generate the
                        type constraints of each components of the
                        "let-definition", we must take these schemes
                        aside.
                        It is sometimes impossible yet to have this
                        information. In this case, no type constraint
                        will be added to the parameter of the bound
                        identifiers.

    {b Rem} : Not exported outside this module.                          *)
(* ********************************************************************* *)
and let_def_compile ctx let_def opt_bound_schemes =
  let out_fmter = ctx.ecc_out_fmter in
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
(* expr_compil_context -> Parsetree.expr -> unit                 *)
(** {b Descr} : Generate the OCaml code from a FoCaL expression.

    {b Rem} : Not exported outside this module.                  *)
(* ************************************************************* *)
and generate_expr ctx initial_expression =
  let out_fmter = ctx.ecc_out_fmter in
  let rec rec_generate expr =
    (* Generate the source code for the expression. *)
    match expr.Parsetree.ast_desc with
     | Parsetree.E_self ->
	 Format.eprintf "generate_expr E_self TODO@."
     | Parsetree.E_const cst -> generate_constant out_fmter cst
     | Parsetree.E_fun (args_names, body) ->
	 List.iter
	   (fun n ->
	     Format.fprintf out_fmter "@[<2>fun@ %a@ ->@ " pp_to_ocaml_vname n)
	   args_names ;
	 rec_generate body ;
         Format.fprintf out_fmter "@]"
     | Parsetree.E_var ident ->
	 generate_ident_for_method_generator ctx ident
     | Parsetree.E_app (expr, exprs) ->
	 Format.fprintf out_fmter "@[<2>(" ;
	 rec_generate expr ;
	 Format.fprintf out_fmter "@ " ;
	 rec_generate_exprs_list ~comma: false exprs ;
	 Format.fprintf out_fmter ")@]"
     | Parsetree.E_constr (cstr_expr, exprs) ->
	 (begin
	 generate_constructor_expr_for_method_generator ctx cstr_expr ;
	 match exprs with
	  | [] -> ()
	  | [one] -> rec_generate one   (* Not a tuple => no parenthesis. *)
	  | _ -> 
	      (* If several arguments, enclose by parens to make a tuple. *)
	      Format.fprintf out_fmter "@[<1>(" ;
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
	 Format.eprintf "generate_expr E_record TODO@."
     | Parsetree.E_record_access (expr, label_name) ->
	 Format.eprintf "generate_expr E_record_access TODO@."
     | Parsetree.E_record_with (expr, labs_exprs) ->
	 Format.eprintf "generate_expr E_record_with TODO@."
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
	 Format.eprintf "generate_expr E_external TODO@."
     | Parsetree.E_paren e -> rec_generate e



  and rec_generate_exprs_list ~comma = function
    | [] -> ()
    | [last] -> rec_generate last 
    | h :: q ->
	rec_generate h ;
	if comma then Format.fprintf out_fmter ",@ "
	else Format.fprintf out_fmter "@ " ;
	rec_generate_exprs_list ~comma q in
  (* ********************** *)
  (* Now, let's do the job. *)
  rec_generate initial_expression
;;



(* *********************************************************************** *)
(* species_compil_context -> Types.collection_name list ->                 *)
(*   Env.TypeInformation.species_field -> unit                             *)
(** {b Desc} : Generates the OCaml code for ONE method field (i.e. for one
             let-bound construct or for one item of let-rec-bound items.

    {b Args} :
      - [ctx] : The species-compilation-context merging the various
          stuffs sometimes needed during the compilation pass.
      - [species_parameters_names] : The list of the names of parameters
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
    (* First of all, only methods defined in the current species must *)
    (* be generated. Inherited methods ARE NOT generated again !      *)
    if from = ctx.scc_current_species then
      (begin
      (* Just a bit of debug. *)
      if Configuration.get_verbose () then
	Format.eprintf "Generating OCaml code for field '%a'.@."
	  pp_to_ocaml_vname name ;
      (* Now, get all the methods we directly decl-depend on. They will *)
      (* lead each to an extra parameter of the final OCaml function    *)
      (* (lambda-lifing).                                               *)
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
      (* Start the OCaml function definition. *)
      if is_first then
	Format.fprintf out_fmter "@[<2>let %a" pp_to_ocaml_vname name
      else
	Format.fprintf out_fmter "@[<2>and %a" pp_to_ocaml_vname name ;
      (* First, abstract according to the species's parameters the current  *)
      (* method depends on. Do not [fold_left] to keep the extra parameters *)
      (* in the same order than the species parameters order. I.e. for a    *)
      (* species [Foo (A ..., B) ...] we want to have the extra parameters  *)
      (* due to lambda-lifting in the OCaml function ordered such as those  *)
      (* coming from [A] are first, then come those from [B].               *)
      let dependencies_from_params =
	List.fold_right
	  (fun species_param_name accu ->
	    let meths_from_param =
	      Param_dep_analysis.param_deps_expr species_param_name body in
	    (* Return a couple binding the species parameter's name with the *)
	    (* methods of it we found as required for the current method.    *)
	    (species_param_name, meths_from_param) :: accu)
	  species_parameters_names
	  [] in
      List.iter
	(fun (species_param_name, meths) ->
	  (* Each abstracted method will be named like "_p_", followed by *)
	  (* the species parameter name, followed by "_", followed by the *)
          (* method's name.                                               *)
	  let prefix = "_p_" ^ (String.lowercase species_param_name) ^ "_" in
	  Dep_analysis.VnameSet.iter
	    (fun meth ->
	      Format.fprintf out_fmter "@ %s%a" prefix pp_to_ocaml_vname meth)
	    meths)
	dependencies_from_params ;
      (* Now, lambda-lift all the dependencies (i.e function we depend on). *)
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
	ecc_current_unit = ctx.scc_current_unit ;
	ecc_out_fmter = out_fmter } in
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
      end) in

  (* ****************************** *)
  (* Now, really process the field. *)
  match field with
   | Env.TypeInformation.SF_sig (_, name, _) ->
       (* Only declared, hence, no code to generate yet ! *)
       if Configuration.get_verbose () then
	 Format.eprintf "OCaml code for signature '%a' leads to void code.@."
	   pp_to_ocaml_vname name
   | Env.TypeInformation.SF_let (from, name, params, scheme, body) ->
       generate_one_binding
	 ~is_first: true (from, name, params, (Some scheme), body)
   | Env.TypeInformation.SF_let_rec l ->
       (begin
       match l with
	| [] ->
	    (* A "let", then a fortiori "let rec" construct *)
	    (* must at least bind one identifier !          *)
	    assert false
	| (from, name, params, scheme, body) :: q ->
	    generate_one_binding
	      ~is_first: true (from, name, params, (Some scheme), body) ;
	    List.iter
	      (fun (_from, _name, _params, _scheme, _body) ->
		generate_one_binding
		  ~is_first: false
		  (_from, _name, _params, (Some _scheme), _body))
	      q
       end)
   | Env.TypeInformation.SF_theorem (_, name, _, _, _)
   | Env.TypeInformation.SF_property (_, name, _, _) ->
       (* Properties and theorems are purely  *)
       (* discarded in the Ocaml translation. *)
       if Configuration.get_verbose () then
	 Format.eprintf
	   "OCaml code for theorem/property '%a' leads to void code.@."
	   pp_to_ocaml_vname name
;;



let species_compile ~current_unit out_fmter species_def species_descr
    dep_graph =
  let species_def_desc = species_def.Parsetree.ast_desc in
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf "Generating OCaml code for species %s@."
      species_def_desc.Parsetree.sd_name ;
  (* Start the module encapsulating the species representation. *)
  Format.fprintf out_fmter "@[<2>module %s =@\nstruct@\n"
    species_def_desc.Parsetree.sd_name ;
  (* Now, establish the mapping between collections available *)
  (* and the type variable names representing their carrier.  *)
  let collections_carrier_mapping =
    build_collections_carrier_mapping ~current_unit species_descr in
  (* Create the initial compilation context for this species. *)
  let ctx = {
    scc_current_unit = current_unit ;
    scc_current_species = species_def_desc.Parsetree.sd_name ;
    scc_dependency_graph_nodes = dep_graph ;
    scc_collections_carrier_mapping = collections_carrier_mapping ;
    scc_out_fmter = out_fmter } in
  (* The record type representing the species' type. *)
  generate_record_type ctx species_def species_descr ;
  (* Compute the list of names of parameters of the species. This  *)
  (* will be use to ompute for each method the set of methods from *)
  (* the parameters the method depends on.                         *)
  let species_parameters_names =
    List.map
      (function
	| Env.TypeInformation.SPAR_in (n, _)
	| Env.TypeInformation.SPAR_is (n, _) -> Parsetree_utils.name_of_vname n)
      species_descr.Env.TypeInformation.spe_sig_params in
  (* Now, the methods of the species. *)
  List.iter
    (generate_methods ctx species_parameters_names)
    species_descr.Env.TypeInformation.spe_sig_methods ;
  Format.fprintf out_fmter "end ;;@]@\n@."
;;



(* ************************************************************************* *)
(* Format.formatter -> Parsetree.external_type_def_body -> unit              *)
(** {b Descr} : Generates the OCaml code to bind a FoCaL type onto an OCaml
        existing type. If the FoCaL type name is the same than the OCaml
        one, then we silently ignore the type definition to avoid OCaml type
        definitions like [type int = int] that would lead to a cyclic type
        abbreviation and would be rejected by OCaml.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let external_type_def_compile out_fmter external_type_def =
  Format.eprintf "external_type_def_compile TODO@." ;;



let external_value_def_compile out_fmter external_value_def =
  let external_value_def_desc = external_value_def.Parsetree.ast_desc in
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
    (Types.pp_type_simple_to_ml ~reuse_mapping: false []) extern_val_type ;
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



let external_def_compile out_fmter extern_def =
  match extern_def.Parsetree.ast_desc with
   | Parsetree.ED_type external_type_def ->
       external_type_def_compile out_fmter external_type_def
   | Parsetree.ED_value external_value_def ->
       external_value_def_compile out_fmter external_value_def
;;



let toplevel_compile ~current_unit out_fmter global_env = function
  | Infer.PCM_no_matter -> ()
  | Infer.PCM_external extern_def -> external_def_compile out_fmter extern_def
  | Infer.PCM_species (species_def, species_descr, dep_graph) ->
      species_compile
	~current_unit out_fmter species_def species_descr dep_graph
  | Infer.PCM_collection (coll_def, fields) ->
      Format.eprintf "Infer.PCM_collection expr : TODO@."
  | Infer.PCM_type -> Format.eprintf "Infer.PCM_type expr : TODO@."
  | Infer.PCM_let_def (let_def, def_schemes) ->
      (* Create the initial context for compiling the let-definition. *)
      let ctx = {
	ecc_current_unit = current_unit ;
	ecc_out_fmter = out_fmter } in
      (* We have the schemes under the hand. Then we will be able    *)
      (* to annotate the parameters of the toplevel let-bound idents *)
      (* with type constraints.                                      *)
      let bound_schemes = List.map (fun sch -> Some sch) def_schemes in
      let_def_compile ctx let_def bound_schemes ;
      Format.fprintf out_fmter "@\n;;@\n"
  | Infer.PCM_theorem theorem_def ->
      Format.eprintf "Infer.PCM_theorem expr : TODO@."
  | Infer.PCM_expr expr -> Format.eprintf "Infer.PCM_expr expr : TODO@."
;;



let root_compile ~current_unit ~out_file_name global_env stuff =
  let out_hd = open_out_bin out_file_name in
  let out_fmter = Format.formatter_of_out_channel out_hd in
  try
    List.iter (toplevel_compile ~current_unit out_fmter global_env) stuff ;
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
