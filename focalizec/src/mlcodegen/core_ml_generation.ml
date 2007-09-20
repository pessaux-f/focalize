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


(* $Id: core_ml_generation.ml,v 1.7 2007-09-20 10:38:19 pessaux Exp $ *)


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
  current_unit : Types.fname ;
  (** The name of the current species. *)
  current_species : Types.species_name ;
  (** The nodes of the current species's dependency graph. *)
  dependency_graph_nodes : Dep_analysis.name_node list ;
  (** The current correspondance between collection types and type variable
      names representing the carrier of a species type in the OCaml code. *)
  collections_carrier_mapping : (Types.type_collection * string) list ;
  (** The current output formatter where to send the generated code. *)
  out_fmter : Format.formatter
} ;;



(* *********************************************************************** *)
(* current_unit: Types.fname -> Env.TypeInformation.species_description -> *)
(*   (Types.type_collection * string) list                                 *)
(** {b Descr} : Create the correspondance between the collection type of
              the species definition parameters and the type variables
              names to be used later during the OCaml translation.

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



(* ******************************************************************** *)
(* ('a * string) list -> unit                                           *)
(** {b Descr} : Helper to print the list of known variables names in
              a collections carrier mapping as a legal OCaml list of
              type parameters, i.e, comma separated except for the last
              one.

    [b Rem} : Not exported outside this module.                         *)
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
	     (* Check if the si is "rep". *)
	     if (Parsetree_utils.name_of_vname n) = "rep" then
	       (begin
	       Format.fprintf ctx.out_fmter "@[<2>type " ;
	       (* First, output the type parameters if some, and enclose *)
	       (* them by perentheses if there are several.              *)
	       (match ctx.collections_carrier_mapping with
		| [] -> ()
		| [ (_, only_var_name) ] ->
		    Format.fprintf ctx.out_fmter "%s@ " only_var_name
		| _ ->
		    (* More than one, then sourround by parentheses. *)
		    Format.fprintf ctx.out_fmter "@[<1>(" ;
		    (* Print the variables names... *)
		    print_comma_separated_vars_list_from_mapping
		      ctx.out_fmter ctx.collections_carrier_mapping ;
		    Format.fprintf ctx.out_fmter ")@]@ ") ;
	       (* Now, output the type's name and body. *)
	       let ty = Types.specialize sch in
	       Format.fprintf ctx.out_fmter
		 "@[<2>me_as_carrier =@ %a@]@\n"
		 (Types.pp_type_simple_to_ml
		    ctx.collections_carrier_mapping) ty
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
  let out_fmter = ctx.out_fmter in
  let collections_carrier_mapping = ctx.collections_carrier_mapping in
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
      | Env.TypeInformation.SF_sig (_, n, sch)
      | Env.TypeInformation.SF_let (_, n, sch, _) ->
	  (begin
	  (* Skip "rep", because it is a bit different and processed above *)
	  (* c.f. function [generate_rep_constraint_in_record_type].       *)
	  if (Parsetree_utils.name_of_vname n) <> "rep" then
	    (begin
	    let ty = Types.specialize sch in
	    Format.fprintf out_fmter "%s_%a :@ %a ;@\n"
	      field_prefix pp_to_ocaml_vname n
	      (Types.pp_type_simple_to_ml collections_carrier_mapping) ty
	    end)
	  end)
      | Env.TypeInformation.SF_let_rec l ->
	  List.iter
	    (fun (_, n, sch, _) ->
	      let ty = Types.specialize sch in
	      Format.fprintf out_fmter "%s_%a :@ %a ;@\n"
		field_prefix pp_to_ocaml_vname n
		(Types.pp_type_simple_to_ml collections_carrier_mapping) ty)
	    l
      | Env.TypeInformation.SF_theorem  (_, _, _, _, _)
      | Env.TypeInformation.SF_property (_, _, _, _) ->
	  (* Properties and theorems are purely  *)
          (* discarded in the Ocaml translation. *)
	  ())
    species_descr.Env.TypeInformation.spe_sig_methods ;
  Format.fprintf out_fmter "@]}@\n"
;;




let generate_constant out_fmter constant =
  match constant.Parsetree.ast_desc with
   | Parsetree.C_int i -> Format.fprintf out_fmter "%s" i
   | Parsetree.C_float fl -> Format.fprintf out_fmter "%s" fl
   | Parsetree.C_bool b -> Format.fprintf out_fmter "%s" b
   | Parsetree.C_string s -> Format.fprintf out_fmter "\"%s\"" s
   | Parsetree.C_char c -> Format.fprintf out_fmter "'%c'" c
;;



let generate_expr out_fmter collections_carrier_mapping initial_expression =
  let rec rec_generate expr =
    match expr.Parsetree.ast_desc with
     | Parsetree.E_self ->
	 Format.eprintf "generate_expr E_self TODO@."
     | Parsetree.E_const cst -> generate_constant out_fmter cst
     | Parsetree.E_fun (args_names, body) ->
	 Format.eprintf "generate_expr E_fun TODO@."
     | Parsetree.E_var ident ->
	 Format.eprintf "generate_expr E_var TODO@."
     | Parsetree.E_app (expr, exprs) ->
	 Format.fprintf out_fmter "@[<1>(" ;
	 rec_generate expr ;
	 rec_generate_exprs_list ~comma: false exprs ;
	 Format.fprintf out_fmter ")@]"
     | Parsetree.E_constr (cstr_expr, exprs) ->
	 Format.eprintf "generate_expr E_constr TODO@."
     | Parsetree.E_match (expr, pats_exprs) ->
	 Format.eprintf "generate_expr E_match TODO@."
     | Parsetree.E_if (expr1, expr2, expr3) ->
	 Format.fprintf out_fmter "if@ " ;
	 rec_generate expr1 ;
	 Format.fprintf out_fmter "@ then@ " ;
	 rec_generate expr2 ;
         Format.fprintf out_fmter "@ else@ " ;
	 rec_generate expr3 ;
     | Parsetree.E_let (let_def, expr) ->
	 Format.eprintf "generate_expr E_let TODO@."
     | Parsetree.E_record labs_exprs ->
	 Format.eprintf "generate_expr E_record TODO@."
     | Parsetree.E_record_access (expr, label_name) ->
	 Format.eprintf "generate_expr E_record_access TODO@."
     | Parsetree.E_record_with (expr, labs_exprs) ->
	 Format.eprintf "generate_expr E_record_with TODO@."
     | Parsetree.E_tuple exprs ->
	 Format.fprintf out_fmter "@[<1>(" ;
	 rec_generate_exprs_list ~comma: true exprs ;
	 Format.fprintf out_fmter ")@]"
     | Parsetree.E_external external_expr ->
	 Format.eprintf "generate_expr E_external TODO@."
     | Parsetree.E_paren e ->
	 Format.fprintf out_fmter "@[<1>(" ;
	 rec_generate e ;
	 Format.fprintf out_fmter ")@]"


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



let generate_methods ctx field =
  let out_fmter = ctx.out_fmter in
  (* Local function to handle one binding. Will be directly used in case *)
  (* of [SF_let] field, or be iterated in case of [SF_let_rec] field.    *)
  (* The [~is_first] boolean tells whether we must start de function     *)
  (* binding with "let" or "and".                                        *)
  let generate_one_binding ~is_first (from, name, scheme, body) =
    (* Now, get all the methods we directly decl-depend on. They will *)
    (* lead each to an extra parameter of the final OCaml function    *)
    (* (lambda-lifing).                                               *)
    let decl_children =
      (try
	let my_node =
	  List.find
	    (fun { Dep_analysis.nn_name = n } -> n = name)
	    ctx.dependency_graph_nodes in
	(* Only keep "decl-dependencies". *)
	List.filter
	  (function
	    | (_, Dep_analysis.DK_decl) -> true
	    | (_, Dep_analysis.DK_def) -> false)
	  my_node.Dep_analysis.nn_children
      with Not_found -> []  (* No children at all. *)) in
    (* Start the OCaml function definition. *)
    if is_first then
      Format.fprintf out_fmter "let %a " pp_to_ocaml_vname name
    else
      Format.fprintf out_fmter "and %a " pp_to_ocaml_vname name ;
    (* Now, lambda-lift all the dependencies. *)
    List.iter
      (fun ({ Dep_analysis.nn_name = dep_name }, _) ->
	Format.fprintf out_fmter "abst_%a " pp_to_ocaml_vname dep_name)
      decl_children ;
    (* The "=" sign ending the OCaml function's "header". *)
    Format.fprintf out_fmter "=@ " ;
    (* Add the real arguments of the method if some, with their *)
    (* type constraints and generate the body's code.           *)
    generate_expr out_fmter ctx.collections_carrier_mapping body ;
    (* Done... Then, final carriage return. *)
    Format.fprintf out_fmter "@."
    in
  (* ****************************** *)
  (* Now, really process the field. *)
  match field with
   | Env.TypeInformation.SF_sig (_, _, _) ->
       (* Only declared, hence, no code to generate yet ! *)
       ()
   | Env.TypeInformation.SF_let (from, name, scheme, body) ->
       generate_one_binding ~is_first: true (from, name, scheme, body)
   | Env.TypeInformation.SF_let_rec l ->
       (begin
       match l with
	| [] ->
	    (* A "let", then a fortiori "let rec" construct *)
	    (* must at least bind one identifier !          *)
	    assert false
	| h :: q ->
	    generate_one_binding ~is_first: true h ;
	    List.iter (generate_one_binding ~is_first: false) q
       end)
   | Env.TypeInformation.SF_theorem (_, _, _, _, _)
   | Env.TypeInformation.SF_property (_, _, _, _) ->
       (* Properties and theorems are purely  *)
       (* discarded in the Ocaml translation. *)
       ()
;;



let species_compile ~current_unit out_fmter species_def species_descr
    dep_graph =
  let species_def_desc = species_def.Parsetree.ast_desc in
  (* Start the module encapsulating the species representation. *)
  Format.fprintf out_fmter "@[<2>module %s =@\nstruct@\n"
    species_def_desc.Parsetree.sd_name ;
  (* Now, establish the mapping between collections available *)
  (* and the type variable names representing their carrier.  *)
  let collections_carrier_mapping =
    build_collections_carrier_mapping ~current_unit species_descr in
  (* Create the initial compilation context for this species. *)
  let ctx = {
    current_unit = current_unit ;
    current_species = species_def_desc.Parsetree.sd_name ;
    dependency_graph_nodes = dep_graph ;
    collections_carrier_mapping = collections_carrier_mapping ;
    out_fmter = out_fmter } in
  (* The record type representing the species' type. *)
  generate_record_type ctx species_def species_descr ;
  (* Now, the methods of the species. *)
  List.iter
    (generate_methods ctx) species_descr.Env.TypeInformation.spe_sig_methods ;
  Format.fprintf out_fmter "@\nend ;;@]@\n@."
;;



let toplevel_compile ~current_unit out_fmter global_env = function
  | Infer.PCM_no_matter -> ()
  | Infer.PCM_external -> Format.eprintf "Infer.PCM_external expr : TODO@."
  | Infer.PCM_species (species_def, species_descr, dep_graph) ->
      species_compile
	~current_unit out_fmter species_def species_descr dep_graph
  | Infer.PCM_collection (coll_def, fields) ->
      Format.eprintf "Infer.PCM_collection expr : TODO@."
  | Infer.PCM_type -> Format.eprintf "Infer.PCM_type expr : TODO@."
  | Infer.PCM_let_def let_def ->
      Format.eprintf "Infer.PCM_let_def expr : TODO@."
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
