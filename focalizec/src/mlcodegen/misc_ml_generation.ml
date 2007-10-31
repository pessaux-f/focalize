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

(* $Id: misc_ml_generation.ml,v 1.6 2007-10-31 15:33:37 pessaux Exp $ *)


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



let pp_to_ocaml_label_ident ppf lab_ident =
  match lab_ident.Parsetree.ast_desc with
   | Parsetree.LI qual_name ->
       let vname =
	 (match qual_name with
	  | Parsetree.Vname n -> n
	  | Parsetree.Qualified (modname, n) ->
	      Format.fprintf ppf "%s." (String.capitalize modname) ;
	      n) in
       Format.fprintf ppf "%a" pp_to_ocaml_vname vname
;;



let ocaml_vname_as_string = function
  | Parsetree.Vlident s
  | Parsetree.Vuident s
  | Parsetree.Vqident s -> s
  | Parsetree.Vpident s
  | Parsetree.Viident s -> parse_operator_string s
;;



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

    {b Rem} Exported outside this module.                                *)
(* ********************************************************************* *)
type reduced_compil_context = {
  (** The name of the currently analysed compilation unit. *)
  rcc_current_unit : Types.fname ;
  (** The list of the current species species parameters if we are in the
      scope of a species and if it has some parameters. *)
  rcc_species_parameters_names : Parsetree.vname list ;
  (** The current correspondance between method names of Self and their
      extra parameters they must be applied to because of the lambda-lifting
      process. This info is used when generating the OCaml code of a
      method, hence it is only relevant in case of recursive methods to know
      in their own body what they must be applied to in addition to their
      explicit arguments (those given by the FoCaL programmer). *)
  rcc_lambda_lift_params_mapping : (Parsetree.vname * (string list)) list ;
  (** The current output formatter where to send the generated code. *)
  rcc_out_fmter : Format.formatter
} ;;



(* ************************************************************************ *)
(* Types.type_scheme option -> Parsetree.vname list ->                      *)
(*  (Parsetree.vname * Types.type_simple option) list                       *)
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

    {b Rem} : Exported outside this module.                                 *)
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
	       (* We split the functionnal type. First, get argument type. *)
 	       let h_type = Types.extract_fun_ty_arg ty in
	       (* Next, get the result type. *)
	       let q_type = Types.extract_fun_ty_result ty in
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

