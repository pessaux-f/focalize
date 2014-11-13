(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            Pierre Weis                                                     *)
(*            Damien Doligez                                                  *)
(*            François Pessaux                                                *)
(*                                                                            *)
(*               LIP6  --  INRIA Rocquencourt -- ENSTA ParisTech              *)
(*                                                                            *)
(*  Copyright 2007 - ... LIP6 and INRIA                                       *)
(*            2012 - ... ENSTA ParisTech                                      *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)

let name_of_vname = function
  | Parsetree.Vlident s
  | Parsetree.Vuident s
  | Parsetree.Vpident s
  | Parsetree.Viident s
  | Parsetree.Vqident s -> s
;;



(* ************************************************************************* *)
(** {b Descr} : Module stuff to create sets of [Parsetree.vname]s with their
    type.
    This will serves to make sets of methods [vname]s in order to represent
    dependencies of methods of "Self" on methods of "Self".
    We keep the name's scheme in order to be able to annotate it if required
    during Coq code generation.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
module SelfDepMod = struct
  type t = (Parsetree.vname * Types.type_simple)
  let compare (n1, _) (n2, _) = compare n1 n2
end ;;
module SelfDepSet = Set.Make (SelfDepMod) ;;



(* ************************************************************************ *)
(* {b Descr} Allows to embedd in sets of dependencies on species parameters
   either the type scheme of the method if it is computational or the
   logical expression if it it a logical property. This comes from the fact
   that "type" of a computational method is its ML-like type but for
   logical methods (theorem/property), it is its logical property.

   {b Rem} : Exported outside this module.                                  *)
(* ************************************************************************ *)
type dependency_elem_type_kind =
  | DETK_computational of Types.type_simple
  | DETK_logical of Parsetree.logical_expr
;;


(* ************************************************************************* *)
(** {b Descr} : Module stuff to create sets of [Parsetree.vname]s with either
    their ML-like type or their logical expression.
    This will serves to make sets of methods [vname]s in order to represent
    dependencies of methods of "Self" on methods of species parameters.
    We keep the name's scheme or logical expression (depending on wether the
    method we depend on is computational or logical) in order to be able to
    annotate it or print its body (for logical expressions) if required
    during Coq code generation.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
module ParamDepMod = struct
  type t = (Parsetree.vname * dependency_elem_type_kind)
  let compare (n1, _) (n2, _) = compare n1 n2
end ;;
module ParamDepSet = Set.Make (ParamDepMod) ;;



let list_to_param_dep_set l =
  let rec rec_to_set accu = function
    | [] -> accu
    | h :: q ->
        rec_to_set (ParamDepSet.add h accu) q in
  rec_to_set ParamDepSet.empty l
;;



let param_dep_set_find predicate set =
  let found = ref None in
  try
    ParamDepSet.iter
      (fun elem ->
        if predicate elem then
          begin
          found := Some elem ;
          raise Exit
          end)
      set ;
      raise Not_found
  with Exit ->
    match !found with
     | None -> assert false
     | Some elem -> elem
;;



module VnameMod = struct
  type t = Parsetree.vname
  let compare = compare
end ;;
module VnameSet = Set.Make (VnameMod) ;;



module Qualified_speciesMod = struct
  type t = Parsetree.qualified_species
  let compare = compare
end;;
module Qualified_speciesMap = Map.Make(Qualified_speciesMod) ;;



(* ************************************************************************* *)
(* Parsetree.pattern -> Parsetree.vname list                                 *)
(** {b Descr} : Returns the list of local identifiers induced by a pattern.
        In other words, the list of identifiers a pattern binds and that
        in effect now are local idents in the expression using this pattern.

    {b Rem} : Exported outside this module.                                  *)
(* ************************************************************************* *)
let rec get_local_idents_from_pattern pat =
  match pat.Parsetree.ast_desc with
   | Parsetree.P_const _
   | Parsetree.P_wild -> []
   | Parsetree.P_var v -> [v]
   | Parsetree.P_as (p, v) -> v :: (get_local_idents_from_pattern p)
   | Parsetree.P_tuple ps
   | Parsetree.P_constr (_, ps) ->
       List.flatten (List.map get_local_idents_from_pattern ps)
   | Parsetree.P_record labs_pats ->
        List.flatten
         (List.map (fun (_, p) -> get_local_idents_from_pattern p) labs_pats)
   | Parsetree.P_paren p -> get_local_idents_from_pattern p
;;



let rec get_local_idents_and_types_from_pattern pat =
  match pat.Parsetree.ast_desc with
   | Parsetree.P_const _
   | Parsetree.P_wild -> []
   | Parsetree.P_var v -> [(v, pat.Parsetree.ast_type)]
   | Parsetree.P_as (p, v) ->
       (v, pat.Parsetree.ast_type) ::
       (get_local_idents_and_types_from_pattern p)
   | Parsetree.P_tuple ps
   | Parsetree.P_constr (_, ps) ->
       List.flatten (List.map get_local_idents_and_types_from_pattern ps)
   | Parsetree.P_record labs_pats ->
        List.flatten
         (List.map
            (fun (_, p) -> get_local_idents_and_types_from_pattern p) labs_pats)
   | Parsetree.P_paren p -> get_local_idents_and_types_from_pattern p
;;



(* ********************************************************************* *)
(* string -> string                                                      *)
(** {b Descr} : Translate a FoCaLize operator name to a legal OCaml or Coq
              function name, preventing the versatile FoCaLize operators
              names from being lexically incorrect if straighforwardly
              converted into OCaml or Coq identifiers.
              The transformation is pretty simple and stupid (read
              sturdy :), replacing all the legal "symbolic" characters
              available for FoCaLize prefix/infix idents (extracted from
              the lexer definitions) by a regular string describing it.

    {b Rem} : Not exported outside this module.

    {b Rem} : This function should not be defined here but in the lexical
    analyser directory, in order to be up to date with the lexem
    definitions. To be even safer, we should define the full range of tokens
    in the [vname] type: we still lack the prefix upper and infix upper
    classes of idents; this would correct and simplify the definition of
    [parse_operator_string], which is still wrong, since it does not handle
    the bunch of particular cases it should treat to (re)-discover the true
    class of identifiers, if we want to correctly treat them.  *)

(* ********************************************************************* *)
let parse_operator_string op_string =
  let renamed_operator = ref "" in
  String.iter
    (fun character ->
      let str_tail =
        (match character with
         | '`' (* ` Helping emacs *) -> "_backquote_"
         | '~' -> "_tilda_"
         | '?' -> "_question_"
         | '$' -> "_dollar_"
         | '!' -> "_bang_"
         | '#' -> "_sharp_"
         | '+' -> "_plus_"
         | '-' -> "_dash_"
         | '*' -> "_star_"
         | '/' -> "_slash_"
         | '%' -> "_percent_"
         | '&' -> "_amper_"
         | '|' -> "_bar_"
         | ',' -> "_comma_"
         | ':' -> "_colon_"
         | ';' -> "_semi_"
         | '<' -> "_lt_"
         | '=' -> "_equal_"
         | '>' -> "_gt_"
         | '@' -> "_at_"
         | '^' -> "_hat_"
         | '\\' -> "_backslash_"
           (* In case we have a delimited ident with strange chars in it. *)
         | ' ' -> "_space_"
         | '.' -> "_dot_"
         | '(' -> "_lparen_"
         | ')' -> "_rparen_"
         | '[' -> "_lbracket_"
         | ']' -> "_rbracket_"
         | '{' -> "_lbrace_"
         | '}' -> "_rbrace_"
         | c ->
             (* For any other character, keep it unchanged. *)
             String.make 1 c) in
      (* Appending on string is not very efficient, but *)
      (* this should not be a real matter here ! *)
      renamed_operator := !renamed_operator ^ str_tail)
    op_string ;
  (* Just return the "translated" identifier name. *)
  !renamed_operator
;;


(** {b Visibility}: Exported outside this module. *)
let vname_as_string_with_operators_expanded = function
  | Parsetree.Vqident s ->  "'" ^ (parse_operator_string s)
  | Parsetree.Vuident ("()" | "[]" | "::" as s) -> s
  | Parsetree.Vlident s -> (
      (* Check if the lowercase identifier must be renamed to prevent keyword
         collision in OCaml of Coq. In this case, transformation of possible
         special characters is directly handled by the keyword collision
         avoidance. So no need to call [parse_operator_string] after. *)
      match Anti_keyword_conflict.string_to_no_keyword_string_if_diff s with
      | Some s' -> s'
      | None ->
          (* If the identifier was not conflicting with keywords then we must
             process it like other identifiers. In effect, conflict resolution
             is done so that replacement string is suitable for direct output,
             so we don't need to send this string to the parsing done on other
             identifiers. *)
          parse_operator_string s
     )
  | Parsetree.Vuident s | Parsetree.Vpident s
  | Parsetree.Viident s -> parse_operator_string s
;;



(* ********************************************************************* *)
(* Format.formatter -> Parsetree.vname -> unit                           *)
(** {b Descr} : Pretty prints a [vname] value as an OCaml or Coq source ident.
    Because FoC allows more infix/prefix operators than OCaml or Coq
    syntax, it's impossible to crudely translate the string of the
    [vname] to OCaml or Coq.
    For instance, a FoC infix operator "( **+ )" has no equivalent in
    OCaml or Coq syntax : "( **+ )" is not a correct operator identifier
    according to OCaml or Coq.
    Then, instead of having particular cases for operators that can be
    straighforward translated (like "( +)") and the others, we adopt a
    uniform mapping for infix and prefix operators using the
    [vname_as_string_with_operators_expanded] function to transform
    infix/prefix operators names before printing and straighforwardly
    print other operators names.

    {b Rem} : Exported ouside this module.                               *)
(* ********************************************************************* *)
let pp_vname_with_operators_expanded ppf vname =
  Format.fprintf ppf "%s" (vname_as_string_with_operators_expanded vname)
;;

let type_coll_from_qualified_species (species_modname, species_vname) =
  (species_modname, (name_of_vname species_vname))
;;



(* ********************************************************************** *)
(** {b Descr} : Describes a species expression used a effective argument
    of a parametrised species. Since an effective parameter of a
    parametrised species can not have itself effective parameters, the
    only possible expressions are those denoting "Self" or another atomic
    species name.

    {b Rem}: Exported outside this module.                                  *)
(* ********************************************************************** *)
type simple_species_expr_as_effective_parameter =
  (** The name of the species used as species parameter is "Self". *)
  | SPE_Self
  (** The name of the species used as species parameter "IS" something else.
      We tag to know if it "IS" a species parameter, a toplevel collection
      or a toplevel species. *)
  | SPE_Species of (Parsetree.qualified_vname * Types.species_collection_kind)
  (** Regular expression used to instanciate a "in" (i.e. entity) parameter. *)
  | SPE_Expr_entity of Parsetree.expr
;;



type simple_species_expr = {
  sse_name : Parsetree.ident ;  (** Name of the base species. *)
  (** Effective arguments that are applied to it. *)
  sse_effective_args : simple_species_expr_as_effective_parameter list
  } ;;



(* ********************************************************************* *)
(* current_unit: Parsetree.module_name -> Parsetree.qualified_species -> *)
(*   Parsetree.ident                                                     *)
(** {b Descr} : Creates a fake [Parsetree.ident] from a
    [Parsetree.qualified_species] in order to be able to lookup in an
    environment. The ident is "fake" in the sense it has no location no
    annotation and no type. Then it must only be used when these fields
    are non-relevant.

    {b Rem} : Exported outside this module.                              *)
(* ********************************************************************* *)
let make_pseudo_species_ident ~current_unit (species_mod, species_name) =
  let ident_desc =
    if species_mod = current_unit then
      Parsetree.I_global (Parsetree.Vname species_name)
    else
      Parsetree.I_global (Parsetree.Qualified (species_mod, species_name)) in
  { Parsetree.ast_loc = Location.none ;
    Parsetree.ast_desc = ident_desc ;
    Parsetree.ast_annot = [] ;
    Parsetree.ast_type = Parsetree.ANTI_none }
;;


(* ********************************************************************** *)
(* Parsetree.qualified_vname -> Parsetree.vname                           *)
(** {b Descr} : Helper to extract the [vname] from a [qualified_vname].
    This basically is forgetting the compilation unit specification if
    there is one in the [qualified_vname].

    {b Args} :
      - unnammed : The [qualified_vname] in which to find the inner
        [vname].

    {b Ret} :
      - Parsetree.vname : The [vname] contained in the [qualified_vname].

    {b Exported} : No.                                                    *)
(* ********************************************************************** *)
let vname_of_qvname = function
  | Parsetree.Vname vname | Parsetree.Qualified (_, vname) -> vname
;;



(* *********************************************************************** *)
(* Parsetree.ident -> Parsetree.vname                                      *)
(** {b Descr} : Extracts the [vname] from an [ident], hence providing the
    name denoted by this identifier without any qualification/scoping.
    For example, "bar", "foo#bar" or "foo!bar" will lead to the [vname]
    "bar".

    {b Args} :
      - [ident] : The identifier in which to find the inner [vname].

    {b Ret} :
      - Parsetree.vname : The [vname] contained in the identifier.

    {b Exported} : Yes.                                                     *)
(* *********************************************************************** *)
let unqualified_vname_of_ident ident =
  match ident.Parsetree.ast_desc with
  | Parsetree.I_local vname -> vname
  | Parsetree.I_global qvname -> vname_of_qvname qvname
;;



(* ************************************************************************* *)
(** {b Descr} : Extracts the [vname] from a [constructor_ident], hence
    providing the name denoted by this identifier without any
    qualification/scoping.
    For example, "bar", "foo#Bar" will lead to the [vname] "Bar".

    {b Args} :
      - [ident] : The [constructor_ident] in which to find the inner [vname].

    {b Ret} :
      - Parsetree.vname : The [vname] contained in the [constructor_ident].

    {b Exported} : Yes.                                                      *)
(* ************************************************************************* *)
let unqualified_vname_of_constructor_ident ident =
  let Parsetree.CI ident = ident.Parsetree.ast_desc in
  unqualified_vname_of_ident ident
;;



(* ************************************************************************* *)
(** {b Descr} : Extracts the [vname] from a [label_ident], hence providing
    the name denoted by this identifier without any qualification/scoping.
    For example, "bar", "foo#bar" will lead to the [vname] "bar".

    {b Args} :
      - [ident] : The [label_ident] in which to find the inner [vname].

    {b Ret} :
      - Parsetree.vname : The [vname] contained in the [constructor_ident].

    {b Exported} : Yes.                                                      *)
(* ************************************************************************* *)
let unqualified_vname_of_label_ident ident =
  let Parsetree.LI ident = ident.Parsetree.ast_desc in
  unqualified_vname_of_ident ident
;;



(* *********************************************************************** *)
(* Parsetree.expr_ident -> Parsetree.vname                                 *)
(** {b Descr} : Extracts the [vname] from an [expt_ident], hence providing
    the name denoted by this identifier without any qualification/scoping.
    For example, "bar", "foo#bar" or "foo!bar" will lead to the [vname]
    "bar".

    {b Args} :
      - [ident] : The [expr_ident] in which to find the inner [vname].

    {b Ret} :
      - Parsetree.vname : The [vname] contained in the [expr_ident].

    {b Exported} : Yes.                                                     *)
(* *********************************************************************** *)
let unqualified_vname_of_expr_ident ident =
  match ident.Parsetree.ast_desc with
   | Parsetree.EI_local vname -> vname
   | Parsetree.EI_global qvname -> vname_of_qvname qvname
   | Parsetree.EI_method (_, vname) -> vname
;;



let make_concatenated_name_from_qualified_vname = function
  | Parsetree.Vname vname -> name_of_vname vname
  | Parsetree.Qualified (mod_name, vname) ->
      mod_name ^ "." ^(name_of_vname vname)
;;



(** [dont_qualify_if_local]: If [true] then if the name is qualified an
    appears to come from the [current_unit] then qualification is omitted.
    This prevents to generate names refering explicitely to their hosting
    module. *)
let make_concatenated_name_with_operators_expanded_from_qualified_vname
    ~current_unit ~dont_qualify_if_local =
  function
  | Parsetree.Vname vname -> vname_as_string_with_operators_expanded vname
  | Parsetree.Qualified (mod_name, vname) -> (
      if dont_qualify_if_local && current_unit = mod_name then
        vname_as_string_with_operators_expanded vname
      else
        mod_name ^ "." ^ (vname_as_string_with_operators_expanded vname)
     )
;;



(* ************************************************************************* *)
(** {b Descr} : Return the list of [vnames] representing free variable
    identifiers in an [expr_desc]. Inside this [expr_desc] any identifier
    bound by a inner let, fun, pattern will hence not be long to the returned
    list.
    {b Exported} : No.                                                       *)
(* ************************************************************************* *)
let get_free_local_vnames_from_expr_desc initial_e_descr =
  (* Remind in [bound_vnames] the identifiers bound in the expression ... by
     binding constructs (fun and patterns) to ignore them. *)
  let rec rec_get_descr bound_vnames = function
  | Parsetree.E_self | Parsetree.E_const _ | Parsetree.E_external _ -> []
  | Parsetree.E_fun (args, body) -> rec_get_expr (args @ bound_vnames) body
  | Parsetree.E_var ({ Parsetree.ast_desc = Parsetree.EI_local vname }) ->
      if List.mem vname bound_vnames then [] else [vname]
  | Parsetree.E_var _ -> []
  | Parsetree.E_app (e1, e2) ->
      let e2s_vnames = rec_get_exprs bound_vnames e2 in
      Handy.list_concat_uniq (rec_get_expr bound_vnames e1) e2s_vnames
  | Parsetree.E_match (e1, pat_exprs) ->
      let e1s_vnames = rec_get_expr bound_vnames e1 in
      List.fold_left
        (fun accu (pat, expr) ->
          (* Extend bound identifiers by those of the pattern. *)
          let bound_vnames' =
            (get_local_idents_from_pattern pat) @ bound_vnames in
          Handy.list_concat_uniq (rec_get_expr bound_vnames' expr) accu)
        e1s_vnames
        pat_exprs
  | Parsetree.E_let (let_def, expr) ->
      (* Recover the new names the let-definition binds to "ignore" then when
         we will descend in the "in" part of the definition. We expect just
         an "extension", i.e. just the new bound names, hence, we append them
         to the already known. *)
      let (bound_vnames', defs_vnames) = rec_get_let_def bound_vnames let_def in
      Handy.list_concat_uniq
        (rec_get_expr (bound_vnames' @ bound_vnames) expr) defs_vnames
  | Parsetree.E_if (e1, e2, e3) ->
      Handy.list_concat_uniq (rec_get_expr bound_vnames e1)
        (Handy.list_concat_uniq
           (rec_get_expr bound_vnames e2) (rec_get_expr bound_vnames e3))
  | Parsetree.E_tuple exprs | Parsetree.E_constr (_, exprs)
  | Parsetree.E_sequence exprs -> rec_get_exprs bound_vnames exprs
  | Parsetree.E_paren e -> rec_get_expr bound_vnames e
  | Parsetree.E_record fields -> rec_get_fields bound_vnames fields
  | Parsetree.E_record_access (expr, _) -> rec_get_expr bound_vnames expr
  | Parsetree.E_record_with (expr, fields) ->
      Handy.list_concat_uniq
        (rec_get_expr bound_vnames expr) (rec_get_fields bound_vnames fields)

  and rec_get_expr bound_vnames e =
    rec_get_descr bound_vnames e.Parsetree.ast_desc

  and rec_get_fields bound_vnames exprs =
    List.fold_left
      (fun accu (_, e) ->
        Handy.list_concat_uniq (rec_get_expr bound_vnames e) accu)
      [] exprs

  and rec_get_exprs bound_vnames exprs =
    List.fold_left
      (fun accu e ->
        Handy.list_concat_uniq (rec_get_expr bound_vnames e) accu)
      [] exprs

  (* In term of bound names, we just return an "extension", i.e. just the new
     bound names, hence, the caller will append them to its already known. *)
  and rec_get_let_def bound_vnames let_def =
    (* Remind the idents that the let binds. It will serve in case of recursive
       definition and also will be returned so that the caller known the new
       names to "ignore" when it will descend in the "in" part of the
       let-definition. *)
    let let_bound_names =
      List.map
        (fun b -> b.Parsetree.ast_desc.Parsetree.b_name)
        let_def.Parsetree.ast_desc.Parsetree.ld_bindings in
    (* If let definition is/are recursive, then pre-insert all the bound names
       in the list. *)
    let bound_vnames' =
      if let_def.Parsetree.ast_desc.Parsetree.ld_rec = Parsetree.RF_rec then
        let_bound_names @ bound_vnames
      else bound_vnames in
    (* Descend in each binding. *)
    let found_free_vnames =
      List.fold_left
        (fun accu binding ->
          let b_descr = binding.Parsetree.ast_desc in
          (* Add the parameters of the definition to the bound [vnames]. *)
          let bound_vnames'' =
            (List.map (fun (n, _) -> n) b_descr.Parsetree.b_params) @
            bound_vnames' in
          match b_descr.Parsetree.b_body with
          | Parsetree.BB_logical lexpr ->
              (rec_get_log_expr bound_vnames'' lexpr) @ accu
          | Parsetree.BB_computational expr ->
              (rec_get_expr bound_vnames'' expr) @ accu)
        [] let_def.Parsetree.ast_desc.Parsetree.ld_bindings in
    (* Return the names bound by the let and those free found. *)
    (let_bound_names, found_free_vnames)

  and rec_get_log_expr bound_vnames log_expr =
    match log_expr.Parsetree.ast_desc with
    | Parsetree.Pr_forall (names, _, l_expr)
    | Parsetree.Pr_exists (names, _, l_expr) ->
        rec_get_log_expr (names @ bound_vnames) l_expr
    | Parsetree.Pr_imply (l_expr1, l_expr2) | Parsetree.Pr_or (l_expr1, l_expr2)
    | Parsetree.Pr_and (l_expr1, l_expr2)
    | Parsetree.Pr_equiv (l_expr1, l_expr2) ->
        Handy.list_concat_uniq
          (rec_get_log_expr bound_vnames l_expr1)
          (rec_get_log_expr bound_vnames l_expr2)
    | Parsetree.Pr_expr expr -> rec_get_expr bound_vnames expr
    | Parsetree.Pr_not l_expr | Parsetree.Pr_paren l_expr ->
        rec_get_log_expr bound_vnames l_expr in

  (* Finally do the job, descending on the initial [expr_desc]. *)
  rec_get_descr [] initial_e_descr
;;



(** Creates an AST node out of some annotations and an expression. *)
let make_annot annot desc = {
  Parsetree.ast_loc = Location.none;
  Parsetree.ast_desc = desc;
  Parsetree.ast_annot = annot;
  Parsetree.ast_type = Parsetree.ANTI_none;
}
;;

(** Creates an AST node out of an expression. *)
let make_ast desc =
  make_annot [] desc
;;
