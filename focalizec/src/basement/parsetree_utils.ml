(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            Fran�ois Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                                                                     *)
(*                 LIP6  --  INRIA Rocquencourt  -- ENSTA              *)
(*                                                                     *)
(*  Copyright 2007 - 2012 LIP6 and INRIA                               *)
(*            2012 ENSTA ParisTech                                     *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: parsetree_utils.ml,v 1.42 2012-11-13 14:02:05 pessaux Exp $ *)

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
(* Parsetree.constructor_ident -> Parsetree.vname                            *)
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
    Thsi prevents to generate names refering explicitely to their hosting
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



(* *********************************************************************** *)
(** {b Descr} : Check if an [expr_desc] has the form of a local identifier
    and if yes, returns the [vname] representing this identifier. If no,
    raises [Not_found].
    Note that the check is independent of parentheses.

    {b Exported} : Yes.                                                    *)
(* *********************************************************************** *)
let rec get_local_vname_from_expr_desc = function
  | Parsetree.E_var ({ Parsetree.ast_desc = Parsetree.EI_local vname }) ->
      vname
  | Parsetree.E_paren expr ->
      get_local_vname_from_expr_desc expr.Parsetree.ast_desc
  | _ -> raise Not_found
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
