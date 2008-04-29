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

(* $Id: parsetree_utils.ml,v 1.11 2008-04-29 13:27:01 pessaux Exp $ *)

let name_of_vname = function
  | Parsetree.Vlident s
  | Parsetree.Vuident s
  | Parsetree.Vpident s
  | Parsetree.Viident s
  | Parsetree.Vqident s -> s
;;


(* *************************************************************** *)
(** {b Descr} : Module stuff to create sets of [Parsetree.vname]s.
              This will serves to make sets of methods [vname]s in
              order to represent dependencies.
              We also keep the name's scheme in order to be able
              to annotate it if required during Coq code
              generation.

    {b Rem} : Not exported outside this module.                    *)
(* *************************************************************** *)
module DepNameMod = struct
  type t = (Parsetree.vname * Types.type_simple)
  let compare (n1, _) (n2, _) = compare n1 n2
end ;;
module DepNameSet = Set.Make (DepNameMod) ;;



module VnameMod = struct
  type t = Parsetree.vname
  let compare = compare
end ;;
module VnameSet = Set.Make (VnameMod) ;;

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
(** {b Descr} : Pretty prints a [vname] value as OCaml or Coq source.
              Because FoC allows more infix/prefix operators than OCaml
              or Coq syntax, it's impossible to crudely translate the
              string of the [vname] to OCaml or Coq
              For instance, a FoC infix operator "( **+ )" has no
              equivalent in OCaml or Coq syntax : "( **+ )" is not a
              correct operator identifier according to OCaml or Coq.
              Then, instead of havign particular cases for operators
              that can be straighforward translated (like "( +)") and
              the others, we adopt a uniform mapping for infix and
              prefix operators using the [parse_operator_string]
              function to transform infix/prefix operators names
              before printing and straighforwardly print other
              operators names.

    {b Rem} : Exported ouside this module.                              *)
(* ******************************************************************** *)
let pp_vname_with_operators_expanded ppf = function
  | Parsetree.Vlident s
  | Parsetree.Vuident s
  | Parsetree.Vqident s -> Format.fprintf ppf "%s" s
  | Parsetree.Vpident s
  | Parsetree.Viident s -> Format.fprintf ppf "%s" (parse_operator_string s)
;;



let vname_as_string_with_operators_expanded = function
  | Parsetree.Vlident s
  | Parsetree.Vuident s
  | Parsetree.Vqident s -> s
  | Parsetree.Vpident s
  | Parsetree.Viident s -> parse_operator_string s
;;



let type_coll_from_qualified_species (species_modname, species_vname) =
  (species_modname, (name_of_vname species_vname))
;;


type species_param_kind =
  | SPK_in       (** Parameter is an entity parameter ("in"). *)
  | SPK_is       (** Parameter is a collection parameter ("is"). *)
;;
