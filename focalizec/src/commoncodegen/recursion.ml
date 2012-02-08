(***********************************************************************)
(*                                                                     *)
(*                        FoCaLiZe compiler                            *)
(*                                                                     *)
(*            William Bartlett                                         *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                                                                     *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: recursion.ml,v 1.20 2012-02-08 16:35:29 pessaux Exp $ *)

(**
  This module provides utilities for dealing with recursive function
  definitions.
  Certain functions verify properties of the recursive functions. Others aid in
  the generation of proof obligations.
 *)

exception NestedRecursiveCalls of Parsetree.vname * Location.t;;
exception PartialRecursiveCall of Parsetree.vname * Location.t;;
exception MutualRecursion of (Parsetree.vname * Parsetree.vname);;

(** ***************************************************************************
  Useful for storing bindings that were made between one point of a program
  and another. These constructs express constraints that must hold in
  order to have arrived at a certain point of a program.
 *************************************************************************** *)
type binding =
  | B_let of Parsetree.binding (** The variable was bound to the expression. *)
  | B_match of
      (** The expression matched and the pattern. *)
      Parsetree.expr * Parsetree.pattern
  | B_condition of
      (** The expression was tested and has the given truth value. *)
      Parsetree.expr * bool
;;



(** ***************************************************************************
    {b Descr} : Tests whether a given function applied to given arguments
    constitutes a recursive call.

    @param function_name the qualified name of the recursive function.
    @param argument_list the list of arguments that must be supplied when
    calling the recursive function.
    @param expr_list the arguments supplied at the call being examined.
    @param fexpr the expression indicating the function being applied.

    @raise PartialRecursiveCall Recursive calls to the main function are only
    valid if all arguments are supplied in one go. If the call is partial this
    exception is raised.
    @return [true] if the application constitutes a valid recursive
    call; [false] otherwise.

  {b Visibility}: Not exported outside this module.
 *************************************************************************** *)
let is_recursive_call function_name argument_list expr_list fexpr =
  match fexpr.Parsetree.ast_desc with
   | Parsetree.E_var ident_expr_ast ->
       begin
       match ident_expr_ast.Parsetree.ast_desc with
        | Parsetree.EI_local name ->
            name = function_name
              && (List.length argument_list = List.length expr_list
                ||
                  raise
                    (PartialRecursiveCall
                       (function_name, fexpr.Parsetree.ast_loc)))
        | _ -> false
       end
   | _ -> false
;;



type typed_vname = (Parsetree.vname * Types.type_simple) ;;



(** ***************************************************************************
    {b Descr}: Just to give a name to the result type of [list_recursive_calls]
    to manipulate easier.

    {b Visibility}: Exported outside this module.
 *************************************************************************** *)
type recursive_calls_description =
  ((typed_vname * Parsetree.expr) list * binding list) list
;;



(** ***************************************************************************
    {b Descr} : Compiles a list of information about recursive calls made in a
    function body.
    Information included is :
      - An association list between the declared arguments of the function and
        those passed to the recursive call;
      - A list of bindings visible in the context of the recursive call. This
        list is ordered from innermost to outermost.

    @raise NestedRecursiveCalls in the event of nested recursion.
    @raise PartialRecursiveCall in the event that a recursive call is
    incomplete.

    {b Visibility}: Exported outside this module.
 *************************************************************************** *)
let rec list_recursive_calls function_name argument_list bindings expr =
  let filter_nested_recursive_calls calls1 calls2 = match (calls1, calls2) with
   | ([], l) | (l, []) -> l
   | _ ->
       raise (NestedRecursiveCalls (function_name, expr.Parsetree.ast_loc)) in
  match expr.Parsetree.ast_desc with
   | Parsetree.E_fun (names, expr) ->
       (* Get the type of the function. *)
       let fun_ty =
         (match expr.Parsetree.ast_type with
          | Parsetree.ANTI_none | Parsetree.ANTI_irrelevant
          | Parsetree.ANTI_scheme _ -> assert false
          | Parsetree.ANTI_type t -> t) in
       let fake_scheme = Types.trivial_scheme fun_ty in
       (* We do not have anymore information about "Self"'s structure... *)
       let (names_with_types, _, _) =
         MiscHelpers.bind_parameters_to_types_from_type_scheme
           ~self_manifest: None ~gen_vars_in_scope: [] (Some fake_scheme)
           names in
       (* Just remove the option that must always be Some since we provided
          a scheme. *)
       let names_with_types =
         List.map
           (fun (n, opt_ty) ->
             match opt_ty with None -> assert false | Some t -> (n, t))
           names_with_types in
       (* Add the argument to the list and find recursive calls in the function
          body. *)
       list_recursive_calls
         function_name (argument_list @ names_with_types) bindings expr
   | Parsetree.E_var _ ->
       if is_recursive_call function_name argument_list [] expr then
         [[], bindings]
       else []
   | Parsetree.E_app (fexpr, argexprlist) -> (
       (* Test whether it is the function being defined that is called. *)
       if is_recursive_call function_name argument_list argexprlist fexpr then
         (* If that is the case, check for recursive calls in the arguments of
            this call. *)
         match
           List.concat
             (List.map
                (list_recursive_calls function_name argument_list bindings)
                argexprlist) with
         | [] -> [ List.combine argument_list argexprlist, bindings ]
               (* If no recursive calls are made when calculating the
                  arguments, return the arguments (original and used to make
                  this recursive call) and bindings. *)
         | _ ->
             (* Otherwise raise an exception. *)
             raise
               (NestedRecursiveCalls (function_name, expr.Parsetree.ast_loc))
       else
         (* If this is not a recursive call then look for recursive calls in
            the arguments. *)
         List.concat
           (List.map
              (list_recursive_calls function_name argument_list bindings)
              argexprlist)
      )
   | Parsetree.E_constr (_, expr_list) ->
       List.concat
         (List.map
            (list_recursive_calls function_name argument_list bindings)
            expr_list)
   | Parsetree.E_match (matched_expr, pattern_expr_list) ->
       let list_recursive_calls_in_matched_expr =
         list_recursive_calls function_name argument_list bindings matched_expr
       in
       (* Find the recursive calls in each expression, adding the proper
          'pattern match' binding to the list of bindings. *)
       let list_recursive_calls_for_pattern (p, e) =
         let new_bindings = (B_match (matched_expr, p)) :: bindings in
         list_recursive_calls function_name argument_list new_bindings e in
       let list_recursive_calls_for_all_patterns =
         List.concat
           (List.map list_recursive_calls_for_pattern pattern_expr_list) in
       filter_nested_recursive_calls
         list_recursive_calls_in_matched_expr
         list_recursive_calls_for_all_patterns
   | Parsetree.E_if (condition, expr_true, expr_false) ->
       (* [list_recursive_calls_in_condition] calculates the information
          pertaining to recursive calls in the condition clause. *)
       let list_recursive_calls_in_condition =
         list_recursive_calls function_name argument_list bindings condition in
       (* [list_recursive_calls_in_expr] calculates the information
          pertaining to recursive calls in an expression. *)
       let list_recursive_calls_in_expr boolean expr =
         let new_bindings = (B_condition (condition, boolean)) :: bindings in
         list_recursive_calls function_name argument_list new_bindings expr in
       let list_recursive_calls_in_both_exprs =
         List.concat
           (List.map2
              list_recursive_calls_in_expr
              [true; false] [expr_true; expr_false]) in
       filter_nested_recursive_calls
         list_recursive_calls_in_condition
         list_recursive_calls_in_both_exprs
   | Parsetree.E_let (let_def, expr) ->
       (* Look for recursive calls in the body of the let_def. *)
       let list_recursive_calls_in_def =
         let list_recursive_calls_in_binding binding =
           match binding.Parsetree.ast_desc.Parsetree.b_body with
            | Parsetree.BB_logical _ -> assert(false)
            | Parsetree.BB_computational expr ->
                list_recursive_calls
                  function_name argument_list bindings expr in
         List.concat
           (List.map list_recursive_calls_in_binding
              let_def.Parsetree.ast_desc.Parsetree.ld_bindings) in
       (* Then look in the following expression having augmented the list of
          bindings with those created by the let_def. *)
       let new_bindings =
         let extract_binding binding = B_let binding in
         (List.map extract_binding
            let_def.Parsetree.ast_desc.Parsetree.ld_bindings)
         @ bindings in
       let list_recursive_calls_in_expr =
         list_recursive_calls function_name argument_list new_bindings expr in
       (* Finally make sure that recursive calls are not made in both the
          let_def and the expression. *)
       filter_nested_recursive_calls
         list_recursive_calls_in_def
         list_recursive_calls_in_expr
   | Parsetree.E_record label_expr_list ->
       let list_recursive_calls_in_record_item (_, expr) =
         list_recursive_calls function_name argument_list bindings expr in
       List.concat
         (List.map list_recursive_calls_in_record_item label_expr_list)
   | Parsetree.E_record_access (expr, _) ->
       list_recursive_calls function_name argument_list bindings expr
   | Parsetree.E_record_with (expr, label_expr_list) ->
       let list_recursive_calls_in_record_item (_, expr) =
         list_recursive_calls function_name argument_list bindings expr in
       List.concat
         ((list_recursive_calls function_name argument_list bindings expr) ::
          (List.map list_recursive_calls_in_record_item label_expr_list))
   | Parsetree.E_tuple expr_list
   | Parsetree.E_sequence expr_list ->
       List.concat
         (List.map
            (list_recursive_calls function_name argument_list bindings)
            expr_list)
   | Parsetree.E_self | Parsetree.E_const _ | Parsetree.E_external _ ->
       (* The remaining expressions cannot lead to recursive calls *)
       []
   | Parsetree.E_paren expr ->
       list_recursive_calls function_name argument_list bindings expr
;;



(** ***************************************************************************
    {b Descr} : Given a list of bindings, calculates the list of variables
    that can be considered structurally smaller than any of the given
    variables.

    @param variables the variables.
    @param bindings the list of bindings to be searched for structurally
    smaller variables, ordered from innermost to outermost.

    @return the list of the variables that are structurally smaller than
    [variables].

    {b Visibility}: Not exported outside this module.
 *************************************************************************** *)
let rec get_smaller_variables variables bindings =
  (* NB: Be wary of inner bindings that mask outer variables !!
     It is assumed that patterns do not contain multiple occurrences of the
     same variable.
     [fold_vars_in_pattern] performs the given [action] on each variable in a
     given [pattern]. Information as to whether at least one deconstructive
     pattern has been traversed is passed to the [action] in the form of a
     flag. *)
  let fold_vars_in_pattern action data pattern =
    let rec fold_vars_in_pattern_aux deconstructed_once_flag data pattern =
      match pattern.Parsetree.ast_desc with
       | Parsetree.P_const _ -> data
       | Parsetree.P_var v -> action deconstructed_once_flag data v
       | Parsetree.P_as (p, v) ->
           fold_vars_in_pattern_aux deconstructed_once_flag
             (action deconstructed_once_flag data v) p
       | Parsetree.P_wild -> data
       (* The following patterns are deconstructive. *)
       | Parsetree.P_constr (_, p_list) ->
           List.fold_left (fold_vars_in_pattern_aux true) data p_list
       | Parsetree.P_record lp_list ->
           List.fold_left (fold_vars_in_pattern_aux true) data
             (snd (List.split lp_list))
       | Parsetree.P_tuple p_list ->
           List.fold_left (fold_vars_in_pattern_aux true) data p_list
       | Parsetree.P_paren p ->
           fold_vars_in_pattern_aux deconstructed_once_flag data p in
    fold_vars_in_pattern_aux false data pattern in

  (** *************************************************************************
     {b Descr} : [analyse_match] finds structurally smaller variables of a
     given set of variables by analysing pattern-matching. The function first
     recursively breaks tuples in both matched expression and pattern until a
     variable is reached in the former. At this point we can extract all
     variables in the resulting pattern and (unless the pattern is reduced to
     a single variable) assert that they are all structurally smaller.
   ************************************************************************* *)
  let rec analyse_match (variable_set, structural_set) (expr, pattern) =
    match (expr.Parsetree.ast_desc, pattern.Parsetree.ast_desc) with
     | (Parsetree.E_var { Parsetree.ast_desc = Parsetree.EI_local v }, _) ->
         (* In this case a single variable is being deconstructed by the
            pattern and therefore all variables in the pattern are elligible
            to be structurally smaller. *)
         if Parsetree_utils.VnameSet.mem v variable_set
            || Parsetree_utils.VnameSet.mem v structural_set then
           let action deconstructed_once_flag
               (variable_set, structural_set) variable =
             (* If at least one deconstructing pattern has been encountered it
                is structurally smaller, otherwise it is an alias. *)
             if deconstructed_once_flag then
               (Parsetree_utils.VnameSet.remove variable variable_set,
                Parsetree_utils.VnameSet.add variable structural_set)
             else
               (Parsetree_utils.VnameSet.add variable variable_set,
                Parsetree_utils.VnameSet.remove variable structural_set) in
           fold_vars_in_pattern action (variable_set, structural_set) pattern
         else
           (Parsetree_utils.VnameSet.remove v variable_set,
            Parsetree_utils.VnameSet.remove v structural_set)
     | (Parsetree.E_tuple e_list, Parsetree.P_tuple p_list) ->
         (* Tuples are broken down. *)
         List.fold_left
           analyse_match
           (variable_set, structural_set)
           (List.combine e_list p_list)
     | (Parsetree.E_paren e, _) ->
         analyse_match (variable_set, structural_set) (e, pattern)
     | (_, Parsetree.P_paren p) ->
         analyse_match (variable_set, structural_set) (expr, p)
     | _ ->
         (* In this case none of the variables in the pattern are structurally
            smaller and must therefore be removed from both sets in case they
            mask variables from either set. *)
         let action _ (variable_set, structural_set) v =
           (Parsetree_utils.VnameSet.remove v variable_set,
            Parsetree_utils.VnameSet.remove v structural_set) in
         fold_vars_in_pattern action (variable_set, structural_set) pattern in

  (** *************************************************************************
      {b Descr} : [analyse_binding] adds the appropriate variables in a given
      binding to either or both of given variable sets : each alias of a
      variable is added to the set that contains the latter, and each
      variable that is structurally smaller than a variable in either set is
      added to the [structural_set].

      Btw: this function also removes all other variables encountered that
      do not fit in either category as they may mask variables from either
      set.
   ************************************************************************* *)
  let analyse_binding binding (variable_set, structural_set) =
    match binding with
     | B_let binding ->
         (begin
         let binding_desc = binding.Parsetree.ast_desc in
         let bound_variable = binding_desc.Parsetree.b_name in
         match binding_desc.Parsetree.b_body with
          | Parsetree.BB_logical _ -> assert(false)
          | Parsetree.BB_computational
              { Parsetree.ast_desc = Parsetree.E_var
                 { Parsetree.ast_desc = Parsetree.EI_local v }} ->
                   (* If the variable [v] is one of the considered variables *)
                   (* then the [bound_variable] becomes an alias.            *)
                   if Parsetree_utils.VnameSet.mem v variable_set then
                     (Parsetree_utils.VnameSet.add bound_variable variable_set,
                      Parsetree_utils.VnameSet.remove bound_variable
                        structural_set)
                   else
                     if Parsetree_utils.VnameSet.mem v structural_set then
                       (Parsetree_utils.VnameSet.remove
                          bound_variable variable_set,
                        Parsetree_utils.VnameSet.add bound_variable
                          structural_set)
                     else
                       (Parsetree_utils.VnameSet.remove
                          bound_variable variable_set,
                        Parsetree_utils.VnameSet.remove
                          bound_variable structural_set)
          | Parsetree.BB_computational _ ->
              (Parsetree_utils.VnameSet.remove bound_variable variable_set,
               Parsetree_utils.VnameSet.remove bound_variable structural_set)
         end)
     | B_match (expr, pattern) ->
         analyse_match (variable_set, structural_set) (expr, pattern)
     | B_condition _ ->
         (* No new variables are bound by a condition. *)
         (variable_set, structural_set) in

  let variable_set =
    List.fold_right
      Parsetree_utils.VnameSet.add
      variables Parsetree_utils.VnameSet.empty in
  let result =
    List.fold_right
      analyse_binding bindings
      (variable_set, Parsetree_utils.VnameSet.empty) in
  Parsetree_utils.VnameSet.elements (snd result)
;;



(** ***************************************************************************
    {b Descr}: Tests whether a recursive function uses structural recursion.

    @param function_name the name of the function.
    @param arguments the arguments of the function.
    @param structural_argument the argument supposedly destructured before
    each recursive call.
    @param body the body of the function.

    @return [true] if the given function uses structural recursion; [false]
    otherwise.

    {b Visibility}: Exported outside this module.
 *************************************************************************** *)
let is_structural function_name arguments structural_argument body =
  let recursive_calls = list_recursive_calls function_name arguments [] body in
  let analyse_recursive_call (arguments_assoc_list, bindings) =
    (* Just forget the type while searching in the assoc list. *)
    let argument_expr =
      Handy.list_assoc_custom_eq
        (fun (n1, _) n2 -> n1 = n2)
        structural_argument arguments_assoc_list in
    match argument_expr.Parsetree.ast_desc with
     | Parsetree.E_var { Parsetree.ast_desc = Parsetree.EI_local v } ->
         let smaller_variables =
           get_smaller_variables [structural_argument] bindings in
         List.mem v smaller_variables
     | _ -> false in
  List.for_all analyse_recursive_call recursive_calls
;;
