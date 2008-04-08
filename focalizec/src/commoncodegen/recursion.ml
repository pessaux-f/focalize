(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*            William Bartlett                                         *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: recursion.ml,v 1.1 2008-04-08 15:26:02 bartlett Exp $ *)

(**
  This module provides utilities for dealing with recursive function definitions.
  Certain functions verify properties of the recursive functions. Others aid in
  the generation of proof obligations.
 *)

exception NestedRecursiveCalls
exception PartialRecursiveCall

(**
  Useful for storing bindings that were made between one point of a program
  and another. These constructs express constraints that must hold in
  order to have arrived at a certain point of a program.
 *)
type binding =
| B_let of Parsetree.binding (** the variable was bound to the expression *)
| B_match of Parsetree.pattern * Parsetree.expr (** the expression matched the pattern *)
| B_condition of Parsetree.expr * bool (** The expression was tested and has the given truth value *)

(**
  Tests whether a given function applied to given arguments constitutes a recursive call.
  @param function_name the qualified name of the recursive function
  @param argument_list the list of arguments that must be supplied when calling the recursive function
  @param argexprlist the arguments supplied at the call being examined
  @param fexpr the expression indicating the function being applied

  @raise PartialRecursiveCall Recursive calls to the main function are only valid if all arguments
    are supplied in one go. If the call is partial this exception is raised.
  @return [true] if the application constitutes a valid recursive call; [false] otherwise
 *)
let is_recursive_call function_name argument_list argexprlist fexpr =
match fexpr.Parsetree.ast_desc with
| Parsetree.E_var ident_expr_ast ->
  begin match ident_expr_ast.Parsetree.ast_desc with
  | Parsetree.EI_local name ->
      name = function_name
      && (List.length argument_list = List.length argexprlist || raise PartialRecursiveCall)
  | _ -> false
  end
| Parsetree.E_app _ | Parsetree.E_constr _
| Parsetree.E_match _ | Parsetree.E_if _
| Parsetree.E_let _ | Parsetree.E_self
| Parsetree.E_const _ | Parsetree.E_fun _
| Parsetree.E_record _ | Parsetree.E_record_access _
| Parsetree.E_record_with _ | Parsetree.E_external _
| Parsetree.E_tuple _ -> false

(**
  Compiles a list of information about recursive calls made in a function body.
  Information included is :
    - An association list between the declared arguments of the function and those
    passed to the recursive call;
    - A list of bindings visible in the context of the recursive call
 *)
let rec list_recursive_calls function_name argument_list bindings expr_ast =
match expr_ast.Parsetree.ast_desc with
(* no recursive calls in certain constants *)
| Parsetree.E_fun (names, expr) ->
  (* add the argument to the list and find recursive calls in the
   * function body *)
  list_recursive_calls function_name (argument_list@names) bindings expr
| Parsetree.E_var _ ->
  if is_recursive_call function_name argument_list [] expr_ast
  then [[], bindings]
  else []
| Parsetree.E_app (fexpr, argexprlist) -> begin
  (* test whether it is the function being defined that is called *)
  if is_recursive_call function_name argument_list argexprlist fexpr then
    (* if that is the case, check for recursive calls in the arguments of this call *)
    match List.concat (List.map
      (list_recursive_calls function_name argument_list bindings) argexprlist)
    with
    (* if no recursive calls are made when calculating the arguments, return the
     * arguments (original and used to make this recursive call) and bindings *)
    | [] -> [List.combine argument_list argexprlist, bindings]
    (* otherwise raise an exception *)
    | _ -> raise NestedRecursiveCalls
  else
    (* if this is not a recursive call then look for recursive calls in the arguments *)
    List.concat (List.map (list_recursive_calls function_name argument_list bindings) 
      argexprlist)
  end
| Parsetree.E_constr (_, expr_list) ->
  List.concat (List.map (list_recursive_calls function_name argument_list bindings) expr_list)
| Parsetree.E_match (matched_expr, pattern_expr_list) ->
  (* find the recursive calls in each expression, adding the proper 'pattern match' binding
   * to the list of bindings. *)
  (* FIXME a recursive call in the matched_expr can create nested recursive calls *)
  let list_recursive_calls_for_pattern (p,e) =
    let new_bindings = bindings@[B_match (p, matched_expr)] in
    list_recursive_calls function_name argument_list new_bindings e
  in List.concat (List.map list_recursive_calls_for_pattern pattern_expr_list)
| Parsetree.E_if (condition, expr_true, expr_false) ->
  (* FIXME some strange things can happen if there are recursive calls in the condition and
   * either or both of the alternatives *)
  (* [list_recursive_calls_in_condition] calculates the information pertaining
   * to recursive calls in the condition clause *)
  let list_recursive_calls_in_condition =
    list_recursive_calls function_name argument_list bindings condition in
  (* [list_recursive_calls_in_expr] calculates the information pertaining to recursive
   * calls in an expression *)
  let list_recursive_calls_in_expr boolean expr =
    let new_bindings = bindings@[B_condition (condition, boolean)] in
    list_recursive_calls function_name argument_list new_bindings expr
  in List.concat ((list_recursive_calls_in_condition)::
    (List.map2 list_recursive_calls_in_expr [true; false] [expr_true; expr_false]))
| Parsetree.E_let (let_def, expr) ->
  let list_recursive_calls_in_def =
    let list_recursive_calls_in_binding binding =
      match binding.Parsetree.ast_desc.Parsetree.b_body with
      | Parsetree.BB_logical _ -> assert(false)
      | Parsetree.BB_computational expr ->
        list_recursive_calls function_name argument_list bindings expr
    in List.concat (List.map list_recursive_calls_in_binding
      let_def.Parsetree.ast_desc.Parsetree.ld_bindings)
  in
  let new_bindings =
    let extract_binding binding = B_let binding in
    bindings @ (List.map extract_binding let_def.Parsetree.ast_desc.Parsetree.ld_bindings)
  in list_recursive_calls_in_def @
    (list_recursive_calls function_name argument_list new_bindings expr)
| Parsetree.E_record label_expr_list ->
  let list_recursive_calls_in_record_item (_, expr) =
    list_recursive_calls function_name argument_list bindings expr
  in List.concat (List.map list_recursive_calls_in_record_item label_expr_list)
| Parsetree.E_record_access (expr, _) ->
  list_recursive_calls function_name argument_list bindings expr
| Parsetree.E_record_with (expr, label_expr_list) ->
  let list_recursive_calls_in_record_item (_, expr) =
    list_recursive_calls function_name argument_list bindings expr
  in List.concat ((list_recursive_calls function_name argument_list bindings expr)::
    (List.map list_recursive_calls_in_record_item label_expr_list))
| Parsetree.E_tuple expr_list ->
  List.concat (List.map (list_recursive_calls function_name argument_list bindings) expr_list)
| Parsetree.E_self | Parsetree.E_const _
| Parsetree.E_external _ -> []

(**
  Given a list of bindings, calculates the list of variables that can be considered structurally
  smaller than a given variable.
 *)

let rec get_smaller_variables variables bindings =
  let analyse_binding (variable_set, structural_set) =
  function
  | B_let binding ->
      let binding_desc = binding.Parsetree.ast_desc in
      let bound_variable = binding_desc.Parsetree.b_name in
      begin match binding_desc.Parsetree.b_body with
        | Parsetree.BB_logical _ -> assert(false)
        | Parsetree.BB_computational
              {Parsetree.ast_desc = Parsetree.E_var
              {Parsetree.ast_desc = Parsetree.EI_local v}} ->
            if Parsetree_utils.VnameSet.mem v variable_set then
              (variable_set, Parsetree_utils.VnameSet.add bound_variable structural_set)
            else
              (Parsetree_utils.VnameSet.remove bound_variable variable_set, structural_set)
        | Parsetree.BB_computational _ ->
              (variable_set, structural_set)
      end
  | B_match (pattern, expr) ->
      let rec analyse_match (variable_set, structural_set) (pattern, expr) =
      begin match (pattern.Parsetree.ast_desc, expr.Parsetree.ast_desc) with
      | (_, Parsetree.E_var {Parsetree.ast_desc = Parsetree.EI_local v}) ->
        if Parsetree_utils.VnameSet.mem v variable_set then
          let rec add_vars_in_pattern deconstructed_once_flag set pattern =
            begin match pattern.Parsetree.ast_desc with
            | Parsetree.P_const _ -> set
            | Parsetree.P_var v ->
                if deconstructed_once_flag then
                  Parsetree_utils.VnameSet.add v set
                else set
            | Parsetree.P_as (p,v) ->
                if deconstructed_once_flag then
                  Parsetree_utils.VnameSet.add v (add_vars_in_pattern true set p)
                else add_vars_in_pattern false set p
            | Parsetree.P_wild -> set
            | Parsetree.P_constr (_, p_list) ->
                List.fold_left (add_vars_in_pattern true) set p_list
            | Parsetree.P_record lp_list ->
                List.fold_left (add_vars_in_pattern true) set (snd (List.split lp_list))
            | Parsetree.P_tuple p_list ->
                List.fold_left (add_vars_in_pattern true) set p_list
          end in
          (variable_set, add_vars_in_pattern false structural_set pattern)
        else
          (Parsetree_utils.VnameSet.remove v variable_set, structural_set)
      | (Parsetree.P_tuple p_list, Parsetree.E_tuple e_list) ->
          List.fold_left analyse_match (variable_set, structural_set) (List.combine p_list e_list)
      | _ -> (variable_set, structural_set)
      end in
      analyse_match (variable_set, structural_set) (pattern, expr)
  | B_condition _ -> (variable_set, structural_set)
  in
  let variable_set = List.fold_right Parsetree_utils.VnameSet.add
    variables Parsetree_utils.VnameSet.empty in
  let result = List.fold_left analyse_binding
    (variable_set, Parsetree_utils.VnameSet.empty) bindings in
  Parsetree_utils.VnameSet.elements (snd result)

(**
  Tests whether a recursive function uses structural recursion.
 *)
let is_structural function_name arguments structural_argument body =
  let recursive_calls = list_recursive_calls function_name arguments [] body in
  let analyse_recursive_call (arguments_assoc_list, bindings) =
    let argument_expr = List.assoc structural_argument arguments_assoc_list in
    let smaller_variables = get_smaller_variables [structural_argument] bindings in
    match argument_expr.Parsetree.ast_desc with
    | Parsetree.E_var {Parsetree.ast_desc = Parsetree.EI_local v} -> List.mem v smaller_variables
    | _ -> false
  in
  List.fold_right ( && ) (List.map analyse_recursive_call recursive_calls) true
