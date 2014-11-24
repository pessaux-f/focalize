(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            François Pessaux                                                *)
(*            Pierre Weis                                                     *)
(*            Damien Doligez                                                  *)
(*                                                                            *)
(*               LIP6  --  INRIA Rocquencourt -- ENSTA ParisTech              *)
(*                                                                            *)
(*  Copyright 2007 - ... LIP6 and INRIA                                       *)
(*            2012 - ... ENSTA ParisTech                                      *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)

(**
  This module provides utilities for dealing with recursive function
  definitions.
  Certain functions verify properties of the recursive functions. Others aid in
  the generation of proof obligations.
 *)

exception NestedRecursiveCalls of Parsetree.vname * Location.t ;;
exception PartialRecursiveCall of Parsetree.vname * Location.t ;;
exception MutualRecursion of (Parsetree.vname * Parsetree.vname) ;;

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
           ~self_manifest: None (Some fake_scheme) names in
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
            this call. To follow the order the Coq "Function" harvests
            recursive call in an application, reverse the list. Hence, calls
            are harvested from right to left. *)
         match
           List.concat
             (List.rev
                (List.map
                   (list_recursive_calls function_name argument_list bindings)
                   argexprlist)) with
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
            the arguments. Same remark about order of recursive calls to fit
            the Coq "Function". *)
         List.concat
           (List.rev
              (List.map
                 (list_recursive_calls function_name argument_list bindings)
                 argexprlist))
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
       list_recursive_calls_in_matched_expr @
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
       list_recursive_calls_in_condition @
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
       list_recursive_calls_in_def @ list_recursive_calls_in_expr
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
