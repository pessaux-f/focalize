(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            Fran�ois Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

type termination_expr_kind =
  | TEK_order of Parsetree.expr
  | TEK_measure of Parsetree.expr

type order_kind =
  | OK_expr of (termination_expr_kind * int)
  | OK_wfounded of
      (Parsetree.vname *
       (Parsetree.vname list) *
       ((Env.TypeInformation.species_param *
         Env.ordered_methods_from_params) list) *
       Parsetree.vname list)

val generate_termination_lemmas :
  Context.species_compil_context -> Coq_pprint.coq_print_context ->
  Env.CoqGenEnv.t -> Types.type_variable list -> explicit_order: order_kind ->
  Recursion.recursive_calls_description ->
    unit

val transform_recursive_calls_args_into_tuple :
  Context.species_compil_context -> current_unit: Parsetree.module_name ->
  local_idents: Parsetree.vname list -> Parsetree.vname -> Parsetree.expr ->
  Parsetree.expr

val print_user_termination_obls_for_order :
  Parsetree.vname -> (Parsetree.vname * Types.type_simple) list ->
  (((Parsetree.vname * Types.type_simple) * Parsetree.expr) list *
   Recursion.binding list)
  list ->
    Parsetree.expr -> int -> unit

val print_user_termination_obls_for_measure :
  Parsetree.vname -> (Parsetree.vname * Types.type_simple) list ->
  (((Parsetree.vname * Types.type_simple) * Parsetree.expr) list *
   Recursion.binding list)
  list ->
    Parsetree.expr -> int -> Parsetree.vname -> Types.type_simple -> unit

val subst_params_by_tuple_projections :
  string -> Parsetree.expr -> int ->
  (Parsetree.vname * Types.type_simple) list -> Parsetree.expr
