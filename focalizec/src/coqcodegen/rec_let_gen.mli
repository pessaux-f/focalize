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

type termination_expr_kind =
  | TEK_order of Parsetree.expr
  | TEK_measure of Parsetree.expr

type order_kind =
  | OK_expr of (termination_expr_kind * (int list))
  | OK_wfounded of
      (Parsetree.vname *
       (Parsetree.vname list) *
       ((Env.TypeInformation.species_param *
         Env.ordered_methods_from_params) list) *
       Parsetree.vname list)

val generate_termination_lemmas :
  Context.species_compil_context -> Types.coq_print_context ->
  Env.CoqGenEnv.t -> explicit_order: order_kind ->
  Recursion.recursive_calls_description ->
    unit

val transform_recursive_calls_args_into_tuple :
  Context.species_compil_context -> local_idents: Parsetree.vname list ->
    Parsetree.vname -> Parsetree.expr -> Parsetree.expr

val print_user_termination_obls :
  Parsetree.vname ->
  (((Parsetree.vname * Types.type_simple) * Parsetree.expr) list *
   Recursion.binding list)
  list ->
    Parsetree.expr -> int list -> unit
