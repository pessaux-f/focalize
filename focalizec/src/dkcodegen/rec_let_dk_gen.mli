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


type order_kind =
  | OK_expr of (Parsetree.expr * (int list)) (* Liste des indices d'arguments
                                                de la fonction récursive
                                                uilisés dans l'ordre. *)
  | OK_wfounded of
      (Parsetree.vname *
       (Parsetree.vname list) *
       ((Env.TypeInformation.species_param *
         Env.ordered_methods_from_params) list) *
       Parsetree.vname list)

val generate_termination_lemmas :
  Context.species_compil_context -> Dk_pprint.dk_print_context ->
  Env.DkGenEnv.t -> explicit_order: order_kind ->
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
