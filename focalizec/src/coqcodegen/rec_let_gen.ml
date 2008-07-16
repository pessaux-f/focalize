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

(* $Id: rec_let_gen.ml,v 1.4 2008-07-16 13:24:19 pessaux Exp $ *)

let generate_termination_lemmas _ctx _print_ctx _env recursive_calls =
  List.iter
    (fun (n_exprs, bindings) ->
      Format.eprintf "n/expr@." ;
      List.iter
	(fun (n, expr) ->
	  Format.eprintf "\tn: %a, expr: %a@."
	    Sourcify.pp_vname n Sourcify.pp_expr expr)
	n_exprs ;
      Format.eprintf "bindings@." ;
      List.iter
	(function
	  | Recursion.B_let let_binding ->
	      Format.eprintf "\tB_let, binding: %a@."
		Sourcify.pp_binding let_binding
	  | Recursion.B_match (expr, pattern) ->
	      Format.eprintf "\tB_match,  expr: %a, pattern: %a@."
		Sourcify.pp_expr expr Sourcify.pp_pattern pattern
	  | Recursion.B_condition (expr, bool_val) ->
	      Format.eprintf "\tB_condition, expr: %a, bool: %b@."
		Sourcify.pp_expr expr bool_val)
	bindings)
    recursive_calls
;;
