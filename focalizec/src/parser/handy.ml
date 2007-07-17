(* $Id: handy.ml,v 1.5 2007-07-17 08:25:10 pessaux Exp $ *)
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


(** Pretty printing tools. *)

(* *************************************************************** *)
(*   pp_generic_separated_list :                              *)
(*         string -> (Format.formatter -> 'a -> unit) ->           *)
(*            Format.formatter -> 'a list -> unit                  *)
(** {b Descr} : Pretty prints a list of items thanks to the provided
              printing function, separating each item by the
              specified string argument [separator].

    [Rem] : Exported ouside this module.                           *)
(* *************************************************************** *)
let rec pp_generic_separated_list separator printer_fct ppf = function
  | [] -> ()
  | [last] -> printer_fct ppf last
  |  h :: q ->
      Format.fprintf ppf "%a@,%s@ %a"
	printer_fct h separator
	(pp_generic_separated_list separator printer_fct) q
;;



(* ***************************************************************** *)
(* pp_generic_newlined_list :                                        *)
(*  (Format.formatter -> 'a -> unit) -> Format.formatter ->          *)
(*    'a list -> unit                                                *)
(** {b Descr} : Pretty prints a list of items thanks to the provided
              printing function, separating each item by a newline.

    {b Rem} : Exported ouside this module.                           *)
(* ***************************************************************** *)
let rec pp_generic_newlined_list printer_fct ppf = function
  | [] -> ()
  | [last] -> printer_fct ppf last
  |  h :: q ->
      Format.fprintf ppf "%a@\n%a"
	printer_fct h (pp_generic_newlined_list printer_fct) q
;;



(* ****************************************************************** *)
(* pp_generic_explicit_option :                                       *)
(*   (Format.formatter -> 'a -> unit) -> Format.formatter ->          *)
(*     'a option -> unit                                              *)
(** {b Descr} : Pretty prints an optional item thanks to the provided
              printing function. If None, then prints "None" else
              prints "Some (...)".

    {b Rem} : Exported ouside this module.                            *)
(* ****************************************************************** *)
let pp_generic_explicit_option printer_fct ppf = function
  | None -> Format.fprintf ppf "None"
  | Some data -> Format.fprintf ppf "Some@ (%a)" printer_fct data
;;



(* ****************************************************************** *)
(* pp_generic_option :                                                *)
(*  string -> (Format.formatter -> 'a -> unit) ->                     *)
(*    Format.formatter -> 'a option -> unit                           *)
(** {b Descr} : Pretty prints an optional item thanks to the provided
              printing function. If None, then prints nothing else
              prints the [some_prefix] string provided as argument
              followed by the value itself.

    {b Rem} : Exported ouside this module.                            *)
(* ****************************************************************** *)
let pp_generic_option some_prefix printer_fct ppf = function
  | None -> ()
  | Some data -> Format.fprintf ppf "%s%a" some_prefix printer_fct data
;;
