(* $Id: handy.ml,v 1.2 2007-08-17 15:02:49 pessaux Exp $ *)
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



(* ************************************************************************* *)
(* int -> string                                                             *)
(* {b Descr} : Transforms an integer to a string compound of only a-z chars.
              Used to write type variables names. In fact, that only a
              integer->base 26 printer (thar I stole in my PhD code :)).

   {b Rem} : Exported outside this module.                                   *)
(* ************************************************************************* *)
let rec int_to_base_26 i =
 if i >= 26 then
   (begin
   let ch = (i mod 26) + (Char.code 'a') in
   (int_to_base_26 (i / 26)) ^ Char.escaped (Char.chr ch)
   end)
   else
   (begin
   let ch = (i mod 26) + (Char.code 'a') in
   Char.escaped (Char.chr ch)
   end)
;;



(* *************************************************************** *)
(* 'a list -> 'a list -> bool                                      *)
(** {b Descr} : Checks if 2 lists intersect (i.e. have a least one
	      common element).

    {b Rem} : Exported outside this module.                        *)
(* *************************************************************** *)
let list_intersect_p l1 l2 = List.exists (fun e -> List.mem e l2) l1 ;;
