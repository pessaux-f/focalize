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

(* $Id: handy.ml,v 1.11 2007-11-21 16:34:15 pessaux Exp $ *)


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



(* *********************************************************************** *)
(* 'a -> 'a list -> 'a list                                                *)
(** {b Descr} : Return a list with the element [e] in head of the list [l]
              only if [e] did not already belong to [l] (equality test
              performed with structural equality [=]).

    {b Rem} : Exported outside this module.                                *)
(* *********************************************************************** *)
let list_cons_uniq_eq a l =
  if List.mem a l then l else a :: l
;;



(* ********************************************************************** *)
(* 'a list -> 'a list -> 'a list                                          *)
(** {b Descr} : Return a list containing elements of [l2] that are not in
              [l1]. This is equivalent to say that the resultign list is
              [l1 - l2].
    {b Rem} : Exported outside this module.                               *)
(* ********************************************************************** *)
let list_substract l1 l2 =
  List.filter (fun e -> not (List.mem e l2)) l1
;;



(* ********************************************************************* *)
(* 'a -> 'a list -> 'a list                                              *)
(** {b Descr} : Returns the index in the list [l] where the element [e]
              was found. Raises [Not_found] if [e] doesn't belong to [l].

    {b Rem} : Exported outside this module.                              *)
(* ********************************************************************* *)
let list_mem_count e l =
  let rec rec_find counter = function
    | [] -> raise Not_found
    | h :: q ->
        if h = e then counter else rec_find (counter + 1) q in
  rec_find 0 l
;;



(* *********************************************************************** *)
(* 'a list -> 'a list -> ' a list *)
(** {b Descr} : Assuming that [l2] does not contain doubles, returns the
              concatenation of elements of [l1] that do not belong to [l2].
              If [l1] contains doubles, then only one of each will appear
              in the resulting list.
              The equality predicate used here is the physical equality.

    {b Rem} : Exported outside this module.                                *)
(* *********************************************************************** *)
let list_concat_uniqq l1 l2 =
  let rec rec_append = function
    | [] -> l2
    | h :: q ->
        if List.memq h l2 then rec_append q else h :: (rec_append q) in
  rec_append l1
;;



(* *********************************************************************** *)
(* ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list                     *)
(** {b Descr} : Assuming that [l2] does not contain doubles, returns the
              concatenation of elements of [l1] that do not belong to [l2].
              If [l1] contains doubles, then only one of each will appear
              in the resulting list.
              The equality predicate used is provided by the [eq_fct]
              argument.

    {b Rem} : Exported outside this module.                                *)
(* *********************************************************************** *)
let list_concat_uniq_custom_eq eq_fct l1 l2 =
  let rec rec_append = function
    | [] -> l2
    | h :: q ->
        if List.exists (fun x -> eq_fct x h) l2 then
          rec_append q
        else h :: (rec_append q) in
  rec_append l1
;;



(* ************************************************************************ *)
(* ('a -> 'a -> bool) -> 'a -> 'a list -> bool                              *)
(** {b Descr} : Test if the element [e] belongs to the list [l]. The
              comparision function used to test the equality is provided by
              the argument [eq_fct].
              Returns [true] if a element of [l] is equal to [e] otherwise
              returns [false].

   {b Rem} : Exported outside this module.                                  *)
(* ************************************************************************ *)
let list_mem_custom_eq eq_fct e l =
  let rec rec_mem = function
    | [] -> false
    | h :: q -> if eq_fct h e then true else rec_mem q in
  rec_mem l
;;



(* ******************************************************************** *)
(* 'a -> 'a list -> 'a list                                             *)
(** {b Descr} :  Test if the element [elem] belongs to the list [l] and
       remove its (first) occurrence from the returned list. Raises
       [Not_found] if [elem] was not found in [l].

    {b Rem} : Exported outside this module.                             *)
(* ******************************************************************** *)
let list_mem_n_remove elem l =
  let rec rec_mem = function
    | [] -> raise Not_found
    | h :: q -> if h = elem then q else h :: (rec_mem q) in
  rec_mem l                                                     
;;



let pp_set_shaded ppf =
  if Configuration.get_fancy_ansi () then Format.fprintf ppf "@<0>\027[2m" ;;

let pp_set_underlined ppf =
  if Configuration.get_fancy_ansi () then Format.fprintf ppf "@<0>\027[4m" ;;

let pp_set_bold ppf =
  if Configuration.get_fancy_ansi () then Format.fprintf ppf "@<0>\027[1m" ;;

let pp_set_videoinv ppf =
  if Configuration.get_fancy_ansi () then Format.fprintf ppf "@<0>\027[7m" ;;

let pp_reset_effects ppf =
  if Configuration.get_fancy_ansi () then Format.fprintf ppf "@<0>\027[0m" ;;



(* ************************************************************************ *)
(* int -> string                                                            *)
(** {b Descr}: Transform an integer to a string compound of only a-z chars.
      Used to write variables. In fact, that only a integer->base 26
      printer.
      I love this function !!! I wrote it once during my Phd Thesis, and
      then I copied/pasted it tons of times since !!!

    {b Rem} : Exported outside this module.                                 *)
(* ************************************************************************ *)
let rec int_to_base_26 i =
  if i >= 26 then
    let ch = (i mod 26) + (Char.code 'a') in
    (int_to_base_26 (i / 26)) ^ Char.escaped (Char.chr ch)
  else
    let ch = (i mod 26) + (Char.code 'a') in
    Char.escaped (Char.chr ch)
;;
