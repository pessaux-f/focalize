(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* Runtime library for pretty-printers generated with camlpp.*)

open Format;;

(* Printing tokens that must be escaped.
   These escapes are not in the sense of String.escaped or
   Char.escaped, since accented chars should not be replaced by the
   decimal encoding of their ascii code. *) 

(* Printing strings with quotes. *)
let escape_string s =
  let more = ref 0 in
  for i = 0 to String.length s - 1 do
   match s.[i] with
   | '\\' | '"' -> incr more
   |  _ -> ()
  done;
  if !more = 0 then s else
  let res = String.create (String.length s + !more) in
  let j = ref 0 in
  for i = 0 to String.length s - 1 do
   let c = s.[i] in
   match c with
   | '\\' | '"' -> res.[!j] <- '\\'; incr j; res.[!j] <- c; incr j
   | _ -> res.[!j] <- c; incr j
  done;
  res;;

let escape_char c = if c = '\'' then "\\\'" else String.make 1 c;;

let print_quoted_string s = printf "\"%s\"" (escape_string s);;
let print_quoted_char c = printf "'%s'" (escape_char c);;
let print_quoted_int i = if i < 0 then printf "(%d)" i else printf "%d" i;;
let print_quoted_float f = if f <= 0.0 then printf "(%f)" f else printf "%f" f;;

(* Iterators *)
let print_list f l =
 printf "@[<1>[";
 let rec pl = function
 | [] -> printf "@;<0 -1>]@]"
 | [x] -> f x; pl []
 | x :: xs -> f x; printf ";@ "; pl xs in
 pl l;;

let print_array f v =
 printf "@[<2>[|";
 let l = Array.length v in
 if l >= 1 then f v.(0);
 if l >= 2 then
  for i = 1 to l - 1 do
   printf ";@ "; f v.(i)
  done;
 printf "@;<0 -1>|]@]";;

let print_option f = function
  | None -> print_string "None"
  | Some x -> printf "@[<1>Some@ "; f x; printf "@]";;

let print_bool = function
  | true -> print_string "true" | _ -> print_string "false";;

let print_poly x = print_string "<poly>";;

(* End of runtime library *)
