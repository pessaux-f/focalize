(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            David Delahaye                                           *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                                                                     *)
(*                        CNAM -  LIP6  -  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2009 CNAM, LIP6 and INRIA                                *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: printer.ml,v 1.1 2009-09-17 16:31:50 delahaye Exp $ *)

open Format
open Xml

let print_option f ppf = function
  | None -> ()
  | Some e -> f ppf e

let print_qstring ppf = function
  | Dquote s -> fprintf ppf "\"%s\"" s
  | Squote s -> fprintf ppf "'%s'" s

let print_version ppf = fprintf ppf "version=%a" print_qstring
let print_encoding ppf = fprintf ppf "@ encoding=%a" print_qstring
let print_standalone ppf = fprintf ppf "@ standalone=%a" print_qstring

let print_xml_decl ppf = function
  | { version = v; encoding = e; standalone = s } ->
    fprintf ppf "@[<hov><?xml %a%a%a?>@.@]" print_version v
      (print_option print_encoding) e (print_option print_standalone) s

let print_system ppf = fprintf ppf "@ SYSTEM %a" print_qstring

let print_doctype_decl ppf = function
  | { dtname = n; system = s } ->
    fprintf ppf "@[<hov><!DOCTYPE %s%a>@.@]" n (print_option print_system) s

let print_prolog ppf = function
  | { xml = x ; doctype = d} ->
    begin
      print_option print_xml_decl ppf x;
      print_option print_doctype_decl ppf d
    end

let rec print_att ppf = function
  | [] -> ()
  | (n, v) :: l ->
    begin
      fprintf ppf " %s=%a" n print_qstring v;
      print_att ppf l
    end

let print_tag att ppf = function
  | { tag_name = n; tag_att = a } ->
    begin
      fprintf ppf "%s" n;
      if att then fprintf ppf "%a" print_att a
    end

let rec print_element ppf = function
  | Empty_tag t -> fprintf ppf "<%a/>" (print_tag true) t
  | Elem_tag (t, c) ->
    fprintf ppf "<%a>%a</%a>" (print_tag true) t print_content c
      (print_tag false) t

and print_content ppf = function
  | { cdata = c; cargs = a } ->
    fprintf ppf "%a%a" (print_option pp_print_string) c print_cargs a

and print_cargs ppf = function
  | [] -> ()
  | (e, a) :: l ->
    begin 
      fprintf ppf "%a%a" print_element e (print_option pp_print_string) a;
      print_cargs ppf l
    end

let print_document ppf = function
  | { plg = p; elem = e } ->
    begin
      print_prolog ppf p;
      print_element ppf e;
      pp_print_newline ppf ()
    end
