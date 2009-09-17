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

(* $Id: xml.mli,v 1.1 2009-09-17 16:31:50 delahaye Exp $ *)

type qstring =
  | Squote of string
  | Dquote of string

type xml_decl =
  { version : qstring;
    encoding : qstring option;
    standalone : qstring option }

type doctype_decl =
  { dtname : string;
    system : qstring option }

type prolog =
  { xml : xml_decl option;
    doctype : doctype_decl option }

type tag =
  { tag_name : string;
    tag_att : (string * qstring) list }

type element =
  | Empty_tag of tag
  | Elem_tag of tag * content

and content =
  { cdata : string option;
    cargs : (element * (string option)) list }

type document = { plg : prolog; elem : element }
