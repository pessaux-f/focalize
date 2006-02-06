(*  Copyright 2004 INRIA  *)
(*  $Id: parser.ml,v 1.13 2006-02-06 17:56:06 doligez Exp $  *)

open Misc;;
open Printf;;
open Token;;

let cur_species = ref "";;
let cur_proof = ref "";;
let cur_step = ref ([] : int list);;
let cur_loc = ref None;;

let rec incr_last = function
  | [] -> []
  | [i] -> [i+1]
  | h::t -> h :: (incr_last t)
;;

let prelude = Printf.sprintf "\
   Require Import zenon.\n\
   Require Import zenon_coqbool.\n\
  "
;;

let parse filename lb oc =
  let prelude_inserted = ref false in
  let check_insert_prelude () =
    if not !prelude_inserted then begin
      output_string oc prelude;
      prelude_inserted := true;
    end;
  in
  let loc = ref "" in
  let name = ref "" in
  let syntax = ref "" in
  let statement = ref "" in
  let buf = Buffer.create 10000 in
  let rec loop () =
    match Lexer.token lb with
    | REQUIRE ->
        check_insert_prelude ();
        output_string oc "Require";
        loop ();
    | CHAR c ->
        output_char oc c;
        loop ();
    | BEGINAUTOPROOF ->
        check_insert_prelude ();
        loc := "";
        name := "";
        syntax := "";
        statement := "";
        Buffer.clear buf;
        Buffer.add_string buf "\n%%begin-auto-proof";
        autoproof ();
    | EOF -> ()
    | _ -> error "unexpected %% header outside begin/end-auto-proof"
  and autoproof () =
    match Lexer.token lb with
    | LOCATION l -> loc := l; autoproof ();
    | NAME n ->
        name := n;
        Buffer.add_string buf (sprintf "\n%%%%name: %s" n);
        autoproof ();
    | SYNTAX s -> syntax := s; autoproof ();
    | STATEMENT s -> statement := s; autoproof ();
    | CHAR c -> Buffer.add_char buf c; autoproof ();
    | ENDAUTOPROOF ->
        Buffer.add_string buf "\n%%end-auto-proof";
        if !syntax = "TPTP" then Invoke.set_tptp_option ();
        Printf.fprintf oc "\n(* %s *)\n" !loc;
        Invoke.atp filename (!statement, !name) (Buffer.contents buf) !loc oc;
        loop ();
    | REQUIRE -> output_string oc "Require";
    | BEGINAUTOPROOF -> error "nested begin/end-auto-proof"
    | EOF -> error "unmatched begin-auto-proof at end of file"
  in loop ();
;;
