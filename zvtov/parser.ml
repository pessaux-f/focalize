(*  Copyright 2004 INRIA  *)
(*  $Id: parser.ml,v 1.2 2004-05-27 17:18:10 doligez Exp $  *)

open Token;;

let cur_species = ref "";;
let cur_proof = ref "";;
let cur_step = ref ([] : int list);;

let rec incr_last = function
  | [] -> []
  | [i] -> [i+1]
  | h::t -> h :: (incr_last t)
;;

let rec xparse filename lb oc =
  match Lexer.token lb with
  | CHAR c ->
      output_string oc c;
      xparse filename lb oc;
  | SECTION s ->
      output_string oc s;
      let b = Lexing.from_string s in
      let (sp, pr) = Lexer.section b in
      cur_species := sp;
      cur_proof := pr;
      cur_step := [];
      xparse filename lb oc;
  | LEMMA s ->
      output_string oc s;
      let b = Lexing.from_string s in
      cur_step := Lexer.lemma b;
      xparse filename lb oc;
  | GOAL s ->
      output_string oc s;
      cur_step := incr_last !cur_step;
      xparse filename lb oc;
  | TOBE s ->
      Invoke.zenon filename !cur_species !cur_proof !cur_step s oc;
      xparse filename lb oc;
  | EOF -> ()
;;

let prelude =
  "Require Import Classical.\n\
   Require Import zenon.\n\
   Require Import zenon_coqbool.\n\
  "
;;

let parse filename lb oc =
  output_string oc prelude;
  xparse filename lb oc;
;;
