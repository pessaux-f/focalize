(*  Copyright 2004 INRIA  *)
(*  $Id: parser.ml,v 1.4 2004-06-02 17:08:10 doligez Exp $  *)

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

let prelude = "\
   Require Import Classical.\n\
   Require Import zenon.\n\
   Require Import zenon_coqbool.\n\
  "
;;

let prelude_inserted = ref false;;

let rec parse filename lb oc =
  match Lexer.token lb with
  | REQUIRE s ->
      if not !prelude_inserted then begin
        output_string oc prelude;
        prelude_inserted := true;
      end;
      output_string oc s;
      parse filename lb oc;
  | CHAR c ->
      output_string oc c;
      parse filename lb oc;
  | SECTION s ->
      output_string oc s;
      let b = Lexing.from_string s in
      let (sp, pr) = Lexer.section b in
      cur_species := sp;
      cur_proof := pr;
      cur_step := [];
      parse filename lb oc;
  | TOBE s ->
      Invoke.zenon filename !cur_species !cur_proof !cur_step s oc;
      begin
        let b = Lexing.from_string s in
        match Lexer.lemma b with
        | LEMMA path -> cur_step := path
        | GOAL -> cur_step := incr_last !cur_step;
        | TOP -> ()
      end;
      parse filename lb oc;
  | AUTOPROOF (data, loc) ->
      Printf.fprintf oc "(* %s *)\n" loc;
      Invoke.zenon_loc filename data loc oc;
      parse filename lb oc;
  | EOF -> ()
;;
