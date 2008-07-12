Require Export ast.
Require Import pcm.
Require Import ilnl.
Require Import pcm2ilnl.
(* Require Import ilcaml. *)
(* Require Import ilnl2ilcaml. *)

(* External *)
Extract Inductive unit => "unit" [ "()" ].
Extract Inductive bool => "bool" [ "true" "false" ].
Extract Inlined Constant string => "string".
Extract Inlined Constant int => "int".
Extract Inductive list => "list" [ "[]" "(::)" ].
Extract Inductive option => "option" [ "Some" "None" ].
(* Extract Constant history => "Env.from_history". *)

(* AST *)
Extract Inlined Constant location => "Location.t".

(* PCM *)
Extract Constant pcm_expr => "Parsetree.expr".

Extract Constant pcm_phrase => "Infer.please_compile_me".
Extract Inlined Constant pcm_phrase_location =>
"Imports.pcm_phrase_location".
Extract Inlined Constant pcm_phrase_is_open =>
"Imports.pcm_phrase_is_open".
Extract Inlined Constant pcm_phrase_open_name =>
"Imports.pcm_phrase_open_name".
Extract Inlined Constant pcm_phrase_is_expr => 
"Imports.pcm_phrase_is_expr".
Extract Inlined Constant pcm_phrase_expr_value =>
"Imports.pcm_phrase_expr_value".


(* Extract Constant pcm.species =>  *)
(* "(Parsetree.species_def * Env.TypeInformation.species_description * Dep_analysis.name_node list)". *)

(* *)

(* PCM 2 ILNL*)
Extract Constant PCM2ILNL.env => "unit".


Extraction "ccodegen" PCM2ILNL.translate.