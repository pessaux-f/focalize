Require Export ast.
Require Import pcm.
Require Import ilnl.
Require Import pcm2ilnl.
(* Require Import ilcaml. *)
(* Require Import ilnl2ilcaml. *)

(* External *)
Extract Inlined Constant string => "string".
Extract Inlined Constant int => "int".
Extract Inductive list => "list" [ "[]" "(::)" ].
Extract Inductive option => "option" [ "Some" "None" ].

(* AST *)
Extract Inlined Constant info => "Imports.info".

(* PCM *)
Extract Constant PCM.species => "Imports.dummy_species".
Extract Constant PCM.dummy_species => "()".

(* PCM 2 ILNL*)


Extraction "ccodegen" PCM2ILNL.