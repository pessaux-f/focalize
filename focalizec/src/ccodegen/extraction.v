Require Import pcm.
Require Import ilnl.
Require Import pcm2ilnl.
(* Require Import ilcaml. *)
(* Require Import ilnl2ilcaml. *)

(* External *)
Extract Inlined Constant string => "string".
Extract Inductive list => "list" [ "[]" "(::)" ].

(* PCM *)

(* PCM 2 ILNL*)


Extraction "ccodegen" PCM2ILNL.