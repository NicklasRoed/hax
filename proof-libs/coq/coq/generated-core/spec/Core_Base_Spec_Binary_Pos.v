(* File automatically generated by Hacspec *)
From Coq Require Import ZArith.
Require Import List.
Import List.ListNotations.
Open Scope Z_scope.
Open Scope bool_scope.
Require Import Ascii.
Require Import String.
Require Import Coq.Floats.Floats.
From RecordUpdate Require Import RecordSet.
Import RecordSetNotations.

(* From Core Require Import Core. *)

From Core Require Import Core_Base_Spec_Haxint.
Export Core_Base_Spec_Haxint.

From Core Require Import Core_Base_Spec_Binary_Positive.
Export Core_Base_Spec_Binary_Positive.

Notation "'t_POS'" := N.
Notation "'POS_ZERO'" := N0.
Notation "'POS_POS'" := Npos.

Definition match_pos (s : t_HaxInt) : t_POS := s.
