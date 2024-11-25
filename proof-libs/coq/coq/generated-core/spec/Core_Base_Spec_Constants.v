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

Definition v_BITS_128_ : t_HaxInt := 128.
Definition v_BITS_16_ : t_HaxInt := 16.
Definition v_BITS_32_ : t_HaxInt := 32.
Definition v_BITS_64_ : t_HaxInt := 64.
Definition v_BITS_8_ : t_HaxInt := 8.

Definition v_WORDSIZE_128_ : t_HaxInt := N.pow 2 128.
Definition v_WORDSIZE_128_SUB_1_ : t_HaxInt := N.pow 2 128 - 1.

Definition v_WORDSIZE_16_ : t_HaxInt := N.pow 2 16.
Definition v_WORDSIZE_16_SUB_1_ : t_HaxInt := N.pow 2 16.

Definition v_WORDSIZE_32_ : t_HaxInt := N.pow 2 32.
Definition v_WORDSIZE_32_SUB_1_ : t_HaxInt := N.pow 2 32 - 1.

Definition v_WORDSIZE_4_ : t_HaxInt := N.pow 2 4.
Definition v_WORDSIZE_4_SUB_1_ : t_HaxInt := N.pow 2 4 - 1.

Definition v_WORDSIZE_64_ : t_HaxInt := N.pow 2 64.
Definition v_WORDSIZE_64_SUB_1_ : t_HaxInt := N.pow 2 64 - 1.

Definition v_WORDSIZE_8_ : t_HaxInt := N.pow 2 8.
Definition v_WORDSIZE_8_SUB_1_ : t_HaxInt := N.pow 2 8 - 1.
