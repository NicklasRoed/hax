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

(* NotImplementedYet *)

Definition first `{v_A : Type} `{v_B : Type} `{t_Sized (v_A)} `{t_Sized (v_B)} `{t_Clone (v_B)} ((value,_) : (v_A*t_i32)) (y : v_B) : v_A :=
  value.

Definition foo1 `{v_A : Type} `{v_B : Type} `{t_Sized (v_A)} `{t_Sized (v_B)} (x : v_A) (y : v_B) : unit :=
  tt.

Definition foo2 `{v_T : Type} `{t_Sized (v_T)} `{t_Clone (v_T)} (x : t_Slice v_T) (y : t_Array (v_T) (1)) : unit :=
  tt.

Definition test (_ : unit) : unit :=
  let x := [1] in
  let _ := foo2 (unsize (x)) (x) in
  let _ := foo2 (unsize ([1; 2])) (x) in
  tt.

Definition foo3 (_ : unit) : unit :=
  tt.
