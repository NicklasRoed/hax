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

Class t_Minimal `{v_Self : Type} : Type :=
  {
    f_minimal_method : v_Self -> bool;
  }.
Arguments t_Minimal:clear implicits.
Arguments t_Minimal (_).

Definition identity `{v_T : Type} `{t_Sized (v_T)} `{t_Minimal (v_T)} (x : v_T) : v_T :=
  x.

Instance t_Minimal_618094455 : t_Minimal ((t_i32)) :=
  {
    t_Minimal_f_minimal_method := fun (self : t_i32) =>
      true;
  }.

Definition main (_ : unit) : unit :=
  let x : t_i32 := 42 in
  let y := identity (x) in
  tt.

Class t_Creator `{v_Self : Type} : Type :=
  {
    f_create : t_i32 -> v_Self;
  }.
Arguments t_Creator:clear implicits.
Arguments t_Creator (_).

Definition factory `{v_T : Type} `{t_Sized (v_T)} `{t_Creator (v_T)} (x : t_i32) : v_T :=
  f_create (x).
