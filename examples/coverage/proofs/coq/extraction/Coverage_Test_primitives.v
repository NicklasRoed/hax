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

Definition test_primtives (_ : unit) : unit :=
  let _ : bool := false in
  let _ : bool := true in
  let _ : t_u8 := 12 in
  let _ : t_u16 := 123 in
  let _ : t_u32 := 1234 in
  let _ : t_u64 := 12345 in
  let _ : t_u128 := 123456 in
  let _ : t_usize := 32 in
  let _ : t_i8 := -12 in
  let _ : t_i16 := 123 in
  let _ : t_i32 := -1234 in
  let _ : t_i64 := 12345 in
  let _ : t_i128 := 123456 in
  let _ : t_isize := -32 in
  let _ : float := 1.2%float in
  let _ : float := -1.23%float in
  let _ : ascii := "c"%char in
  let _ : string := "hello world"%string in
  tt.
