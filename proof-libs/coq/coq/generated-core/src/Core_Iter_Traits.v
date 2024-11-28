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

(* TODO: Replace this dummy lib with core lib *)
Class t_Sized (T : Type) := { }.
Definition t_u8 := Z.
Definition t_u16 := Z.
Definition t_u32 := Z.
Definition t_u64 := Z.
Definition t_u128 := Z.
Definition t_usize := Z.
Definition t_i8 := Z.
Definition t_i16 := Z.
Definition t_i32 := Z.
Definition t_i64 := Z.
Definition t_i128 := Z.
Definition t_isize := Z.
Definition t_Array T (x : t_usize) := list T.
Definition t_String := string.
Definition ToString_f_to_string (x : string) := x.
Instance Sized_any : forall {t_A}, t_Sized t_A := {}.
Class t_Clone (T : Type) := { Clone_f_clone : T -> T }.
Instance Clone_any : forall {t_A}, t_Clone t_A := {Clone_f_clone := fun x => x}.
Definition t_Slice (T : Type) := list T.
Definition unsize {T : Type} : list T -> t_Slice T := id.
Definition t_PartialEq_f_eq x y := x =? y.
Definition t_Rem_f_rem (x y : Z) := x mod y.
Definition assert (b : bool) (* `{H_assert : b = true} *) : unit := tt.
Inductive globality := | t_Global.
Definition t_Vec T (_ : globality) : Type := list T.
Definition impl_1__append {T} l1 l2 : list T * list T := (app l1 l2, l2).
Definition impl_1__len {A} (l : list A) := Z.of_nat (List.length l).
Definition impl__new {A} (_ : Datatypes.unit) : list A := nil.
Definition impl__with_capacity {A} (_ : Z)  : list A := nil.
Definition impl_1__push {A} l (x : A) := cons x l.
Class t_From (A B : Type) := { From_f_from : B -> A }.
Definition impl__to_vec {T} (x : t_Slice T) : t_Vec T t_Global := x.
Class t_Into (A B : Type) := { Into_f_into : A -> B }.
Instance t_Into_from_t_From {A B : Type} `{H : t_From B A} : t_Into A B := { Into_f_into x := @From_f_from B A H x }.
Definition from_elem {A} (x : A) (l : Z) := repeat x (Z.to_nat l).
Definition t_Option := option.
Definition impl__map {A B} (x : t_Option A) (f : A -> B) : t_Option B := match x with | Some x => Some (f x) | None => None end.
Definition t_Add_f_add x y := x + y.
Class Cast A B := { cast : A -> B }.
Instance cast_t_u8_t_u32 : Cast t_u8 t_u32 := {| cast x := x |}.
(* / dummy lib *)





From Core Require Import Self_Collect (t_IntoIterator).
Export Self_Collect (t_IntoIterator).

From Core Require Import Self_Exact_size (t_ExactSizeIterator).
Export Self_Exact_size (t_ExactSizeIterator).

From Core Require Import Self_Iterator (t_Iterator).
Export Self_Iterator (t_Iterator).



From Core Require Import Self_Marker (t_FusedIterator).
Export Self_Marker (t_FusedIterator).

From Core Require Import Self_Marker (t_TrustedLen).
Export Self_Marker (t_TrustedLen).

From Core Require Import Self_Marker (t_TrustedStep).
Export Self_Marker (t_TrustedStep).

(* NotImplementedYet *)

(* NotImplementedYet *)

(* NotImplementedYet *)

(* NotImplementedYet *)
