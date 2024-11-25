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

From Core Require Import Core_Primitive.
Export Core_Primitive.

From Core Require Import Core_Iter (t_Iterator).
Export Core_Iter (t_Iterator).

Record t_IndexRange : Type :=
  {
    IndexRange_f_start : t_usize;
    IndexRange_f_end : t_usize;
  }.
Arguments Build_t_IndexRange.
Arguments IndexRange_f_start.
Arguments IndexRange_f_end.
#[export] Instance settable_t_IndexRange : Settable _ :=
  settable! (Build_t_IndexRange) <IndexRange_f_start; IndexRange_f_end>.

Definition impl__IndexRange__zero_to (v_end : t_usize) : t_IndexRange :=
  Build_t_IndexRange (0) (v_end).

Definition impl__IndexRange__next_unchecked (self : t_IndexRange) : (t_IndexRange*t_usize) :=
  let value := IndexRange_f_start self in
  let self := self <|IndexRange_f_start := t_Add_f_add (value) (1) |> in
  let hax_temp_output := value in
  (self,hax_temp_output).

Definition impl__IndexRange__len (self : t_IndexRange) : t_usize :=
  sub (IndexRange_f_end self) (IndexRange_f_start self).

Instance t_Iterator_538767852 : t_Iterator ((t_IndexRange)) :=
  {
    Iterator_impl_1_f_Item := t_usize;
    Iterator_impl_1_f_next := fun  (self : t_IndexRange)=>
      let hax_temp_output := never_to_any (panic_fmt (impl_2__new_v1 (["not yet implemented: specification needed"%string]) (impl_1__none (tt)))) in
      (self,hax_temp_output);
    Iterator_impl_1_f_size_hint := fun  (self : t_IndexRange)=>
      let len := impl__IndexRange__len (self) in
      (len,Option_Some (len));
    Iterator_impl_1_f_fold := fun  (self : t_IndexRange) (init : v_B) (f : v_F)=>
      never_to_any (panic_fmt (impl_2__new_v1 (["not yet implemented: specification needed"%string]) (impl_1__none (tt))));
  }.

Instance t_ExactSizeIterator_661616782 : t_ExactSizeIterator ((t_IndexRange)) :=
  {
    ExactSizeIterator_impl_2_f_len := fun  (self : t_IndexRange)=>
      impl__IndexRange__len (self);
  }.
