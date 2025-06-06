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

From Core Require Import Core_Marker.
Export Core_Marker.

Class t_Concretization (v_Self : Type) (v_T : Type) `{t_Sized (v_T)} : Type :=
  {
    Concretization_f_concretize : v_Self -> v_T;
  }.
Arguments t_Concretization (_) (_) {_}.

Class t_Abstraction (v_Self : Type) : Type :=
  {
    Abstraction_f_AbstractType : Type;
    _ :: `{t_Sized (Abstraction_f_AbstractType)};
    Abstraction_f_lift : v_Self -> Abstraction_f_AbstractType;
  }.
Arguments t_Abstraction (_).
