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

From Core Require Import Core_Marker (t_Sized).
Export Core_Marker (t_Sized).

(* NotImplementedYet *)

(* NotImplementedYet *)

Class t_Add (v_Self : Type) (v_Rhs : Type) `{t_Sized (v_Rhs)} : Type :=
  {
    Add_f_Output : Type;
    _ :: `{t_Sized (Add_f_Output)};
    Add_f_add : v_Self -> v_Rhs -> Add_f_Output;
  }.
Arguments t_Add (_) (_) {_}.

Class t_Div (v_Self : Type) (v_Rhs : Type) `{t_Sized (v_Rhs)} : Type :=
  {
    Div_f_Output : Type;
    _ :: `{t_Sized (Div_f_Output)};
    Div_f_div : v_Self -> v_Rhs -> Div_f_Output;
  }.
Arguments t_Div (_) (_) {_}.

Class t_Mul (v_Self : Type) (v_Rhs : Type) `{t_Sized (v_Rhs)} : Type :=
  {
    Mul_f_Output : Type;
    _ :: `{t_Sized (Mul_f_Output)};
    Mul_f_mul : v_Self -> v_Rhs -> Mul_f_Output;
  }.
Arguments t_Mul (_) (_) {_}.

Class t_Neg (v_Self : Type) : Type :=
  {
    Neg_f_Output : Type;
    _ :: `{t_Sized (Neg_f_Output)};
    Neg_f_neg : v_Self -> Neg_f_Output;
  }.
Arguments t_Neg (_).

Class t_Rem (v_Self : Type) (v_Rhs : Type) `{t_Sized (v_Rhs)} : Type :=
  {
    Rem_f_Output : Type;
    _ :: `{t_Sized (Rem_f_Output)};
    Rem_f_rem : v_Self -> v_Rhs -> Rem_f_Output;
  }.
Arguments t_Rem (_) (_) {_}.

Class t_Sub (v_Self : Type) (v_Rhs : Type) `{t_Sized (v_Rhs)} : Type :=
  {
    Sub_f_Output : Type;
    _ :: `{t_Sized (Sub_f_Output)};
    Sub_f_sub : v_Self -> v_Rhs -> Sub_f_Output;
  }.
Arguments t_Sub (_) (_) {_}.
