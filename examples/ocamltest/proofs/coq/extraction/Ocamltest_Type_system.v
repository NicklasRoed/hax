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

Record t_User : Type :=
  {
    f_id : t_u64;
    f_name : t_String;
    f_active : bool;
  }.
Arguments t_User:clear implicits.
Arguments t_User.
Arguments Build_t_User.
#[export] Instance settable_t_User : Settable _ :=
  settable! (@Build_t_User) <f_id; f_name; f_active>.

Record t_Point : Type :=
  {
    0 : t_i32;
    1 : t_i32;
    2 : t_i32;
  }.
Arguments t_Point:clear implicits.
Arguments t_Point.
Arguments Build_t_Point.
#[export] Instance settable_t_Point : Settable _ :=
  settable! (@Build_t_Point) <0; 1; 2>.

Record t_UnitStruct : Type :=
  {
  }.
Arguments t_UnitStruct:clear implicits.
Arguments t_UnitStruct.
Arguments Build_t_UnitStruct.
#[export] Instance settable_t_UnitStruct : Settable _ :=
  settable! (@Build_t_UnitStruct) <>.

Inductive t_Shape : Type :=
| Shape_Circle : float -> _
| Shape_Rectangle : float -> float -> _
| Shape_Square : float -> _
| f_a : floatf_b : floatf_c : float;
| Shape_Empty.
Arguments t_Shape:clear implicits.
Arguments t_Shape.

Inductive t_MyError : Type :=
| MyError_NotFound
| MyError_InvalidInput : t_String -> _
| f_code : t_i32f_message : t_String;.
Arguments t_MyError:clear implicits.
Arguments t_MyError.

Record t_Container `{v_T : Type} `{t_Sized (v_T)} : Type :=
  {
    f_value : v_T;
  }.
Arguments t_Container:clear implicits.
Arguments t_Container (_) {_}.
Arguments Build_t_Container {_} {_}.
#[export] Instance settable_t_Container `{v_T : Type} `{t_Sized (v_T)} : Settable _ :=
  settable! (@Build_t_Container `{v_T : Type} `{t_Sized (v_T)}) <f_value>.

Inductive t_Maybe `{v_T : Type} `{t_Sized (v_T)} : Type :=
| Maybe_Something : v_T -> _
| Maybe_Nothing.
Arguments t_Maybe:clear implicits.
Arguments t_Maybe (_) {_}.

Class t_Drawable `{v_Self : Type} : Type :=
  {
    f_draw : v_Self -> unit;
    f_Color : Type;
    _ :: `{t_Sized (f_Color)};
    f_get_color : v_Self -> f_Color;
  }.
Arguments t_Drawable:clear implicits.
Arguments t_Drawable (_).

Definition print_and_draw `{v_T : Type} `{t_Sized (v_T)} `{t_Drawable (v_T)} (item : v_T) : unit :=
  let _ := f_draw (item) in
  tt.

Definition process `{v_T : Type} `{v_U : Type} `{t_Sized (v_T)} `{t_Sized (v_U)} `{t_Drawable (v_T)} `{t_Fn (v_U) ((v_T))} `{_.(f_Output) = t_String} (item : v_T) (processor : v_U) : unit :=
  let result := f_call (processor) ((item)) in
  let _ := e_print (impl_2__new_v1 (["Processed: "%string; "
"%string]) ([impl_1__new_display (result)])) in
  let _ := tt in
  tt.

Instance t_Drawable_37603190 : t_Drawable ((t_Shape)) :=
  {
    t_Drawable_f_draw := fun (self : t_Shape) =>
      match self with
      | Shape_Circle (radius) =>
        let _ := e_print (impl_2__new_v1 (["Drawing circle with radius "%string; "
"%string]) ([impl_1__new_display (radius)])) in
        tt
      | Shape_Rectangle (width) (height) =>
        let _ := e_print (impl_2__new_v1 (["Drawing rectangle "%string; "x"%string; "
"%string]) ([impl_1__new_display (width); impl_1__new_display (height)])) in
        tt
      | Shape_Square (side) =>
        let _ := e_print (impl_2__new_v1 (["Drawing square with side "%string; "
"%string]) ([impl_1__new_display (side)])) in
        tt
      | Shape_Triangle (a, b, c) =>
        let _ := e_print (impl_2__new_v1 (["Drawing triangle with sides "%string; ", "%string; ", "%string; "
"%string]) ([impl_1__new_display (a); impl_1__new_display (b); impl_1__new_display (c)])) in
        tt
      | Shape_Empty =>
        let _ := e_print (impl_2__new_const (["Drawing nothing
"%string])) in
        tt
      end;
    t_Drawable_f_Color := t_String;
    t_Drawable_f_get_color := fun (self : t_Shape) =>
      f_from ("blue"%string);
  }.

Definition main (_ : unit) : unit :=
  let user := Build_t_User (1, f_from ("Alice"%string), true) in
  let user2 := Build_user (f_from ("Bob"%string)) in
  let origin := Build_t_Point (0, 0, 0) in
  let x := 0 origin in
  let unit := Build_t_UnitStruct in
  let circle := Shape_Circle (5.0%float) in
  let rect := Shape_Rectangle (4.0%float, 3.0%float) in
  let triangle := TODO: please implement the method `expr'_Construct_inductive [is_record=true, is_struct = false] todo record` in
  let _ := match match circle with
  | Shape_Circle (radius) =>
    match f_gt (radius) (10.0%float) with
    | true =>
      let _ := e_print (impl_2__new_const (["Large circle
"%string])) in
      Option_Some (tt)
    | _ =>
      Option_None
    end
  | _ =>
    Option_None
  end with
  | Option_Some (x) =>
    x
  | Option_None =>
    match circle with
    | Shape_Circle (radius) =>
      let _ := e_print (impl_2__new_v1 (["Circle with radius "%string; "
"%string]) ([impl_1__new_display (radius)])) in
      tt
    | Shape_Rectangle (w) (h) =>
      let _ := e_print (impl_2__new_v1 (["Rectangle "%string; "x"%string; "
"%string]) ([impl_1__new_display (w); impl_1__new_display (h)])) in
      tt
    | Shape_Square (s) =>
      let _ := e_print (impl_2__new_v1 (["Square with side "%string; "
"%string]) ([impl_1__new_display (s)])) in
      tt
    | Shape_Triangle (a, b, c) =>
      let _ := e_print (impl_2__new_v1 (["Triangle with sides "%string; ", "%string; ", "%string; "
"%string]) ([impl_1__new_display (a); impl_1__new_display (b); impl_1__new_display (c)])) in
      tt
    | Shape_Empty =>
      let _ := e_print (impl_2__new_const (["Empty shape
"%string])) in
      tt
    end
  end in
  let name := Option_Some (f_from ("Alice"%string)) in
  let empty_name : t_Option ((t_String)) := Option_None in
  let _ := match name with
  | Option_Some (n) =>
    let _ := e_print (impl_2__new_v1 (["Name: "%string; "
"%string]) ([impl_1__new_display (n)])) in
    tt
  | Option_None =>
    let _ := e_print (impl_2__new_const (["No name provided
"%string])) in
    tt
  end in
  let result : t_Result ((t_i32)) ((t_MyError)) := Result_Ok (42) in
  let error : t_Result ((t_i32)) ((t_MyError)) := Result_Err (MyError_NotFound) in
  let _ := match result with
  | Result_Ok (value) =>
    let _ := e_print (impl_2__new_v1 (["Success: "%string; "
"%string]) ([impl_1__new_display (value)])) in
    tt
  | Result_Err (MyError_NotFound) =>
    let _ := e_print (impl_2__new_const (["Not found error
"%string])) in
    tt
  | Result_Err (MyError_InvalidInput (msg)) =>
    let _ := e_print (impl_2__new_v1 (["Invalid input: "%string; "
"%string]) ([impl_1__new_display (msg)])) in
    tt
  | Result_Err (MyError_DatabaseError (code, message)) =>
    let _ := e_print (impl_2__new_v1 (["Database error "%string; ": "%string; "
"%string]) ([impl_1__new_display (code); impl_1__new_display (message)])) in
    tt
  end in
  let int_container := Build_t_Container (42) in
  let string_container := Build_t_Container (f_from ("hello"%string)) in
  let maybe_value := Maybe_Something (123) in
  let maybe_empty : t_Maybe ((t_i32)) := Maybe_Nothing in
  let _ := match maybe_value with
  | Maybe_Something (x) =>
    let _ := e_print (impl_2__new_v1 (["Got value: "%string; "
"%string]) ([impl_1__new_display (x)])) in
    tt
  | Maybe_Nothing =>
    let _ := e_print (impl_2__new_const (["Got nothing
"%string])) in
    tt
  end in
  let _ := f_draw (circle) in
  let _ := print_and_draw (circle) in
  let _ := process (rect) (fun shape =>
    match shape with
    | Shape_Rectangle (w) (h) =>
      let res := format (impl_2__new_v1 (["Area: "%string]) ([impl_1__new_display (f_mul (w) (h))])) in
      must_use (res)
    | _ =>
      f_from ("Not a rectangle"%string)
    end) in
  tt.
