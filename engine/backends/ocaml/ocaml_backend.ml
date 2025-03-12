open Hax_engine
open Utils
open Base

include
  Backend.Make
    (struct
      open Features
      include Off
      include On.Slice
      include On.Monadic_binding
      include On.Macro
      include On.Construct_base
    end)
    (struct
      let backend = Diagnostics.Backend.Ocaml
    end)

module SubtypeToInputLanguage
    (FA : Features.T
            with type mutable_reference = Features.Off.mutable_reference
             and type continue = Features.Off.continue
             and type break = Features.Off.break
             and type mutable_reference = Features.Off.mutable_reference
             and type mutable_pointer = Features.Off.mutable_pointer
             and type mutable_variable = Features.Off.mutable_variable
             and type reference = Features.Off.reference
             and type raw_pointer = Features.Off.raw_pointer
             and type early_exit = Features.Off.early_exit
             and type question_mark = Features.Off.question_mark
             and type as_pattern = Features.Off.as_pattern
             and type lifetime = Features.Off.lifetime
             and type monadic_action = Features.Off.monadic_action
             and type arbitrary_lhs = Features.Off.arbitrary_lhs
             and type nontrivial_lhs = Features.Off.nontrivial_lhs
             and type block = Features.Off.block
             and type quote = Features.Off.quote
             and type dyn = Features.Off.dyn
             and type match_guard = Features.Off.match_guard
             and type trait_item_default = Features.Off.trait_item_default
             and type unsafe = Features.Off.unsafe
             and type loop = Features.Off.loop
             and type for_loop = Features.Off.for_loop
             and type while_loop = Features.Off.while_loop
             and type for_index_loop = Features.Off.for_index_loop
             and type state_passing_loop = Features.Off.state_passing_loop
             and type fold_like_loop = Features.Off.fold_like_loop) =
struct
  module FB = InputLanguage

  include
    Subtype.Make (FA) (FB)
      (struct
        module A = FA
        module B = FB
        include Features.SUBTYPE.Id
        include Features.SUBTYPE.On.Monadic_binding
        include Features.SUBTYPE.On.Construct_base
        include Features.SUBTYPE.On.Slice
        include Features.SUBTYPE.On.Macro
      end)

  let metadata = Phase_utils.Metadata.make (Reject (NotInBackendLang backend))
end

module AST = Ast.Make (InputLanguage)
module BackendOptions = Backend.UnitBackendOptions
open Ast
module OCamlNamePolicy = Concrete_ident.DefaultNamePolicy
module U = Ast_utils.Make (InputLanguage)
module RenderId = Concrete_ident.MakeRenderAPI (OCamlNamePolicy)
open AST

module BasePrinter = Generic_printer.Make (InputLanguage)

module Make
    (Default : sig
      val default : string -> string
    end)
    (Attrs : Attrs.WITH_ITEMS) =
struct
  open PPrint

  let default_string_for s = "TODO: please implement the method `" ^ s ^ "`"
  let default_document_for = default_string_for >> string
  let trim_string str =
    String.sub str 2 (String.length str - 2)

  type ('get_span_data, 'a) object_type =
    ('get_span_data, 'a) BasePrinter.Gen.object_type

  class printer =
    object (self)
      inherit BasePrinter.base

      (* BEGIN GENERATED *)
      method arm ~arm ~span:_ = arm#p

      method arm' ~super:_ ~arm_pat ~body ~guard:_ =
        arm_pat#p ^^ space ^^ string "->" ^^ nest 2 (break 1 ^^ body#p)

      method attrs x1 = default_document_for "attrs"

      method binding_mode_ByRef _x1 _x2 =
        default_document_for "binding_mode_ByRef"

      method binding_mode_ByValue = default_document_for "binding_mode_ByValue"
      method borrow_kind_Mut _x1 = default_document_for "borrow_kind_Mut"
      method borrow_kind_Shared = default_document_for "borrow_kind_Shared"
      method borrow_kind_Unique = default_document_for "borrow_kind_Unique"
      method cf_kind_BreakOnly = default_document_for "cf_kind_BreakOnly"

      method cf_kind_BreakOrReturn =
        default_document_for "cf_kind_BreakOrReturn"

      method common_array x1 = brackets (string "|" ^^ (separate (semi ^^ space) x1) ^^ string "|")

      method dyn_trait_goal ~trait:_ ~non_self_args:_ =
        default_document_for "dyn_trait_goal"

      method error_expr _x1 = default_document_for "error_expr"
      method error_item _x1 = default_document_for "error_item"
      method error_pat _x1 = default_document_for "error_pat"

      method expr ~e ~span:_ ~typ:_ = e#p

      method expr'_AddressOf ~super:_ ~mut:_ ~e:_ ~witness:_ =
        default_document_for "expr'_AddressOf"

      method expr'_App_application ~super:_ ~f ~args ~generics:_ =
        f#p ^^ concat_map (fun x -> space ^^ x#p) args

      method expr'_App_constant ~super:_ ~constant:_ ~generics:_ =
        default_document_for "expr'_App_constant"

      method expr'_App_field_projection ~super:_ ~field:_ ~e:_ =
        default_document_for "expr'_App_field_projection"

      method expr'_App_tuple_projection ~super:_ ~size:_ ~nth:_ ~e:_ =
        default_document_for "expr'_App_tuple_projection"

      method expr'_Ascription ~super:_ ~e ~typ =
        e#p ^^ string " : " ^^ typ#p

      method expr'_Assign ~super:_ ~lhs:_ ~e:_ ~witness:_ =
        default_document_for "expr'_Assign"

      method expr'_Block ~super:_ ~e:_ ~safety_mode:_ ~witness:_ =
        default_document_for "expr'_Block"

      method expr'_Borrow ~super:_ ~kind:_ ~e:_ ~witness:_ =
        default_document_for "expr'_Borrow"

      method expr'_Break ~super:_ ~e:_ ~acc:_ ~label:_ ~witness:_ =
        default_document_for "expr'_Break"

      method expr'_Closure ~super:_ ~params:_ ~body:_ ~captures:_ =
        default_document_for "expr'_Closure"

      method expr'_Construct_inductive ~super:_ ~constructor ~is_record ~is_struct ~fields ~base =
        let fields_or_empty add_space =
          if List.is_empty fields then empty
          else
            add_space
            ^^ parens
              (separate_map (comma ^^ space) (fun x -> (snd x)#p) fields)
        in
        if is_record && is_struct then
        (* In OCaml, records use curly braces with field assignments *)
          match base with
            | Some x -> 
              x#p ^^ space ^^ braces
              (separate_map (semi ^^ space) 
                  (fun (name, value) -> name#p ^^ space ^^ equals ^^ space ^^ value#p) 
                  fields)
            | None -> 
              constructor#p ^^ space ^^ braces
              (separate_map (semi ^^ space) 
                  (fun (name, value) -> 
                    let concrete_name = match name#v with
                      | `Concrete cid -> string (trim_string (RenderId.render cid).name) ^^ space ^^ equals ^^ space ^^ value#p
                      | _ -> string "Unexpected non-concrete identifier."
                    in concrete_name
                  ) fields)
        else if not is_record then
          constructor#p ^^ fields_or_empty space
        else
          string "Something very unexpected happened."


      method expr'_Construct_tuple ~super:_ ~components =
        if List.length components == 0 then !^"()"
        else parens (separate_map comma (fun x -> x#p) components)

      method expr'_Continue ~super:_ ~acc:_ ~label:_ ~witness:_ =
        default_document_for "expr'_Continue"

      method expr'_EffectAction ~super:_ ~action:_ ~argument:_ =
        default_document_for "expr'_EffectAction"

      method expr'_GlobalVar_concrete ~super:_ x2 = 
        let x = U.Reducers.collect_concrete_idents#visit_concrete_ident () x2#v in
        let concrete_idents_list = Set.elements x in
        let results = List.map 
          ~f:(fun concrete_id -> (RenderId.render concrete_id).name) concrete_idents_list 
        in
        match results with
        | [] -> empty  (* Handle empty list case *)
        | [single_element] -> string (single_element)  (* Special case for single element *)
        | multiple -> string ("MULTIPLE_IDENTIFIERS: " ^ String.concat ~sep:", " multiple)  (* Multiple elements case *)


      method expr'_GlobalVar_primitive ~super:_ x2 = 
        default_document_for "expr'_GlobalVar_primitive"

      method expr'_If ~super:_ ~cond ~then_ ~else_ =
        string "if" ^^ space ^^ cond#p ^^ space ^^ string "then"
        ^^ nest 2 (break 1 ^^ then_#p)
        ^^ break 1 ^^ string "else"
        ^^ nest 2 (break 1 ^^ match else_ with Some x -> x#p | None -> string "()")

      method expr'_Let ~super:_ ~monadic:_ ~lhs ~rhs ~body =
        string "let " ^^ lhs#p ^^ string " = " ^^ rhs#p 
        ^^ space ^^ string "in" ^^ break 1 ^^ body#p

      method expr'_Literal ~super:_ x2 = x2#p
      method expr'_LocalVar ~super:_ x2 = x2#p

      (** Purely functional or just the for-loop equivalent? **)
      method expr'_Loop ~super:_ ~body:_ ~kind:_ ~state:_ ~control_flow:_
          ~label:_ ~witness:_ =
        default_document_for "expr'_Loop"

      method expr'_MacroInvokation ~super:_ ~macro:_ ~args:_ ~witness:_ =
        default_document_for "expr'_MacroInvokation"

      method expr'_Match ~super:_ ~scrutinee ~arms =
        string "match" ^^ space ^^ scrutinee#p ^^ space ^^ string "with"
        ^^ break 1
        ^^ concat_map (fun x -> string "|" ^^ space ^^ x#p ^^ break 1) arms

      method expr'_QuestionMark ~super:_ ~e:_ ~return_typ:_ ~witness:_ =
        default_document_for "expr'_QuestionMark"

      method expr'_Quote ~super:_ _x2 = default_document_for "expr'_Quote"

      method expr'_Return ~super:_ ~e:_ ~witness:_ =
        default_document_for "expr'_Return"

      method field_pat ~field:_ ~pat:_ = default_document_for "field_pat"

      method generic_constraint_GCLifetime _x1 _x2 =
        default_document_for "generic_constraint_GCLifetime"

      method generic_constraint_GCProjection _x1 =
        default_document_for "generic_constraint_GCProjection"

      method generic_constraint_GCType _x1 =
        default_document_for "generic_constraint_GCType"

      method generic_param ~ident:_ ~span:_ ~attrs:_ ~kind:_ =
        default_document_for "generic_param"

      method generic_param_kind_GPConst ~typ:_ =
        default_document_for "generic_param_kind_GPConst"

      method generic_param_kind_GPLifetime ~witness:_ =
        default_document_for "generic_param_kind_GPLifetime"

      method generic_param_kind_GPType =
        default_document_for "generic_param_kind_GPType"

      method generic_value_GConst _x1 =
        default_document_for "generic_value_GConst"

      method generic_value_GLifetime ~lt:_ ~witness:_ =
        default_document_for "generic_value_GLifetime"

      method generic_value_GType _x1 =
        default_document_for "generic_value_GType"

      method generics ~params ~constraints =
        if List.is_empty params then empty
        else
          angles (separate_map comma (fun p -> p#p) params)

      method guard ~guard:_ ~span:_ = default_document_for "guard"

      method guard'_IfLet ~super:_ ~lhs:_ ~rhs:_ ~witness:_ =
        default_document_for "guard'_IfLet"

      method impl_expr ~kind:_ ~goal:_ = default_document_for "impl_expr"

      method impl_expr_kind_Builtin _x1 =
        default_document_for "impl_expr_kind_Builtin"

      method impl_expr_kind_Concrete _x1 =
        default_document_for "impl_expr_kind_Concrete"

      method impl_expr_kind_Dyn = default_document_for "impl_expr_kind_Dyn"

      method impl_expr_kind_ImplApp ~impl:_ ~args:_ =
        default_document_for "impl_expr_kind_ImplApp"

      method impl_expr_kind_LocalBound ~id:_ =
        default_document_for "impl_expr_kind_LocalBound"

      method impl_expr_kind_Parent ~impl:_ ~ident:_ =
        default_document_for "impl_expr_kind_Parent"

      method impl_expr_kind_Projection ~impl:_ ~item:_ ~ident:_ =
        default_document_for "impl_expr_kind_Projection"

      method impl_expr_kind_Self = default_document_for "impl_expr_kind_Self"
      method impl_ident ~goal:_ ~name:_ = default_document_for "impl_ident"

      method impl_item ~ii_span:_ ~ii_generics:_ ~ii_v:_ ~ii_ident:_ ~ii_attrs:_
          =
        default_document_for "impl_item"

      method impl_item'_IIFn ~body:_ ~params:_ =
        default_document_for "impl_item'_IIFn"

      method impl_item'_IIType ~typ:_ ~parent_bounds:_ =
        default_document_for "impl_item'_IIType"

      method item ~v ~span:_ ~ident:_ ~attrs:_ = v#p ^^ break 1

      method item'_Alias ~super:_ ~name:_ ~item:_ =
        default_document_for "item'_Alias"

      method item'_Enum_Variant ~name:_ ~arguments:_ ~is_record:_ ~attrs:_ =
        default_document_for "item'_Enum_Variant"

      method item'_Fn ~super ~name ~generics ~body ~params ~safety:_ =
        let is_rec =
          Set.mem
            (U.Reducers.collect_concrete_idents#visit_expr () body#v)
            name#v
        in
        let typ =
          self#_do_not_override_lazy_of_ty AstPos_item'_Fn_body body#v.typ
        in
        let has_params = not (List.is_empty (List.filter 
        ~f:(fun x -> 
            match x#v.pat.p with
            | PWild -> false
            | _ -> true) 
          params)
        ) 
        in
        let params_doc = if has_params 
          then separate_map space (fun p -> p#p) params
        else parens empty 
        in
        let keyword = string (if is_rec then "let rec" else "let") in
        let generics_doc = generics#p in
        keyword ^^ space ^^ name#p ^^ generics_doc ^^ space ^^ params_doc ^^
        space ^^ string "->" ^^ space ^^ typ#p ^^ space ^^ equals ^^ 
        nest 2 (break 1 ^^ body#p)
    
      method item'_HaxError ~super:_ _x2 = default_document_for "item'_HaxError"

      method item'_IMacroInvokation ~super:_ ~macro:_ ~argument:_ ~span:_
          ~witness:_ =
        default_document_for "item'_IMacroInvokation"

      method item'_Impl ~super:_ ~generics:_ ~self_ty:_ ~of_trait:_ ~items:_
          ~parent_bounds:_ ~safety:_ =
        default_document_for "item'_Impl"

      method item'_NotImplementedYet =
        string "failwith NotImplementedYet"

      method item'_Quote ~super:_ ~quote:_ ~origin:_ =
        default_document_for "item'_Quote"

      method item'_Trait ~super:_ ~name:_ ~generics:_ ~items:_ ~safety:_ =
        default_document_for "item'_Trait"

      method item'_TyAlias ~super:_ ~name:_ ~generics:_ ~ty:_ =
        default_document_for "item'_TyAlias"

      method item'_Type_enum ~super:_ ~name:_ ~generics:_ ~variants:_ =
        default_document_for "item'_Type_enum"

      method item'_Type_struct ~super:_ ~name ~generics ~tuple_struct
      ~arguments =
        let types = List.map ~f:(
          fun x ->
            let snd_elem = match x with
              | (_, s, _) -> s#p
            in snd_elem) arguments
          in
          let trimmed_name_str = 
            (trim_string (RenderId.render name#v).name)
          in
          let trimmed_var_name_str =
            String.make 1 (Char.lowercase (String.get trimmed_name_str 0))
            ^ String.sub trimmed_name_str 1 (String.length trimmed_name_str - 1)
          in
          let trimmed_name = string trimmed_name_str in
          let trimmed_var_name = string trimmed_var_name_str in
          match tuple_struct with
          | true ->
            string "type" ^^ space ^^ trimmed_var_name ^^ space ^^ equals ^^ space
            ^^ trimmed_name ^^ space ^^ string "of" ^^ space ^^ separate (space ^^ star ^^ space) types
          | false ->
            let idents = List.map ~f:(
              fun x -> 
                let first_elem = match x with
                  | (f, _, _) -> string (trim_string (RenderId.render f#v).name)
                in first_elem) arguments
            in
            let field_definitions = 
              match List.map2 ~f:(fun id typ -> nest 2 (break 1 ^^ id ^^ string ": " ^^ typ ^^ semi)) idents types with
              | Ok result -> 
                string "type" ^^ space ^^ trimmed_name ^^ space ^^ equals ^^ space ^^ lbrace ^^
                concat result ^^ break 1 ^^ rbrace
              | Unequal_lengths -> string "GG"
            in 
            field_definitions

      method item'_Use ~super:_ ~path ~is_external ~rename:_ =
        if List.length path = 0 || is_external then empty
        else
          let module_name =
            match path with
            | "crate" :: xs -> 
                String.capitalize (Option.value ~default:"Crate" 
                  (Option.bind current_namespace ~f:List.hd)) ^
                (if List.length xs > 0 then "." else "") ^
                String.concat ~sep:"." (List.map ~f:String.capitalize xs)
            | "super" :: xs ->
                let parent_namespace = 
                  Option.value ~default:[] 
                    (Option.bind current_namespace ~f:List.tl)
                in
                String.concat ~sep:"." (List.map ~f:String.capitalize 
                  ((List.drop_last_exn parent_namespace) @ xs))
            | [ a ] -> 
                String.capitalize a
            | xs -> 
                String.concat ~sep:"." (List.map ~f:String.capitalize xs)
          in
          if String.is_empty module_name then empty
          else
            string "open" ^^ space ^^ string module_name

      method item_quote_origin ~item_kind:_ ~item_ident:_ ~position:_ =
        default_document_for "item_quote_origin"

      method lhs_LhsArbitraryExpr ~e:_ ~witness:_ =
        default_document_for "lhs_LhsArbitraryExpr"

      method lhs_LhsArrayAccessor ~e:_ ~typ:_ ~index:_ ~witness:_ =
        default_document_for "lhs_LhsArrayAccessor"

      method lhs_LhsFieldAccessor_field ~e:_ ~typ:_ ~field:_ ~witness:_ =
        default_document_for "lhs_LhsFieldAccessor_field"

      method lhs_LhsFieldAccessor_tuple ~e:_ ~typ:_ ~nth:_ ~size:_ ~witness:_ =
        default_document_for "lhs_LhsFieldAccessor_tuple"

      method lhs_LhsLocalVar ~var:_ ~typ:_ =
        default_document_for "lhs_LhsLocalVar"

      method literal_Bool x1 = string (Bool.to_string x1)
      method literal_Char x1 = string "'" ^^ string (Char.to_string x1) ^^ string "'"

      method literal_Float ~value ~negative ~kind:_ =
        (if negative then !^"-" else empty) ^^ string value

      method literal_Int ~value ~negative ~kind:_ =
        (if negative then !^"-" else empty) ^^ string value

      method literal_String x1 = string "\"" ^^ string x1 ^^ string "\""

      method loop_kind_ForIndexLoop ~start:_ ~end_:_ ~var:_ ~var_typ:_
          ~witness:_ =
        default_document_for "loop_kind_ForIndexLoop"

      method loop_kind_ForLoop ~pat:_ ~it:_ ~witness:_ =
        default_document_for "loop_kind_ForLoop"

      method loop_kind_UnconditionalLoop =
        default_document_for "loop_kind_UnconditionalLoop"

      method loop_kind_WhileLoop ~condition:_ ~witness:_ =
        default_document_for "loop_kind_WhileLoop"

      method loop_state ~init:_ ~bpat:_ ~witness:_ =
        default_document_for "loop_state"

      method modul x1 = separate_map (break 1) (fun x -> x#p) x1

      method param ~pat ~typ ~typ_span:_ ~attrs:_ =
        string "~" ^^ pat#p ^^ string ": " ^^ typ#p

      method pat ~p ~span:_ ~typ:_ = p#p

      method pat'_PAscription ~super:_ ~typ ~typ_span:_ ~pat =
        pat#p ^^ string " : " ^^ typ#p

      method pat'_PBinding ~super:_ ~mut:_ ~mode:_ ~var ~typ:_ ~subpat:_ =
        var#p

      method pat'_PConstant ~super:_ ~lit =
        lit#p

      method pat'_PConstruct_inductive ~super:_ ~constructor:_ ~is_record:_
          ~is_struct:_ ~fields:_ =
        default_document_for "pat'_PConstruct_inductive"

      method pat'_PConstruct_tuple ~super:_ ~components:_ =
        default_document_for "pat'_PConstruct_tuple"

      method pat'_PDeref ~super:_ ~subpat:_ ~witness:_ =
        default_document_for "pat'_PDeref"

      method pat'_PWild = string "_"
      method printer_name = default_string_for "printer_name"

      method projection_predicate ~impl:_ ~assoc_item:_ ~typ:_ =
        default_document_for "projection_predicate"

      method safety_kind_Safe = default_document_for "safety_kind_Safe"
      method safety_kind_Unsafe _x1 = default_document_for "safety_kind_Unsafe"

      method supported_monads_MException _x1 =
        default_document_for "supported_monads_MException"

      method supported_monads_MOption =
        default_document_for "supported_monads_MOption"

      method supported_monads_MResult _x1 =
        default_document_for "supported_monads_MResult"

      method trait_goal ~trait:_ ~args:_ = default_document_for "trait_goal"

      method trait_item ~ti_span:_ ~ti_generics:_ ~ti_v:_ ~ti_ident:_
          ~ti_attrs:_ =
        default_document_for "trait_item"

      method trait_item'_TIDefault ~params:_ ~body:_ ~witness:_ =
        default_document_for "trait_item'_TIDefault"

      method trait_item'_TIFn _x1 = default_document_for "trait_item'_TIFn"
      method trait_item'_TIType _x1 = default_document_for "trait_item'_TIType"

      method ty_TApp_application ~typ ~generics =
       typ#p ^^ concat_map (fun x -> space ^^ parens x#p) generics

      method ty_TApp_tuple ~types = 
        if List.length types == 0 then string "unit"
        else parens (separate_map (comma ^^ space) (fun x -> self#entrypoint_ty x) types)

      method ty_TArray ~typ ~length:_ = typ#p ^^ space ^^ string "array" 
      method ty_TArrow x1 x2 = 
        concat_map (fun x -> x#p ^^ space ^^ string "->" ^^ space) x1 ^^ x2#p

      method ty_TAssociatedType ~impl:_ ~item:_ =
        default_document_for "ty_TAssociatedType"

      method ty_TBool = string "bool"
      method ty_TChar = string "char"
      method ty_TDyn ~witness:_ ~goals:_ = default_document_for "ty_TDyn"
      method ty_TFloat _x1 = string "float"
      method ty_TInt x1 = string "int" (*PLACEHOLDER -- WAITING FOR GUIDANCE*)
      method ty_TOpaque x1 = x1#p
      method ty_TParam x1 = x1#p
      method ty_TRawPointer ~witness:_ = default_document_for "ty_TRawPointer"

      method ty_TRef ~witness:_ ~region:_ ~typ:_ ~mut:_ =
        default_document_for "ty_TRef"

      method ty_TSlice ~witness:_ ~ty:_ = default_document_for "ty_TSlice"
      method ty_TStr = string "string"
      (* END GENERATED *)
    end

  let new_printer : BasePrinter.finalized_printer =
    BasePrinter.finalize (fun () -> (new printer :> BasePrinter.printer))
end

module type S = sig
  val new_printer : BasePrinter.finalized_printer
end

let make (module M : Attrs.WITH_ITEMS) =
  let open (
    Make
      (struct
        let default x = x
      end)
      (M) :
      S) in
  new_printer

let translate m _ ~bundles:_ (items : AST.item list) : Types.file list =
  let my_printer = make m in
  U.group_items_by_namespace items
  |> Map.to_alist
  |> List.filter_map ~f:(fun (_, items) ->
         let* first_item = List.hd items in
         Some ((RenderId.render first_item.ident).path, items))
  |> List.map ~f:(fun (ns, items) ->
         let mod_name =
           String.concat ~sep:"_"
             (List.map ~f:(map_first_letter String.uppercase) ns)
         in
         let sourcemap, contents =
           let annotated = my_printer#entrypoint_modul items in
           let open Generic_printer.AnnotatedString in
           (to_sourcemap annotated, to_string annotated)
         in
         let sourcemap = Some sourcemap in
         let path = mod_name ^ ".ml" in
         Types.{ path; contents; sourcemap })

open Phase_utils

module TransformToInputLanguage =
  [%functor_application
  Phases.Reject.Unsafe(Features.Rust)
  |> Phases.Reject.RawOrMutPointer
  |> Phases.And_mut_defsite
  |> Phases.Reconstruct_asserts
  |> Phases.Reconstruct_for_loops
  |> Phases.Direct_and_mut
  |> Phases.Reject.Arbitrary_lhs
  |> Phases.Drop_blocks
  |> Phases.Drop_match_guards
  |> Phases.Reject.Continue
  |> Phases.Drop_references
  |> Phases.Trivialize_assign_lhs
  |> Phases.Reconstruct_question_marks
  (* |> Side_effect_utils.Hoist *)
  |> Phases.Local_mutation
  |> Phases.Reject.Continue
  |> Phases.Cf_into_monads 
  |> Phases.Reject.EarlyExit
  |> Phases.Functionalize_loops
  |> Phases.Reject.As_pattern
  |> Phases.Reject.Dyn
  |> Phases.Reject.Trait_item_default
  |> Phases.Bundle_cycles
  |> Phases.Sort_items
  |> SubtypeToInputLanguage
  |> Identity
  ]
  [@ocamlformat "disable"] 


let apply_phases (_bo : BackendOptions.t) (items : Ast.Rust.item list) :
    AST.item list =
  TransformToInputLanguage.ditems items
