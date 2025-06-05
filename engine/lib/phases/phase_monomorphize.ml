open! Prelude

module%inlined_contents Make (FA : Features.T) = struct
  open Ast

  module FB = struct
    include FA
    include Features.Off.Trait_impls
  end

  include
    Phase_utils.MakeBase (FA) (FB)
      (struct
        let phase_id = [%auto_phase_name auto]
      end)

  module A = Ast.Make (FA)
  module B = Ast.Make (FB)
  module AVisitors = Ast_visitors.Make (FA)
  module BVisitors = Ast_visitors.Make (FB)

  module OCamlNamePolicy = Concrete_ident.DefaultNamePolicy
  module RenderId = Concrete_ident.MakeRenderAPI (OCamlNamePolicy)

  module Implem : ImplemT.T = struct
    let metadata = metadata

    module S = struct
      include Features.SUBTYPE.Id
    end

    module UA = Ast_utils.Make (FA)
    module UB = Ast_utils.Make (FB)
    
    let dsafety_kind (span : Span.t) (safety : A.safety_kind) : B.safety_kind =
    match safety with Safe -> Safe | Unsafe w -> Unsafe (S.unsafe span w)

    let dmutability (span : Span.t) (type a b) (s : Span.t -> a -> b)
        (mutability : a mutability) : b mutability =
      match mutability with
      | Mutable w -> Mutable (s span w)
      | Immutable -> Immutable

    let rec dty (span : span) (ty : A.ty) : B.ty =
      match ty with
      | TBool -> TBool
      | TChar -> TChar
      | TInt k -> TInt k
      | TFloat k -> TFloat k
      | TStr -> TStr
      | TApp { ident; args } ->
          TApp { ident; args = List.map ~f:(dgeneric_value span) args }
      | TArray { typ; length } ->
          TArray { typ = dty span typ; length = dexpr length }
      | TSlice { witness; ty } ->
          TSlice { witness = S.slice span witness; ty = dty span ty }
      | TRef { witness; typ; mut; region } ->
          TRef
            {
              witness = S.reference span witness;
              typ = dty span typ;
              mut = dmutability span S.mutable_reference mut;
              region;
            }
      | TParam local_ident -> TParam local_ident
      | TArrow (inputs, output) ->
          TArrow (List.map ~f:(dty span) inputs, dty span output)
      | TAssociatedType { impl; item } ->
          TAssociatedType { impl = dimpl_expr span impl; item }
      | TOpaque ident -> TOpaque ident
      | TRawPointer { witness } ->
          TRawPointer { witness = S.raw_pointer span witness }
      | TDyn { witness; goals } ->
          TDyn
            {
              witness = S.dyn span witness;
              goals = List.map ~f:(ddyn_trait_goal span) goals;
            }

    and ddyn_trait_goal (span : span) (r : A.dyn_trait_goal) : B.dyn_trait_goal =
      {
        trait = r.trait;
        non_self_args = List.map ~f:(dgeneric_value span) r.non_self_args;
      }

    and dtrait_goal (span : span) (r : A.trait_goal) : B.trait_goal =
      { trait = r.trait; args = List.map ~f:(dgeneric_value span) r.args }

    and dimpl_ident (span : span) (r : A.impl_ident) : B.impl_ident =
      { goal = dtrait_goal span r.goal; name = r.name }

    and dprojection_predicate (span : span) (r : A.projection_predicate) :
        B.projection_predicate =
      {
        impl = dimpl_expr span r.impl;
        assoc_item = r.assoc_item;
        typ = dty span r.typ;
      }

    and dimpl_expr (span : span) (i : A.impl_expr) : B.impl_expr =
      { kind = dimpl_expr_kind span i.kind; goal = dtrait_goal span i.goal }

    and dimpl_expr_kind (span : span) (i : A.impl_expr_kind) : B.impl_expr_kind =
      match i with
      | Self -> Self
      | Concrete tr -> Concrete (dtrait_goal span tr)
      | LocalBound { id } -> LocalBound { id }
      | Parent { impl; ident } ->
          Parent { impl = dimpl_expr span impl; ident = dimpl_ident span ident }
      | Projection { impl; item; ident } ->
          Projection
            { impl = dimpl_expr span impl; item; ident = dimpl_ident span ident }
      | ImplApp { impl; args } ->
          ImplApp
            {
              impl = dimpl_expr span impl;
              args = List.map ~f:(dimpl_expr span) args;
            }
      | Dyn -> Dyn
      | Builtin tr -> Builtin (dtrait_goal span tr)

    and dgeneric_value (span : span) (generic_value : A.generic_value) :
        B.generic_value =
      match generic_value with
      | GLifetime { lt; witness } ->
          GLifetime { lt; witness = S.lifetime span witness }
      | GType t -> GType (dty span t)
      | GConst e -> GConst (dexpr e)

    and dborrow_kind (span : span) (borrow_kind : A.borrow_kind) : B.borrow_kind =
      match borrow_kind with
      | Shared -> Shared
      | Unique -> Unique
      | Mut witness -> Mut (S.mutable_reference span witness)

    and dpat (p : A.pat) : B.pat =
      { p = dpat' p.span p.p; span = p.span; typ = dty p.span p.typ }

    and dpat' (span : span) (pat : A.pat') : B.pat' =
      match pat with
      | PWild -> PWild
      | PAscription { typ; typ_span; pat } ->
          PAscription { typ = dty span typ; pat = dpat pat; typ_span }
      | PConstruct { constructor; is_record; is_struct; fields } ->
          PConstruct
            {
              constructor;
              is_record;
              is_struct;
              fields = List.map ~f:(dfield_pat span) fields;
            }
      | POr { subpats } -> POr { subpats = List.map ~f:dpat subpats }
      | PArray { args } -> PArray { args = List.map ~f:dpat args }
      | PConstant { lit } -> PConstant { lit }
      | PBinding { mut; mode; var : Local_ident.t; typ; subpat } ->
          PBinding
            {
              mut = dmutability span S.mutable_variable mut;
              mode = dbinding_mode span mode;
              var;
              typ = dty span typ;
              subpat = Option.map ~f:(dpat *** S.as_pattern span) subpat;
            }
      | PDeref { subpat; witness } ->
          PDeref { subpat = dpat subpat; witness = S.reference span witness }

    and dfield_pat (_span : span) (p : A.field_pat) : B.field_pat =
      { field = p.field; pat = dpat p.pat }

    and dbinding_mode (span : span) (binding_mode : A.binding_mode) :
        B.binding_mode =
      match binding_mode with
      | ByValue -> ByValue
      | ByRef (kind, witness) ->
         ByRef (dborrow_kind span kind, S.reference span witness)

    and dsupported_monads (span : span) (m : A.supported_monads) :
        B.supported_monads =
      match m with
      | MException t -> MException (dty span t)
      | MResult t -> MResult (dty span t)
      | MOption -> MOption

    and dexpr (e : A.expr) : B.expr =
      try dexpr_unwrapped e
      with Diagnostics.SpanFreeError.Exn (Data (context, kind)) ->
        let typ : B.ty =
          try dty e.span e.typ
          with Diagnostics.SpanFreeError.Exn (Data (_context, _kind)) ->
            UB.hax_failure_typ
        in
        UB.hax_failure_expr e.span typ (context, kind) (UA.LiftToFullAst.expr e)

    and dexpr_unwrapped (e : A.expr) : B.expr =
      { e = dexpr' e.span e.e; span = e.span; typ = dty e.span e.typ }

    and dexpr' (span : span) (expr : A.expr') : B.expr' =
      match expr with
      | If { cond; then_; else_ } ->
          If
            {
              cond = dexpr cond;
              then_ = dexpr then_;
              else_ = Option.map ~f:dexpr else_;
            }
      | App { f; args; generic_args; bounds_impls; trait } ->
          let dgeneric_values = List.map ~f:(dgeneric_value span) in
          App
            {
              f = dexpr f;
              args = List.map ~f:dexpr args;
              generic_args = dgeneric_values generic_args;
              bounds_impls = List.map ~f:(dimpl_expr span) bounds_impls;
              trait = Option.map ~f:(dimpl_expr span *** dgeneric_values) trait;
            }
      | Literal lit -> Literal lit
      | Array l -> Array (List.map ~f:dexpr l)
      | Construct { constructor; is_record; is_struct; fields; base } ->
          Construct
            {
              constructor;
              is_record;
              is_struct;
              fields = List.map ~f:(map_snd dexpr) fields;
              base = Option.map ~f:(dexpr *** S.construct_base span) base;
            }
      | Match { scrutinee; arms } ->
          Match { scrutinee = dexpr scrutinee; arms = List.map ~f:darm arms }
      | Let { monadic; lhs; rhs; body } ->
          Let
            {
              monadic =
                Option.map
                  ~f:(dsupported_monads span *** S.monadic_binding span)
                  monadic;
              lhs = dpat lhs;
              rhs = dexpr rhs;
              body = dexpr body;
            }
      | Block { e; safety_mode; witness } ->
          Block
            {
              e = dexpr e;
              safety_mode = dsafety_kind span safety_mode;
              witness = S.block span witness;
            }
      | LocalVar local_ident -> LocalVar local_ident
      | GlobalVar global_ident -> GlobalVar global_ident
      | Ascription { e; typ } -> Ascription { e = dexpr e; typ = dty span typ }
      | MacroInvokation { macro; args; witness } ->
          MacroInvokation { macro; args; witness = S.macro span witness }
      | Assign { lhs; e; witness } ->
          Assign
            {
              lhs = dlhs span lhs;
              e = dexpr e;
              witness = S.mutable_variable span witness;
            }
      | Loop { body; kind; state; label; witness; control_flow } ->
          Loop
            {
              body = dexpr body;
              kind = dloop_kind span kind;
              state = Option.map ~f:(dloop_state span) state;
              label;
              control_flow =
                Option.map
                  ~f:
                    ((function
                       | A.BreakOnly -> B.BreakOnly
                      | A.BreakOrReturn -> B.BreakOrReturn)
                    *** S.fold_like_loop span)
                  control_flow;
              witness = S.loop span witness;
            }
      | Break { e; acc; label; witness } ->
          Break
            {
              e = dexpr e;
              acc = Option.map ~f:(dexpr *** S.state_passing_loop span) acc;
              label;
              witness = (S.break span *** S.loop span) witness;
            }
      | Return { e; witness } ->
          Return { e = dexpr e; witness = S.early_exit span witness }
      | QuestionMark { e; return_typ; witness } ->
          QuestionMark
            {
              e = dexpr e;
              return_typ = dty span return_typ;
              witness = S.question_mark span witness;
            }
      | Continue { acc; label; witness = w1, w2 } ->
          Continue
            {
              acc = Option.map ~f:(dexpr *** S.state_passing_loop span) acc;
              label;
              witness = (S.continue span w1, S.loop span w2);
            }
      | Borrow { kind; e; witness } ->
          Borrow
            {
              kind = dborrow_kind span kind;
              e = dexpr e;
              witness = S.reference span witness;
            }
      | EffectAction { action; argument } ->
          EffectAction
            { action = S.monadic_action span action; argument = dexpr argument }
      | AddressOf { mut; e; witness } ->
          AddressOf
            {
              mut = dmutability span S.mutable_pointer mut;
              e = dexpr e;
              witness = S.raw_pointer span witness;
            }
      | Closure { params; body; captures } ->
          Closure
            {
              params = List.map ~f:dpat params;
              body = dexpr body;
              captures = List.map ~f:dexpr captures;
            }
      | Quote quote -> Quote (dquote span quote)

    and dquote (span : span) ({ contents; witness } : A.quote) : B.quote =
      let f = function
        | A.Verbatim code -> B.Verbatim code
        | Expr e -> Expr (dexpr e)
        | Pattern p -> Pattern (dpat p)
        | Typ p -> Typ (dty span p)
      in
      { contents = List.map ~f contents; witness = S.quote span witness }

    and dloop_kind (span : span) (k : A.loop_kind) : B.loop_kind =
      match k with
      | UnconditionalLoop -> UnconditionalLoop
      | WhileLoop { condition; witness } ->
          WhileLoop
            { condition = dexpr condition; witness = S.while_loop span witness }
      | ForLoop { it; pat; witness } ->
          ForLoop
            { it = dexpr it; pat = dpat pat; witness = S.for_loop span witness }
      | ForIndexLoop { start; end_; var; var_typ; witness } ->
          ForIndexLoop
            {
              start = dexpr start;
              end_ = dexpr end_;
              var;
              var_typ = dty span var_typ;
              witness = S.for_index_loop span witness;
            }

    and dloop_state (span : span) (s : A.loop_state) : B.loop_state =
      {
        init = dexpr s.init;
        bpat = dpat s.bpat;
        witness = S.state_passing_loop span s.witness;
      }

    and darm (a : A.arm) : B.arm = { span = a.span; arm = darm' a.arm }

    and darm' (a : A.arm') : B.arm' =
    {
      arm_pat = dpat a.arm_pat;
      body = dexpr a.body;
      guard = Option.map ~f:dguard a.guard;
    }

    and dguard (a : A.guard) : B.guard =
      { span = a.span; guard = dguard' a.span a.guard }

    and dguard' (span : span) (guard : A.guard') : B.guard' =
      match guard with
      | IfLet { lhs; rhs; witness } ->
          IfLet
            {
              lhs = dpat lhs;
              rhs = dexpr rhs;
              witness = S.match_guard span witness;
            }

    and dlhs (span : span) (lhs : A.lhs) : B.lhs =
      match lhs with
      | LhsFieldAccessor { e; field; typ; witness } ->
          LhsFieldAccessor
            {
              e = dlhs span e;
              field;
              typ = dty span typ;
              witness = S.nontrivial_lhs span witness;
            }
      | LhsArrayAccessor { e; index; typ; witness } ->
          LhsArrayAccessor
            {
              e = dlhs span e;
              index = dexpr index;
              typ = dty span typ;
              witness = S.nontrivial_lhs span witness;
            }
      | LhsLocalVar { var; typ } -> LhsLocalVar { var; typ = dty span typ }
      | LhsArbitraryExpr { e; witness } ->
          LhsArbitraryExpr { e = dexpr e; witness = S.arbitrary_lhs span witness }
    
    module Item = struct
      let dgeneric_param _span ({ ident; span; attrs; kind } : A.generic_param) :
          B.generic_param =
        let kind =
          match kind with
          | GPLifetime { witness } ->
              B.GPLifetime { witness = S.lifetime span witness }
          | GPType -> GPType
          | GPConst { typ } -> GPConst { typ = dty span typ }
        in
        { ident; span; kind; attrs }

      let dgeneric_constraint (span : span)
          (generic_constraint : A.generic_constraint) : B.generic_constraint =
        match generic_constraint with
        | GCLifetime (lf, witness) -> B.GCLifetime (lf, S.lifetime span witness)
        | GCType impl_ident -> B.GCType (dimpl_ident span impl_ident)
        | GCProjection projection ->
            B.GCProjection (dprojection_predicate span projection)

      let dgenerics (span : span) (g : A.generics) : B.generics =
        {
          params = List.map ~f:(dgeneric_param span) g.params;
          constraints = List.map ~f:(dgeneric_constraint span) g.constraints;
        }

      let dparam (span : span) (p : A.param) : B.param =
        {
          pat = dpat p.pat;
          typ = dty (Option.value ~default:span p.typ_span) p.typ;
          typ_span = p.typ_span;
          attrs = p.attrs;
        }

      let dvariant (span : span) (v : A.variant) : B.variant =
        {
          name = v.name;
          arguments = List.map ~f:(map_snd3 @@ dty span) v.arguments;
          is_record = v.is_record;
          attrs = v.attrs;
        }

      let rec dtrait_item' (span : span) (ti : A.trait_item') : B.trait_item' =
        match ti with
        | TIType idents -> TIType (List.map ~f:(dimpl_ident span) idents)
        | TIFn t -> TIFn (dty span t)
        | TIDefault { params; body; witness } ->
            TIDefault
              {
                params = List.map ~f:(dparam span) params;
                body = dexpr body;
                witness = S.trait_item_default span witness;
              }

      and dtrait_item (ti : A.trait_item) : B.trait_item =
        {
          ti_span = ti.ti_span;
          ti_generics = dgenerics ti.ti_span ti.ti_generics;
          ti_v = dtrait_item' ti.ti_span ti.ti_v;
          ti_ident = ti.ti_ident;
          ti_attrs = ti.ti_attrs;
        }

      let rec dimpl_item' (span : span) (ii : A.impl_item') : B.impl_item' =
        match ii with
        | IIType { typ; parent_bounds } ->
            IIType
              {
                typ = dty span typ;
                parent_bounds =
                  List.map ~f:(dimpl_expr span *** dimpl_ident span) parent_bounds;
              }
        | IIFn { body; params } ->
            IIFn { body = dexpr body; params = List.map ~f:(dparam span) params }

      and dimpl_item (ii : A.impl_item) : B.impl_item =
        {
          ii_span = ii.ii_span;
          ii_generics = dgenerics ii.ii_span ii.ii_generics;
          ii_v = dimpl_item' ii.ii_span ii.ii_v;
          ii_ident = ii.ii_ident;
          ii_attrs = ii.ii_attrs;
        }

      let rec ditem (i : A.item) : B.item list =
        try ditem_unwrapped i
        with Diagnostics.SpanFreeError.Exn (Data (context, kind)) ->
          let error = Diagnostics.pretty_print_context_kind context kind in
          let cast_item : A.item -> Ast.Full.item = Stdlib.Obj.magic in
          let ast = cast_item i |> Print_rust.pitem_str in
          let msg = error ^ "\nLast available AST for this item:\n\n" ^ ast in
          [ B.make_hax_error_item i.span i.ident msg ]

      and ditem_unwrapped (item : A.item) : B.item list =
        [
          {
            v = ditem' item.span item.v;
            span = item.span;
            ident = item.ident;
            attrs = item.attrs;
          };
        ]

      and ditem' (span : span) (item : A.item') : B.item' =
        match item with
        | Fn { name; generics; body; params; safety } ->
            B.Fn
              {
                name;
                generics = dgenerics span generics;
                body = dexpr body;
                params = List.map ~f:(dparam span) params;
                safety = dsafety_kind span safety;
              }
        | Type { name; generics; variants; is_struct } ->
            B.Type
              {
                name;
                generics = dgenerics span generics;
                variants = List.map ~f:(dvariant span) variants;
                is_struct;
              }
        | TyAlias { name; generics; ty } ->
            B.TyAlias
              { name; generics = dgenerics span generics; ty = dty span ty }
        | IMacroInvokation { macro; argument; span; witness } ->
            B.IMacroInvokation
              { macro; argument; span; witness = S.macro span witness }
        | Trait { name; generics; items; safety } ->
            B.Trait
              {
                name;
                generics = dgenerics span generics;
                items = List.map ~f:dtrait_item items;
                safety = dsafety_kind span safety;
              }
        | Impl
            _ ->
            B.NotImplementedYet
        | Alias { name; item } -> B.Alias { name; item }
        | Use { path; is_external; rename } -> B.Use { path; is_external; rename }
        | Quote { quote; origin } -> Quote { quote = dquote span quote; origin }
        | HaxError e -> B.HaxError e
        | NotImplementedYet -> B.NotImplementedYet

    end

  include Item

  (** 
    High-level representation of an IIFn.
    Used for mapping Impls to functions                    
  **)
  type impl_description = {
    ident : concrete_ident;
    trait : concrete_ident;
    typ : A.ty;
  }

  (**
    A mapping from a type parameter to it's concrete type,
    with trait context.
  **)
  type type_instantiation = {
    type_param : local_ident;
    concrete_type : A.ty;
    trait_ids : concrete_ident list;
  }
  
  (**
    High-level representation of a monomorphized function
  **)
  type monomorphized_fn = {
    original_ident : concrete_ident;
    new_ident : concrete_ident;
    type_instantiations : type_instantiation list;
  }
  
  (**
    Intermediate representation of a monomorphized function
  **)
  type intermediate_monomorphized_fn = {
    original_ident : concrete_ident;
    type_instantiations : (local_ident * A.ty) list;
  }

  (**
    Mapping from a high-level representation of an Impl-block
    to it's new identifier. Used to rename function calls of 
    IIFns.
  **)
  type ident_assoc = (impl_description * Concrete_ident.t) list


  (**
    Determine, whether a function has trait-bounded generics.
  **)
  let has_trait_bounds (generics : A.generics) =
    List.exists generics.constraints ~f:(function
      | GCType impl_ident -> 
        let trait_name = (RenderId.render impl_ident.goal.trait).name in
        not (String.equal trait_name "Sized")
      | _ -> false
    )

  (**
    Return a list of (param, trait_bound) of generics 
    from either a IIFn or Fn-item
  **)
  let extract_trait_bounds (generics : A.generics) : (local_ident * concrete_ident) list =
    List.concat_map generics.constraints ~f:(function
      | GCType impl_ident ->
        let trait_name = (RenderId.render impl_ident.goal.trait).name in
        if String.is_suffix trait_name ~suffix:"Sized" then
          []
        else
          List.filter_map impl_ident.goal.args ~f:(function
            | GType (TParam type_param) -> 
              Some (type_param, impl_ident.goal.trait)
            | _ -> None)
      | _ -> [])

  (**
    Return the type that implements a trait of trait_id.
  **)
  let find_impl_types (trait_id : Concrete_ident.t) (impl_mapping : ident_assoc) =
    List.filter_map impl_mapping ~f:(fun (desc, _) ->
      if Concrete_ident.equal trait_id desc.trait then
        Some desc.typ
      else None
    ) |> List.dedup_and_sort ~compare:(fun ty1 ty2 ->
          if UA.ty_equality ty1 ty2 then 0 else Poly.compare ty1 ty2)


  (**
    Checks which functions uses trait-defined functions.
    Those that do will get that particular function call renamed.
  **)
  let uses_trait_functions (trait_bounds : (local_ident * concrete_ident) list) =
    let relevant_traits = List.map trait_bounds ~f:snd |> Set.of_list (module Concrete_ident) in
    object
      inherit [_] AVisitors.reduce as super
      method zero = false
      method plus = (||)
        
      method! visit_expr () e =
        match e.e with
        | App { f = { e = GlobalVar (`Concrete _); _ }; trait = Some (impl_expr, _); _ } ->
          Set.mem relevant_traits impl_expr.goal.trait
        | App { bounds_impls = bounds; _ } when not (List.is_empty bounds) ->
          List.exists bounds ~f:(fun impl_expr ->
            Set.mem relevant_traits impl_expr.goal.trait)
        | _ -> super#visit_expr' () e.e
    end

  (**
    Taking a list of type_instantiation, substitute IITypes with their concrete type.
    Removes associated types from the final AST.
  **)
  let substitute_multiple_type_params (type_instantiations : type_instantiation list) (items : A.item list) =
    object (self)
      inherit [_] AVisitors.map as super  
      
      method! visit_ty () ty =
        match ty with
        | TParam param ->
          (match List.find type_instantiations ~f:(fun inst -> 
             String.equal (RenderId.local_ident param) (RenderId.local_ident inst.type_param)) with
          | Some inst -> inst.concrete_type
          | None -> ty)
        | TAssociatedType { impl; item } ->
          (match impl.goal.args with
          | GType (TParam type_param) :: _ ->
            (match List.find type_instantiations ~f:(fun inst -> 
               String.equal (RenderId.local_ident type_param) (RenderId.local_ident inst.type_param)) with
            | Some inst ->
              (match self#extract_associated_type_in_impl_blocks inst.concrete_type impl.goal.trait item with
              | Some resolved_ty -> resolved_ty
              | None -> ty)
            | None -> ty)
          | _ -> ty)
        | _ -> super#visit_ty () ty
      
      (**
        Extract the concrete type of an IIType in an impl block.
        Helper-method for substitution of associated types in AST.
      **)
      method private extract_associated_type_in_impl_blocks (concrete_type : A.ty) (trait_id : concrete_ident) (assoc_item : concrete_ident) : A.ty option =
        let trait_name = (RenderId.render trait_id).name in
        let assoc_item_name = (RenderId.render assoc_item).name in
        List.find_map items ~f:(fun item ->
          match item.v with
          | A.Impl { of_trait = (impl_trait_id, _); self_ty; items = impl_items; _ } ->
            let impl_trait_name = (RenderId.render impl_trait_id).name in
            if String.equal impl_trait_name trait_name && UA.ty_equality self_ty concrete_type then
              List.find_map impl_items ~f:(fun impl_item ->
                let impl_item_name = (RenderId.render impl_item.ii_ident).name in
                match impl_item.ii_v with
                | IIType { typ; _ } when String.equal impl_item_name assoc_item_name ->
                  Some typ
                | _ -> None
              )
            else None
          | _ -> None
        )
    end
  
  (**
    Helper-function for finding the implementing type from Impl-calls.
    Used to correctly rename function calls.
    For now, naively finds the first GType (seems to work fine).
  **)
  let determine_implementing_type_from_trait (trait_info : (A.impl_expr * A.generic_value list) option) : A.ty option =
    match trait_info with
    | Some (_, generic_values) ->
      List.find_map generic_values ~f:(function
        | GType ty -> Some ty
        | _ -> None)
    | None -> None

  (**
    Helper-function for naming convention
  **)
  let map_typ_to_str (typ: A.ty) =
    match typ with
    | TBool -> "bool"
    | TChar -> "char"
    | TInt { size; signedness; } ->
      let arch_size = phys_equal size SSize in
      let size_doc = match size with
        | S8 -> "8"
        | S16 -> "16"
        | S32 -> "32"
        | S64 -> "64"
        | S128 -> "128"
        | SSize -> 
          match signedness with
          | Unsigned ->
            "usize"
          | Signed ->
            "isize"
      in
      let sign_doc = match signedness with
        | Unsigned -> 
          if arch_size then
            ""
          else "u"
        | Signed -> 
          if arch_size then
            ""
          else "i"
      in sign_doc ^ size_doc
    | TFloat _ -> "float"
    | TStr -> "string"
    | TApp { ident = `Concrete x; _ } | TApp { ident = `Projector (`Concrete x ); _ }  -> (RenderId.render x).name
    | TApp _ -> "placeholder"
    | TArray _ -> "array"
    | TSlice _ -> "slice"
    | TRawPointer _ -> "raw_pointer"
    | TRef _ -> "ref"
    | TParam _ -> "param"
    | TArrow _ -> "arrow"
    | TAssociatedType _ -> "assc_type"
    | TOpaque _ -> "opaque"
    | TDyn _ -> "dyn"
  

  (**
    Traversing the AST, generate ident_assoc.
    Maps an impl_description to their new name,
    used to generate monomorphized functions.
  **)
  let construct_type_aware_mapping item =
    (object
        inherit [_] AVisitors.reduce as super

        method zero = []
        method plus l1 l2 = l1 @ l2
        
        method! visit_item' () item' =
          match item' with
          | A.Impl {items = impl_items; of_trait = (trait_id, _); self_ty; _ } ->
            let extract_impl_desc (impl_item : A.impl_item) =
              match impl_item.ii_v with
              | A.IIFn _ ->
                let desc = {
                  ident = impl_item.ii_ident;
                  trait = trait_id;
                  typ = self_ty;
                }
                in
                let trait_to_str = (RenderId.render (trait_id)).name in
                let new_name = Concrete_ident.map_path_strings ~f:(fun x -> trait_to_str ^ "_" ^ x ^ "_" ^ (map_typ_to_str self_ty)) desc.ident in
                [(desc, new_name)]
              | _ -> []
            in            
            List.concat (List.map ~f:(fun impl_item -> extract_impl_desc impl_item) impl_items)
          | _ -> super#visit_item' () item'
    end)
      #visit_item () item
  

  (**
    Generates monomorphized_fns off of trait-bounded functions, if necessary.
    For trait-bounded functions, group type parameters by trait-bounds.
    Then, find intersection of these groups, determines which types to monomorphize for.
    For multiple trait-bounded type parameters, find the cartesian product of implementing types
    to determine all combinations of monomorphized functions.
  **)
  let generate_monomorphized_functions (impl_mapping : ident_assoc) (items : A.item list) =
    List.concat_map items ~f:(fun item ->
      match item.v with
      | A.Fn { name; generics; body; _ } when has_trait_bounds generics ->
        let trait_bounds = extract_trait_bounds generics in
        let bounds_by_type_param = 
          List.fold trait_bounds ~init:(Map.empty (module Local_ident)) ~f:(fun acc (type_param, trait_id) ->
            Map.add_multi acc ~key:type_param ~data:trait_id
          )
        in
        let type_param_options = 
          Map.to_alist bounds_by_type_param
          |> List.map ~f:(fun (type_param, trait_ids) ->
              let implementing_types_per_trait = 
                List.map trait_ids ~f:(fun trait_id -> find_impl_types trait_id impl_mapping)
              in
              let intersection_types = 
                match implementing_types_per_trait with
                | [] -> []
                | first_set :: rest_sets ->
                    List.fold rest_sets ~init:first_set ~f:(fun acc types ->
                      List.filter acc ~f:(fun ty ->
                        List.exists types ~f:(UA.ty_equality ty)
                      )
                    )
              in
              (type_param, trait_ids, intersection_types)
            )
        in
        let rec cartesian_product = function
          | [] -> [[]]
          | (type_param, trait_ids, types) :: rest ->
              let rest_products = cartesian_product rest in
              List.concat_map types ~f:(fun concrete_type ->
                List.map rest_products ~f:(fun product ->
                  { type_param; concrete_type; trait_ids } :: product
                )
              )
        in
        let all_instantiations = cartesian_product type_param_options in
        let needs_monomorphization = 
          (uses_trait_functions trait_bounds)#visit_expr () body 
        in
        if needs_monomorphization then
          List.map all_instantiations ~f:(fun type_instantiations ->
            let type_suffix = 
              List.map type_instantiations ~f:(fun inst -> map_typ_to_str inst.concrete_type)
              |> String.concat ~sep:"_"
            in
            let new_name = Concrete_ident.map_path_strings name 
              ~f:(fun x -> x ^ "_" ^ type_suffix) 
            in
            {
              original_ident = name;
              new_ident = new_name;
              type_instantiations;
            })
        else []
      | _ -> [])

  (**
    Check if impl_description matches with a mapping of impls.
    If true, a function call is eligible for renaming.
  **)
  let check_eq_key (desc : impl_description) (id : impl_description) =
    let desc_name = (RenderId.render desc.ident).name in
    let id_name = (RenderId.render id.ident).name in
    String.equal desc_name id_name &&
    Concrete_ident.equal desc.trait id.trait &&
    UA.ty_equality desc.typ id.typ


  (**
    Check if intermediate_monomorphized_fn matches with a monomorphized_fn.
    If true, a function call is eligible for renaming. 
  **)
  let check_eq_monomorphized_key (call_desc : intermediate_monomorphized_fn) (mono_fn : monomorphized_fn) =
    let call_name = (RenderId.render call_desc.original_ident).name in
    let mono_name = (RenderId.render mono_fn.original_ident).name in
    String.equal call_name mono_name &&
    Concrete_ident.equal call_desc.original_ident mono_fn.original_ident &&
    List.length call_desc.type_instantiations = List.length mono_fn.type_instantiations &&
    List.for_all2_exn call_desc.type_instantiations mono_fn.type_instantiations 
      ~f:(fun (call_param, call_type) inst ->
        String.equal call_param.name inst.type_param.name &&
        UA.ty_equality call_type inst.concrete_type
      )
  

  (**
    If a mapping corresponds to the function behind a function call,
    rename the function call to the corresponding monomorphized function.
  **)
  let rename_trait_impl_calls (map : ident_assoc) =
    (object
      inherit [_] AVisitors.map as super
  
      method! visit_expr _ e = 
        match e.e with 
        | App { f = f'; args; generic_args; bounds_impls; trait } ->
          (match f' with
          | { e = GlobalVar (`Concrete x); _ } ->
            (match trait with
            | Some trait_info -> 
              let call_trait_ident = (fst trait_info).goal.trait in
              (match determine_implementing_type_from_trait (Some trait_info) with
              | Some implementing_type ->
                let call_desc = { 
                  ident = x;
                  trait = call_trait_ident;
                  typ = implementing_type
                } in
                (match List.find ~f:(fun (desc, _) -> check_eq_key call_desc desc) map with
                | Some (_, new_ident) -> 
                  { e with e = App { 
                    f = { f' with e = GlobalVar (`Concrete new_ident) }; 
                    args; generic_args; 
                    bounds_impls;
                    trait = None
                  }}
                | None -> super#visit_expr e.typ e)
              | None -> 
                super#visit_expr e.typ e)
            | None -> super#visit_expr e.typ e)
          | { e = GlobalVar (`Projector (`Concrete x)); _ } ->
            (match trait with
            | Some trait_info -> 
              let call_trait_ident = (fst trait_info).goal.trait in
              (match determine_implementing_type_from_trait (Some trait_info) with
              | Some implementing_type ->
                let call_desc = { 
                  ident = x;
                  trait = call_trait_ident;
                  typ = implementing_type
                } in
                (match List.find ~f:(fun (desc, _) -> check_eq_key call_desc desc) map with
                | Some (_, new_ident) -> 
                  { e with e = App { 
                    f = { f' with e = GlobalVar (`Projector (`Concrete new_ident)) }; 
                    args; generic_args; 
                    bounds_impls;
                    trait = None
                  }}
                | None -> super#visit_expr e.typ e)
              | None -> super#visit_expr e.typ e)
            | None -> super#visit_expr e.typ e)
          | _ -> super#visit_expr e.typ e)
        | _ -> super#visit_expr e.typ e
    end)#visit_item TBool


  (**
    Determines the concrete types of trait-bounded parameters, by analyzing the function call.
    Used as a helper in renaming function calls to monomorphized functions from trait-bounded
    functions.
  **)
  let find_trait_bounded_types (fn_id : concrete_ident) (args : A.expr list) (return_typ : A.ty) (generic_args : A.generic_value list) (items : A.item list) : (local_ident * A.ty) list =
    List.find_map items ~f:(fun item ->
      match item.v with
      | A.Fn { name; generics; params; body; _ } when Concrete_ident.equal name fn_id ->
        let trait_bounds = extract_trait_bounds generics in
        let bounded_type_params = List.map trait_bounds ~f:fst |> List.dedup_and_sort ~compare:[%compare: local_ident] in
        let param_instantiations = 
          match List.zip params args with
          | Ok param_arg_pairs ->
            List.filter_map param_arg_pairs ~f:(fun (param, arg) ->
              match param.typ with
              | TParam type_param ->
                if List.exists bounded_type_params ~f:(fun tp -> 
                    String.equal tp.name type_param.name) then
                  Some (type_param, arg.typ)
                else None
              | _ -> None)
            |> List.dedup_and_sort ~compare:(fun (p1, _) (p2, _) -> 
                 String.compare (RenderId.local_ident p1) (RenderId.local_ident p2))
          | Unequal_lengths -> []
        in
        let generic_instantiations = 
          match List.zip generics.params generic_args with
          | Ok param_arg_pairs ->
            List.filter_map param_arg_pairs ~f:(fun (param, generic_value) ->
              match (param.kind, generic_value) with
              | (GPType, GType concrete_type) ->
                if List.exists bounded_type_params ~f:(fun tp -> 
                    String.equal tp.name param.ident.name) then
                  Some (param.ident, concrete_type)
                else None
              | _ -> None)
            |> List.dedup_and_sort ~compare:(fun (p1, _) (p2, _) -> 
                 String.compare (RenderId.local_ident p1) (RenderId.local_ident p2))
          | Unequal_lengths -> []
        in
        let return_instantiation =
          match body.typ with
          | TParam type_param ->
            if List.exists bounded_type_params ~f:(fun tp -> 
                String.equal tp.name type_param.name) then
              [(type_param, return_typ)]
            else []
          | _ -> []
        in
        let all_instantiations = 
          let combined = param_instantiations @ generic_instantiations @ return_instantiation in
          List.dedup_and_sort combined ~compare:(fun (p1, _) (p2, _) -> 
            String.compare (RenderId.local_ident p1) (RenderId.local_ident p2))
        in
        if List.is_empty all_instantiations then None else Some all_instantiations
      | _ -> None)
    |> Option.value ~default:[]


  (**
    Renames trait-bounded function calls to monomorphized function.
  **)
  let rename_trait_bounded_calls (impl_map : ident_assoc) (fn_map : monomorphized_fn list) (all_items : A.item list) =
    object
      inherit [_] AVisitors.map as super
          
      method! visit_expr _ e =
        match e.e with
        | App { f = { e = GlobalVar (`Concrete f_id); _ } as f'; args; generic_args; bounds_impls; trait } ->
          let relevant_bounds = List.filter bounds_impls ~f:(fun impl_expr ->
            let trait_name = (RenderId.render impl_expr.goal.trait).name in
            not (String.equal trait_name "Sized")) in
          let call_context = 
            if not (List.is_empty relevant_bounds) || Option.is_some trait then
              find_trait_bounded_types f_id args e.typ generic_args all_items
            else []
          in 
          (match call_context with
          | [] ->
            (match trait with
            | Some trait_info ->
              let call_trait_ident = (fst trait_info).goal.trait in
              (match determine_implementing_type_from_trait (Some trait_info) with
              | Some implementing_type ->
                let impl_call_desc = {
                  ident = f_id;
                  trait = call_trait_ident;
                  typ = implementing_type
                } in
                (match List.find impl_map ~f:(fun (desc, _) -> check_eq_key impl_call_desc desc) with
                | Some (_, new_ident) -> 
                  { e with e = App { 
                    f = { f' with e = GlobalVar (`Concrete new_ident) }; 
                    args; generic_args; 
                    bounds_impls;
                    trait = None
                  }}
                | None -> super#visit_expr e.typ e)
              | None -> super#visit_expr e.typ e)
            | None -> super#visit_expr e.typ e)
          | type_instantiations ->
            let call_desc = {
              original_ident = f_id;
              type_instantiations;
            } in
            (match List.find fn_map ~f:(check_eq_monomorphized_key call_desc) with
            | Some mono_fn ->
              { e with e = App { 
                  f = { f' with e = GlobalVar (`Concrete mono_fn.new_ident) }; 
                  args; 
                  generic_args; 
                  bounds_impls = []; 
                  trait = None
                }
              }
            | None ->
              (match trait with
              | Some trait_info ->
                let call_trait_ident = (fst trait_info).goal.trait in
                (match determine_implementing_type_from_trait (Some trait_info) with
                | Some implementing_type ->
                  let impl_call_desc = {
                    ident = f_id;
                    trait = call_trait_ident;
                    typ = implementing_type
                  } in
                  (match List.find impl_map ~f:(fun (desc, _) -> check_eq_key impl_call_desc desc) with
                  | Some (_, new_ident) -> 
                    { e with e = App { 
                      f = { f' with e = GlobalVar (`Concrete new_ident) }; 
                      args; 
                      generic_args; 
                      bounds_impls;
                      trait = None
                    }}
                  | None -> super#visit_expr e.typ e)
                | None -> super#visit_expr e.typ e)
              | None -> super#visit_expr e.typ e)))
        | App { f = { e = GlobalVar (`Projector (`Concrete f_id)); _ } as f'; args; generic_args; bounds_impls; trait } ->
          (match trait with
          | Some trait_info ->
            let call_trait_ident = (fst trait_info).goal.trait in
            (match determine_implementing_type_from_trait (Some trait_info) with
            | Some implementing_type ->
              let impl_call_desc = {
                ident = f_id;
                trait = call_trait_ident;
                typ = implementing_type
              } in
              (match List.find impl_map ~f:(fun (desc, _) -> check_eq_key impl_call_desc desc) with
              | Some (_, new_ident) -> 
                { e with e = App { 
                  f = { f' with e = GlobalVar (`Projector (`Concrete new_ident)) }; 
                  args; 
                  generic_args; 
                  bounds_impls;
                  trait = None
                }}
              | None -> super#visit_expr e.typ e)
            | None -> super#visit_expr e.typ e)
          | None -> super#visit_expr e.typ e)
        | _ -> super#visit_expr e.typ e
    end

  let change_impl_calls (items : A.item list) (mapping : ident_assoc) =
    List.map ~f:(rename_trait_impl_calls mapping) items


  (**
    Create AST-item of monomorphized function.
  **)
  let create_monomorphized_item (items : A.item list) (mono_fn : monomorphized_fn) : A.item option =
    List.find_map items ~f:(fun item ->
      match item.v with
      | A.Fn { name; generics; body; params; safety } 
        when Concrete_ident.equal name mono_fn.original_ident ->
        let substitution_visitor = substitute_multiple_type_params mono_fn.type_instantiations items in
        let substituted_body = substitution_visitor#visit_expr () body in
        let substituted_params = List.map params ~f:(fun param ->
          { param with typ = substitution_visitor#visit_ty () param.typ }) in
        let substituted_return_type = substitution_visitor#visit_ty () body.typ in
        let final_body = { substituted_body with typ = substituted_return_type } in
        let instantiated_param_names = 
          List.map mono_fn.type_instantiations ~f:(fun inst -> inst.type_param.name)
          |> Set.of_list (module String)
        in
        let remaining_params = List.filter generics.params ~f:(fun param ->
          match param.kind with
          | GPType -> not (Set.mem instantiated_param_names param.ident.name)
          | _ -> true
        ) in
        let remaining_constraints = List.filter generics.constraints ~f:(fun constraint_ ->
          match constraint_ with
          | GCType impl_ident ->
            List.for_all impl_ident.goal.args ~f:(function
              | GType (TParam param) -> not (Set.mem instantiated_param_names param.name)
              | _ -> true)
          | _ -> true
        ) in
        Some { item with 
          v = A.Fn { 
            name = mono_fn.new_ident; 
            generics = { params = remaining_params; constraints = remaining_constraints };
            body = final_body; 
            params = substituted_params; 
            safety 
          };
          ident = mono_fn.new_ident
        }
      | _ -> None)
  
  (**
    Replace Impl-blocks with their monomorphized counter-part.
  **)
  let extract_impl_items_as_functions (items : A.item list) : A.item list =
    List.concat_map items ~f:(fun item ->
      match item.v with
      | A.Impl { items = impl_items; safety; of_trait; self_ty; _ } ->
        List.filter_map impl_items ~f:(fun impl_item ->
          match impl_item.ii_v with
          | IIFn { body; params } ->
            let trait_name = (RenderId.render (fst of_trait)).name in
            let type_suffix = map_typ_to_str self_ty in
            let new_name = Concrete_ident.map_path_strings impl_item.ii_ident 
              ~f:(fun x -> trait_name ^ "_" ^ x ^ "_" ^ type_suffix) in
            Some { item with 
              v = A.Fn {
                name = new_name;
                generics = impl_item.ii_generics;
                body = body;
                params = params;
                safety = safety;
              };
              ident = new_name
            }
          | _ -> None)
      | _ -> [])


  (**
    TODO: Awkward order of operations?
    Entry-point of monomorphization phase:
    - Generate mapping of Impl-blocks
    - Generate monomorphized_fn instances
    - Rename all calls
    - For each IIFn, generate monomorphized AST-item
    - Ignore Traits and trait-bounded functions; no longer appropriate in AST.
  **)
  let ditems (items : A.item list) : B.item list =
    let impl_mapping = List.concat (List.map items ~f:construct_type_aware_mapping) in
    let extracted_impl_functions = extract_impl_items_as_functions items in
    let non_impl_items = List.filter items ~f:(fun item ->
      match item.v with A.Impl _ | A.Trait _ -> false | _ -> true) in
    let original_fn_monomorphizations = generate_monomorphized_functions impl_mapping non_impl_items in
    let original_monomorphized_items = List.filter_map original_fn_monomorphizations ~f:(create_monomorphized_item items) in
    let impl_fn_monomorphizations = generate_monomorphized_functions impl_mapping extracted_impl_functions in
    let impl_monomorphized_items = List.filter_map impl_fn_monomorphizations ~f:(create_monomorphized_item items) in
    let all_functions = non_impl_items @ extracted_impl_functions @ original_monomorphized_items @ impl_monomorphized_items in
    let all_monomorphizations = original_fn_monomorphizations @ impl_fn_monomorphizations in
    let items_with_impl_renames = change_impl_calls all_functions impl_mapping in      
    let items_with_all_renames = 
      (rename_trait_bounded_calls impl_mapping all_monomorphizations items_with_impl_renames)#visit_modul TBool items_with_impl_renames
    in      
    let processed_items = List.filter_map items_with_all_renames ~f:(fun item ->
      match item.v with
      | A.Fn { name; generics; _ } when has_trait_bounds generics &&
        List.exists all_monomorphizations ~f:(fun mono_fn -> 
          Concrete_ident.equal mono_fn.original_ident name) ->
        None
      | _ -> Some (ditem item)
    ) in 
    List.concat processed_items

    let dexpr (_e : A.expr) : B.expr =
      Stdlib.failwith "Should not be called directly"
  end

  include Implem
  module FA = FA
end
[@@add "subtype.ml"]

(* phase_and_mut_defsite.ml *)