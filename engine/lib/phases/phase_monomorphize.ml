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

  module Implem : ImplemT.T = struct
    let metadata = metadata

    module S = struct
      include Features.SUBTYPE.Id
    end
    
    module UA = Ast_utils.Make (FA)
    module UB = Ast_utils.Make (FB)

    let rename_trait_impl_calls (f : A.ty -> concrete_ident -> concrete_ident) =
      (object
         inherit [_] AVisitors.map as super
         method! visit_concrete_ident (typ : A.ty) ident = f typ ident

         method! visit_global_ident typ (x : Global_ident.t) =
           match x with
           | `Concrete x -> `Concrete (f typ x)
           | `Projector (`Concrete x) -> `Projector (`Concrete (f typ x))
           | _ -> super#visit_global_ident typ x

         method! visit_expr _ e = match e.e with _ -> super#visit_expr e.typ e
      end)
        #visit_item TBool (* Dummy value *)

    let change_impl_calls (items : A.item list) =
      List.map ~f:
        (rename_trait_impl_calls (fun typ ident ->
          Concrete_ident.map_path_strings ~f:(fun x -> x ^ "_typ") ident)
        ) items

    [%%inline_defs dmutability + dsafety_kind + dty]

    [%%inline_defs "Item.*" - ditems - ditem']

    let ditem' (span : span) (item : A.item') : B.item' =
      match item with
      | [%inline_arms "ditem'.*"] -> auto
      | Impl _ ->
        Stdlib.failwith "Shouldn't happen"
    [@@inline_ands bindings_of ditem - ditem']
        
    let ditems (items : A.item list) : B.item list =
      let items = change_impl_calls items in
      let outer_item_list = List.bind items ~f:(fun x ->
        match x.v with
        | Impl { generics; self_ty; of_trait; items = impl_items; safety; witness; _ } ->
          let inner_item_list = List.concat_map impl_items ~f:(fun impl_item ->
            match impl_item.ii_v with
            | IIFn { body; params } ->
              let result_list = 
                  [ditem { x with v = 
                      A.Fn {
                      name = impl_item.ii_ident;
                      generics = impl_item.ii_generics;
                      body = body;
                      params = params;
                      safety = safety;
                  } 
                }]
              in
              result_list
            | _ -> []
          ) in
            inner_item_list
        | Trait _ -> []
        | _ -> []
      )
      in
      outer_item_list

    let dexpr (_e : A.expr) : B.expr =
      Stdlib.failwith "Should not be called directly"

  end

  include Implem
  module FA = FA
end
[@@add "subtype.ml"]

(* phase_and_mut_defsite.ml *)