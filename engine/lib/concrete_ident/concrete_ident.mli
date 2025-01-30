(** This module provides the global concrete identifiers. *)

module Fresh_module : sig
  type t [@@deriving show, yojson, hash, compare, sexp, hash, eq]
  (** A type representing a fresh module. Below, we define two functions:
       - [fresh] creates a new fresh module
       - [move_to_fresh_module] creates a new and always fresh identifier by
         "moving" an existing identifier under the given fresh module
  *)
end

module View : module type of Concrete_ident_view

module T : sig
  type t [@@deriving show, yojson, compare, sexp, eq, hash]
  (** A concrete identifier. *)
end

include module type of T with type t = T.t

type reserved_suffix = [ `Cast | `Pre | `Post ]
[@@deriving show, yojson, hash, compare, sexp, hash, eq]
(** A concrete identifier can have a reserved suffix: this is useful to derive
new identifiers from existing identifiers. *)

val of_def_id :
  ?suffix:reserved_suffix option -> value:bool -> Types.def_id -> t
(** [of_def_id ?suffix ~value def_id] a concrete identifier out of a Rust
identifier [def_id]. [value] is a flag that decides whether [def_id]
refers to a value or not.

[value] is important only for constructors: i.e. the identifier for the type
of a struct should be created with [value] set to false while the identifier
for the constructor of a struct should be create with [value] set to true.
For more information, please read the documentation of module
{!Explicit_def_id}.
*)

type name = Concrete_ident_generated.t
[@@deriving show, yojson, compare, sexp, eq, hash]
(** A enumeration of static concrete identifiers useful inside the engine. *)

val of_name : value:bool -> name -> t
(** Creates an identifier given a name. [value] has the same meaning as in function {!of_def_id}. *)

val eq_name : name -> t -> bool
(** [eq_name name identifier] is true whenever [identifier] is [name].  *)

val to_debug_string : t -> string
(** Format an identifier as a (ppx) debug string. The default debug pretty prints the identifier. *)

val fresh_module : label:string -> t list -> Fresh_module.t
(** [fresh_module ~label hints] creates a fresh module given a non-empty list of
      existing identifiers and a label. The generated module name will be
      unique, will be close to the identifiers found in [hints], and will
      include the label.
*)

val move_to_fresh_module : Fresh_module.t -> t -> t
(** Creates a fresh identifier under a given fresh module and given an existing identifier. *)

val with_suffix : reserved_suffix -> t -> t
(** Creates an identifier out of an existing one, adding a suffix. *)

val to_view : t -> Concrete_ident_view.t
(** Compute a view for a given identifier. *)

val map_path_strings : f:(string -> string) -> t -> t
[@@alert unsafe "This function should be only used in Import_thir!"]
(** This function maps any string found in the inner representation of hax. This
  is a hack for Import_thir so that we can generically produce identifiers for
  any integer type, please do not use it elsewhere. *)

val is_constructor : t -> bool
(** Returns true if the ident represents a constructor. *)

type comparator_witness

val comparator : (t, comparator_witness) Base.Comparator.comparator

module RenderSig : module type of Concrete_ident_render_sig.Make (T)

module type RENDER_API = RenderSig.RENDER_API
module type NAME_POLICY = Concrete_ident_render_sig.NAME_POLICY

module DefaultNamePolicy : NAME_POLICY
module MakeRenderAPI (NP : NAME_POLICY) : RenderSig.RENDER_API
module DefaultViewAPI : RenderSig.RENDER_API

module ImplInfoStore : sig
  val init : (Types.def_id * Types.impl_infos) list -> unit

  val lookup_raw : t -> Types.impl_infos option
  (** Lookup the (raw[1]) implementation information given a concrete
  ident. Returns `Some _` if and only if the supplied identifier points
  to an `Impl`.
  
  [1]: those are raw THIR types.
  
  {b WARNING}: due to {{: https://github.com/hacspec/hax/issues/363}
  issue 363}, when looking up certain identifiers generated by the
  engine, this function may return [None] even though the supplied
  identifier points to an [Impl] block. *)
end

val matches_namespace : Types.namespace -> t -> bool
