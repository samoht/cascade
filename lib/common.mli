(** Shared low-level helpers. *)

module List : sig
  include module type of Stdlib.List

  type 'a edit = Keep | Replace of 'a | Drop

  val same : 'a list -> 'a list -> bool
  (** [same xs ys] holds when both lists have the same length and corresponding
      elements are physically equal. *)

  val preserve : 'a list -> 'a list -> 'a list
  (** [preserve before after] returns [before] when [same before after]. *)

  val map_preserve : ('a -> 'a) -> 'a list -> 'a list
  (** [map_preserve f xs] is [Stdlib.List.map f xs], but returns [xs] when every
      element is physically unchanged. *)

  val filter_preserve : ('a -> bool) -> 'a list -> 'a list
  (** [filter_preserve f xs] is [Stdlib.List.filter f xs], but returns [xs] when
      no element is removed. *)

  val filter_map_preserve : ('a -> 'a option) -> 'a list -> 'a list
  (** [filter_map_preserve f xs] is a same-type [Stdlib.List.filter_map f xs],
      but returns [xs] when every element is kept physically unchanged. *)

  val edit_preserve : ('a -> 'a edit) -> 'a list -> 'a list
  (** [edit_preserve f xs] maps and filters [xs], returning [xs] when [f]
      returns {!constructor-Keep} for every element. {!constructor-Keep} and
      {!constructor-Drop} avoid per-element option allocation in
      mostly-unchanged filtering walks. *)
end

module String : sig
  include module type of Stdlib.String

  val lowercase_ascii_preserve : string -> string
  (** [lowercase_ascii_preserve s] returns [s] when it already contains only
      lowercase ASCII letters, digits, [-] and [_]; otherwise it is
      [Stdlib.String.lowercase_ascii s]. CSS identifiers are overwhelmingly in
      that set, so this avoids the [Bytes.map] allocation for them. *)
end
