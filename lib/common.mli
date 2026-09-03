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

  (** One element of UTF-8 text: a Unicode scalar value, or the [n] bytes of a
      maximal subpart of an ill-formed sequence, the longest prefix that could
      still open a well-formed one. An ill-formed run holds as many maximal
      subparts as CSS Syntax 3 (ED) sec. 3.3 puts U+FFFD replacement characters
      in its place. *)
  type utf8 = Scalar of Uchar.t | Malformed of int

  val utf8_decode : ?pos:int -> ?len:int -> string -> utf8 option
  (** [utf8_decode s] is the element at the start of [s], or of its [len] bytes
      from [pos], and [None] when that window holds no byte. A sequence reaching
      past the end of the window is truncated, so the bytes of it that are
      inside are {!constructor-Malformed}. *)

  val utf8_fold :
    ?pos:int -> ?len:int -> ('a -> int -> utf8 -> 'a) -> 'a -> string -> 'a
  (** [utf8_fold f acc s] applies {!utf8_decode} along [s], or along its [len]
      bytes from [pos], passing [f] the byte offset of each element. *)

  val utf8_length : ?pos:int -> ?len:int -> string -> int
  (** [utf8_length s] counts the elements of [s], or of its [len] bytes from
      [pos]: one per Unicode scalar value and one per maximal subpart of an
      ill-formed sequence. *)

  val utf8_lead_before : string -> int -> int
  (** [utf8_lead_before s i] moves [i] back to the lead byte of the UTF-8
      sequence it falls inside, by at most the three continuation bytes a
      sequence holds, so bytes that are not UTF-8 leave [i] where it was.
      Slicing [s] at the result never splits a code point. *)

  val utf8_lead_after : string -> int -> int
  (** [utf8_lead_after s i] moves [i] forward out of the UTF-8 sequence it falls
      inside, on the same terms as {!utf8_lead_before}. *)
end

val mix_int : int -> int -> int
(** [mix_int acc x] folds the integer [x] into the running hash [acc]. *)

val hash_string : string -> int
(** [hash_string s] hashes the bytes of [s] with {!mix_int}. Callers combine the
    result with other hashes through {!mix_int}, so both sides of a bucket key
    must come from the same pair of functions. *)

(** Hash tables whose bindings are lists used as buckets. *)
module Table : sig
  module Make (H : Hashtbl.HashedType) : sig
    include Hashtbl.S with type key = H.t

    val push : 'a list t -> key -> 'a -> unit
    (** [push tbl key value] prepends [value] to the list bound to [key], or
        binds the singleton [[value]] when [key] is absent. *)
  end
end
