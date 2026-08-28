(** An {!Html} element as a {!Cascade.Resolve.NODE}, so the library can match
    selectors and resolve the cascade against a parsed page. *)

include Cascade.Resolve.NODE with type t = Html.element
