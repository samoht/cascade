(** Values shared by the CSS integration test suites. *)

val minify : Cascade.Css.t -> string
(** [minify s] is [s] optimized and printed minified. *)

val btn : Cascade.Css.Selector.t
(** [btn] is the [.btn] class selector. *)

val card : Cascade.Css.Selector.t
(** [card] is the [.card] class selector. *)
