(** Font-face descriptor types for type-safe [\@font-face] construction. *)

(** {1 Metric Override Types} *)

(** Metric override value - either "normal" or a percentage. Used for
    ascent-override, descent-override, line-gap-override. *)
type metric_override = Normal | Percent of float

val string_of_metric_override : metric_override -> string
(** [string_of_metric_override m] converts a metric override to its CSS string
    representation. *)

(** {1 Size Adjust} *)

type size_adjust = float
(** Size adjustment percentage. *)

val string_of_size_adjust : size_adjust -> string
(** [string_of_size_adjust s] converts size adjust to string. *)

(** {1 Font Source} *)

(** A single font source entry. *)
type src_entry =
  | Url of { url : string; format : string option; tech : string option }
  | Quoted_url of {
      url : string;
      quote : char;
      format : string option;
      tech : string option;
    }
  | Local of string
  | Var of src Values.var

and src = src_entry list
(** Font source list. *)

type t = src
(** Font source list. *)

val equal_metric_override : metric_override -> metric_override -> bool
(** [equal_metric_override a b] tests metric overrides for equality. *)

val equal_size_adjust : size_adjust -> size_adjust -> bool
(** [equal_size_adjust a b] tests size adjustments for equality. *)

val compare_size_adjust : size_adjust -> size_adjust -> int
(** [compare_size_adjust a b] orders size adjustments. *)

val equal_src : src -> src -> bool
(** [equal_src a b] tests font source lists for structural equality. *)

val pp : t Pp.t
(** [pp] renders a font source list. *)

val string_of_src_entry : src_entry -> string
(** [string_of_src_entry e] converts source entry to string. *)

val string_of_src : ?minify:bool -> src -> string
(** [string_of_src entries] converts a font source list to its CSS string
    representation. *)

val to_string : ?minify:bool -> t -> string
(** [to_string src] converts a font source list to CSS source text. *)

(** {1 Parsing} *)

val metric_override_of_string : string -> metric_override
(** [metric_override_of_string s] parses a metric override value. *)

val size_adjust_of_string : string -> size_adjust
(** [size_adjust_of_string s] parses a size-adjust percentage. *)

val src_of_string : string -> src
(** [src_of_string s] parses a src value. Raises {!Cursor.Parse_error} for
    unparsed sources. *)

val read_src : Cursor.t -> src
(** [read_src t] parses a font source list from a component cursor. *)
