(** Font-face descriptor types for type-safe [\@font-face] construction. *)

(** {1 Metric Override Types} *)

(** Metric override value - either "normal" or a percentage. Used for
    ascent-override, descent-override, line-gap-override. *)
type metric_override = Normal | Percent of float

val metric_override_to_string : metric_override -> string
(** [metric_override_to_string m] converts a metric override to its CSS string
    representation. *)

(** {1 Size Adjust} *)

type size_adjust = float
(** Size adjustment percentage. *)

val size_adjust_to_string : size_adjust -> string
(** [size_adjust_to_string s] converts size adjust to string. *)

(** {1 Font Source} *)

(** A single font source entry. *)
type src_entry =
  | Url of { url : string; format : string option; tech : string option }
  | Local of string
  | Raw of string  (** Escape hatch for unparsed sources *)

type src = src_entry list
(** Font source list. *)

val src_entry_to_string : src_entry -> string
(** [src_entry_to_string e] converts source entry to string. *)

val src_to_string : src -> string
(** [src_to_string entries] converts a font source list to its CSS string
    representation. *)

(** {1 Parsing} *)

val metric_override_of_string : string -> metric_override
(** [metric_override_of_string s] parses a metric override value. *)

val size_adjust_of_string : string -> size_adjust
(** [size_adjust_of_string s] parses a size-adjust percentage. *)

val src_of_string : string -> src
(** [src_of_string s] parses a src value. Falls back to [Raw] for unparsed. *)
