(** String difference analysis and formatting. *)

val default_max_width : int
(** [default_max_width] is the default maximum width for formatted output (60
    characters). *)

type config = {
  max_width : int;  (** Maximum display width. *)
  short_threshold : int;  (** Threshold below which strings are shown inline. *)
  show_caret : bool;  (** Whether to display position indicators. *)
  indent : int;  (** Left margin for position indicators. *)
}
(** Configuration for formatting. *)

type t = {
  position : int;  (** Character position of first difference. *)
  line_expected : int;  (** Line number in expected string. *)
  column_expected : int;  (** Column in expected string. *)
  line_actual : int;  (** Line number in actual string. *)
  column_actual : int;  (** Column in actual string. *)
  context_before : (string * string) list;  (** Lines before difference. *)
  diff_lines : string * string;  (** Lines containing the difference. *)
  context_after : (string * string) list;  (** Lines after difference. *)
}
(** The type for string differences with context. *)

val diff : ?context_size:int -> expected:string -> string -> t option
(** [diff ?context_size ~expected ~actual] analyzes the difference between two
    strings. Returns [None] if strings are equal, otherwise returns detailed
    difference information with surrounding context lines. Default
    [context_size] is 3. *)

val pp :
  ?config:config ->
  ?expected_label:string ->
  ?actual_label:string ->
  Format.formatter ->
  t ->
  unit
(** [pp ?config ?expected_label ?actual_label fmt t] pretty-prints a string diff
    in unified diff format with adaptive line formatting. Default labels are
    "Expected" and "Actual". *)

(** {1 Utilities} *)

val first_diff_pos : string -> string -> int option
(** [first_diff_pos s1 s2] is the position of the first differing character, or
    [None] if the strings are equal. *)

val truncate_middle : int -> string -> string
(** [truncate_middle max_len s] truncates [s] to at most [max_len] characters,
    preserving both the beginning and end with ellipsis in the middle if
    truncation occurs. *)
