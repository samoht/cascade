(** CSS Pretty Printer

    A minification-aware printer for CSS that uses direct buffer writing for
    performance. This module provides formatting combinators that can produce
    both minified and formatted CSS output.

    The core abstraction is the formatter type ['a t = ctx -> 'a -> unit] which
    writes values of type ['a] directly to a buffer based on the context.

    Design principles:
    - Direct buffer writing (no intermediate strings)
    - Minification-aware (controlled by context)
    - Composable formatters via combinators
    - CSS-specific token handling
    - No Printf dependency (js_of_ocaml friendly) *)

module String_set : Set.S with type elt = string
(** Set of strings, used for theme variable names. *)

type out
(** The output sink a formatter writes to: a [Buffer] for serialisation, or a
    counter that records only length and last byte for {!size} (no allocation).
    Abstract - emit through {!string} / {!char} and inspect with {!last_char}.
*)

type ctx = {
  minify : bool;  (** Whether to produce minified output *)
  level : int;  (** Current nesting depth *)
  indent : int option;
      (** Indent width per nesting level. [None] disables per-level indentation
          even when not minifying. *)
  out : out;  (** Output sink *)
  inline : bool;  (** Whether to inline variables or not *)
  in_function : bool;
      (** Whether inside a CSS function (var fallback, color-mix). Affects
          keyword casing: [currentColor] becomes [currentcolor]. *)
  in_calc : bool;
      (** Inside a [calc()]: suppress canonicalisations that cross a typed leaf
          boundary ([calc] is type-aware so [<percentage>] and [<number>] are
          not interchangeable). *)
  in_feature_query : bool;
      (** Set while serialising the value of an [@supports (property: value)]
          feature test. The value is a capability predicate for that exact
          syntax, so lossy rewrites (e.g. static colour folding) are suppressed
          there. *)
  lossless : bool;
      (** Set under [--minify --lossless]: suppress colour-channel rounding and
          other colour approximations while keeping exact serialisation
          shortenings. *)
  enforce_spec : bool;
      (** Set under [--minify --enforce-spec]: emit the shortest spec-canonical
          serialisation without evergreen-target facts, so target-dependent
          shortenings (e.g. the oklch/lch chroma number -> percentage swap) are
          suppressed. *)
}
(** Formatter context containing output configuration *)

type 'a t = ctx -> 'a -> unit
(** Core formatter type: writes values of type ['a] to a buffer *)

(** {2 Running Formatters} *)

val ctx :
  ?minify:bool ->
  ?indent:int ->
  ?inline:bool ->
  ?lossless:bool ->
  ?enforce_spec:bool ->
  Buffer.t ->
  ctx
(** [ctx buf] builds a formatter context writing to [buf], for the
    serialise-to-string / measuring helpers. *)

val to_buffer :
  ?minify:bool ->
  ?indent:int ->
  ?inline:bool ->
  ?lossless:bool ->
  ?enforce_spec:bool ->
  Buffer.t ->
  'a t ->
  'a ->
  unit
(** [to_buffer buf formatter value] runs the formatter writing into [buf]. The
    optional {!val-indent} sets the per-level indent width (default: [None]
    under {!field-minify}, [Some 2] otherwise). *)

val size :
  ?minify:bool ->
  ?indent:int ->
  ?inline:bool ->
  ?lossless:bool ->
  ?enforce_spec:bool ->
  'a t ->
  'a ->
  int
(** [size formatter value] is the byte length of [to_string formatter value]
    without allocating the result string. Use it for size-based decisions
    instead of measuring [String.length (to_string ...)]. *)

val to_string :
  ?minify:bool ->
  ?indent:int ->
  ?inline:bool ->
  ?lossless:bool ->
  ?enforce_spec:bool ->
  'a t ->
  'a ->
  string
(** [to_string formatter value] runs the formatter and returns a string. The
    optional {!val-indent} sets the per-level indent width (default: [None]
    under {!field-minify}, [Some 2] otherwise). {!field-enforce_spec} suppresses
    target-dependent shortenings. *)

(** {2 Primitive Formatters} *)

val nop : 'a t
(** [nop] is a no-op formatter that writes nothing and ignores its input. *)

val string : string t
(** [string] writes a string value to the buffer. *)

val quoted : string t
(** [quoted] writes a double-quoted string value to the buffer. *)

val char : char t
(** [char] writes a single character to the buffer. *)

val last_char : ctx -> char option
(** [last_char ctx] is the most recently emitted byte, or [None] if nothing has
    been written yet. Use it for token-boundary spacing decisions instead of
    reaching into the output sink directly. *)

val quoted_string : ?quote:char -> string t
(** [quoted_string ?quote] writes a [quote]-delimited string (default ['"'])
    with proper escaping of the delimiter quote and backslashes. *)

(** {2 Layout Control}

    These formatters control whitespace and indentation for readable output.
    They respect the minification setting - producing no output when minifying.
*)

val sp : unit t
(** [sp] writes a space character when not minifying (layout whitespace). *)

val token_sp : unit t
(** [token_sp] writes a token-boundary space: a regular space in pretty mode,
    and a space under minify only when the previous output character would
    otherwise re-tokenise with the next one. Drops the space after [)] or [%]
    since both cleanly close their token (CSS Syntax 3 sec. 4). *)

val cut : unit t
(** [cut] writes a newline when not minifying. *)

val indent : 'a t -> 'a t
(** [indent formatter] runs formatter with increased indentation level. *)

val nest : int -> 'a t -> 'a t
(** [nest n formatter] runs formatter with indentation increased by n levels. *)

(** {2 Combinator Operations}

    Functions for combining and transforming formatters *)

val ( ++ ) : 'a t -> 'a t -> 'a t
(** [f ++ g] sequences two formatters: runs f then g on the same input. *)

val pair : ?sep:unit t -> 'a t -> 'b t -> ('a * 'b) t
(** [pair ~sep f g] formats a pair using f for first, g for second, with
    optional separator between them. *)

val triple : ?sep:unit t -> 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
(** [triple ~sep f g h] formats a triple using f, g, h for the three components,
    with optional separator between them. *)

val list : ?sep:unit t -> 'a t -> 'a list t
(** [list ~sep formatter] formats a list with separator between elements. *)

val column : ctx -> int
(** [column ctx] returns the current column position (chars since last newline).
*)

val list_wrap :
  ?threshold:int -> sep:unit t -> wrap_indent:int -> 'a t -> 'a list t
(** [list_wrap ?threshold ~sep ~wrap_indent formatter] formats a list like
    {!val-list} but wraps to a new line (indented by [wrap_indent] spaces) when
    the current column exceeds [threshold] (default 80). No-op when minifying.
*)

val option : ?none:unit t -> 'a t -> 'a option t
(** [option ~none formatter] formats an option, using none formatter for None.
*)

(** {2 Number Formatting}

    CSS number formatters that handle minification rules like dropping leading
    zeros and avoiding scientific notation *)

val string_of_float :
  ?drop_leading_zero:bool -> ?max_decimals:int -> float -> string
(** [string_of_float ?drop_leading_zero ?max_decimals f] converts a float to a
    string.
    - [drop_leading_zero]: if true, omits leading zero for 0 < |n| < 1 (.5
      instead of 0.5)
    - [max_decimals]: maximum decimal precision (default 8) *)

val float : float t
(** [float] formats floating point numbers with CSS rules:
    - Always drops leading zero for 0 < |n| < 1 (outputs .5 not 0.5)
    - No scientific notation (uses bounded precision)
    - Trims trailing zeros. *)

val float_compact : float t
(** [float_compact] like {!val-float} but always drops leading zeros regardless
    of minification mode. Used for oklch chroma values where Tailwind always
    uses compact format (e.g. [.034] not [0.034]). *)

val float_n : int -> float t
(** [float_n n] formats float to exactly n decimal places using round-half-up.
    Used for CSS color channels and opacity where precision matters. *)

val round_sig : int -> float -> float
(** [round_sig n f] rounds [f] to [n] significant digits. *)

val int : int t
(** [int] formats integers. *)

val hex : int t
(** [hex] formats integers as uppercase hexadecimal. *)

val unit : ctx -> float -> string -> unit
(** [unit ctx f suffix] formats a number with a unit suffix, e.g. "3.5px" or "0"
    for zero. *)

val pct : ctx -> float -> unit
(** [pct ctx f] formats a percentage value with the [%] suffix. The value is
    expected to be in the range 0-100. The unit is always emitted: CSS Values 4
    sec. 6.5 only allows the unit to drop on a zero [<length>], not a zero
    [<percentage>]. *)

val comma : unit t
(** [comma] outputs "," when minifying, ", " when formatting. *)

val semicolon : unit t
(** [semicolon] always outputs ";". *)

val slash : unit t
(** [slash] always outputs "/" (mandatory separator, no spacing control). *)

val space : unit t
(** [space] always outputs " " (mandatory lexical space, not layout). *)

val block_open : unit t
(** [block_open] outputs "\{" (block formatting controlled elsewhere). *)

val block_close : unit t
(** [block_close] outputs "\}" (block formatting controlled elsewhere). *)

(** {2 Helper Types and Functions} *)

val minified : ctx -> bool
(** [minified ctx] queries whether context is in minification mode. *)

val in_feature_query : ctx -> bool
(** [in_feature_query ctx] is true while serialising an [@supports] feature-test
    value, where lossy rewrites must be suppressed. *)

val enter_feature_query : ctx -> ctx
(** [enter_feature_query ctx] marks [ctx] as inside an [@supports] feature-test
    value. *)

val cond : (ctx -> bool) -> 'a t -> 'a t -> 'a t
(** [cond predicate then_fmt else_fmt] conditionally chooses formatter based on
    context predicate. *)

val space_if_pretty : unit t
(** [space_if_pretty] is an alias for {!val-sp} - outputs space when not
    minifying. *)

val op_char : char t
(** [op_char] outputs a character with spaces around it when not minifying.
    Useful for operators like +, -, *, / in expressions. *)

val braces : 'a t -> 'a t
(** [braces formatter] wraps formatter in braces with proper spacing and
    indentation: [{ <indented content> }] when formatting, [{<content>}] when
    minifying. *)

(** {2 Generic Helpers} *)

val semicolon_cut : unit t
(** [semicolon_cut] outputs a semicolon followed by a layout cut. *)

val braced_list : ?sep:unit t -> 'a t -> 'a list t
(** [braced_list formatter] wraps a list in braces with one item per line at one
    indent level deeper and the closing brace back at the parent's indentation.
*)

val braced_semicolon_list : 'a t -> 'a list t
(** [braced_semicolon_list formatter] is {!val-braced_list} with items separated
    by {!val-semicolon_cut} and, in pretty output, a trailing semicolon after
    the last item, matching style rule bodies. *)

val call : string -> 'a t -> 'a t
(** [call name args] formats a function call: [name( args )]. *)

val call_list : string -> 'a t -> 'a list t
(** [call_list name item] formats a function call with a comma-separated list of
    items: [name(a, b, c)]. *)

val call_2 : string -> 'a t -> 'b t -> ('a * 'b) t
(** [call_2 name a b] formats a 2-arg function call: [name(a, b)]. *)

val call_3 : string -> 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
(** [call_3 name a b c] formats a 3-arg function call: [name(a, b, c)]. *)

val url : string t
(** [url] formats a CSS url with quotes: url("s"). *)
