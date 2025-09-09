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

type ctx = {
  minify : bool;  (** Whether to produce minified output *)
  indent : int;  (** Current indentation level *)
  buf : Buffer.t;  (** Output buffer *)
  inline : bool;  (** Whether to inline variables or not *)
}
(** Formatter context containing output configuration *)

type 'a t = ctx -> 'a -> unit
(** Core formatter type: writes values of type ['a] to a buffer *)

(** {2 Running Formatters} *)

val to_string : ?minify:bool -> ?inline:bool -> 'a t -> 'a -> string
(** [to_string ~minify ~inline formatter value] runs the formatter and returns a
    string. Creates a fresh buffer internally. Defaults: minify=false,
    inline=false. *)

val to_buffer : ?minify:bool -> ?inline:bool -> Buffer.t -> 'a t -> 'a -> unit
(** [to_buffer ~minify ~inline buffer formatter value] runs the formatter
    writing to the provided buffer. Defaults: minify=false, inline=false. *)

(** {2 Primitive Formatters} *)

val nop : 'a t
(** [nop] is a no-op formatter that writes nothing and ignores its input. *)

val str : string -> 'a t
(** [str str] always writes the constant string, ignoring input. *)

val string : string t
(** [string] writes a string value to the buffer. *)

val char : char t
(** [char] writes a single character to the buffer. *)

val quoted_string : string t
(** [quoted_string] writes a double-quoted string with proper escaping of quotes
    and backslashes. *)

(** {2 Layout Control}

    These formatters control whitespace and indentation for readable output.
    They respect the minification setting - producing no output when minifying.
*)

val sp : unit t
(** [sp] writes a space character when not minifying (layout whitespace). *)

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

val list_with_last : ?sep:unit t -> (is_last:bool -> 'a t) -> 'a list t
(** [list_with_last ~sep formatter] formats list where formatter receives a flag
    indicating if this is the last element. *)

val option : ?none:unit t -> 'a t -> 'a option t
(** [option ~none formatter] formats an option, using none formatter for None.
*)

val using : ('b -> 'a) -> 'a t -> 'b t
(** [using f formatter] transforms input with f before formatting (contramap).
*)

val surround : left:unit t -> right:unit t -> 'a t -> 'a t
(** [surround ~left ~right formatter] wraps formatter with left and right. *)

(** {2 Number Formatting}

    CSS number formatters that handle minification rules like dropping leading
    zeros and avoiding scientific notation *)

val float : float t
(** [float] formats floating point numbers with CSS rules:
    - Always drops leading zero for 0 < |n| < 1 (outputs .5 not 0.5)
    - No scientific notation (uses bounded precision)
    - Trims trailing zeros. *)

val float_n : int -> float t
(** [float_n n] formats float to exactly n decimal places using round-half-up.
    Used for CSS color channels and opacity where precision matters. *)

val int : int t
(** [int] formats integers. *)

val colon : unit t
(** [colon] always outputs ":". *)

val comma : unit t
(** [comma] outputs "," when minifying, ", " when formatting. *)

val sep : string t
(** [sep s] prints [s] and, when not minifying, appends a single space. *)

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

type sep = unit t
(** Type alias for separator formatters *)

val minified : ctx -> bool
(** [minified ctx] queries whether context is in minification mode. *)

val cond : (ctx -> bool) -> 'a t -> 'a t -> 'a t
(** [cond predicate then_fmt else_fmt] conditionally chooses formatter based on
    context predicate. *)

val space_if_pretty : unit t
(** [space_if_pretty] is an alias for [sp] - outputs space when not minifying.
*)

val braces : 'a t -> 'a t
(** [braces formatter] wraps formatter in braces with proper spacing and
    indentation: [{ <indented content> }] when formatting, [{<content>}] when
    minifying. *)

(** {2 Generic Helpers} *)

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
(** [url s] formats CSS url with quotes: url("s"). *)
