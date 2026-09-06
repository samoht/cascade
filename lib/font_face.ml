(** Font-face descriptor types for type-safe [\@font-face] construction. *)

open Syntax

(** {1 Metric Override Types} *)

(** Metric override value - either "normal", a percentage, or an unresolved
    [var()]. Used for ascent-override, descent-override, line-gap-override. *)
type metric_override =
  | Normal
  | Percent of float
  | Var of metric_override Values.var

let rec pp_metric_override ctx = function
  | Normal -> Pp.string ctx "normal"
  | Percent p ->
      Pp.string ctx (Pp.string_of_float p);
      Pp.char ctx '%'
  | Var var -> Values.pp_var pp_metric_override ctx var

let string_of_metric_override = Pp.to_string ~minify:false pp_metric_override

(** {1 Size Adjust} *)

(** CSS Fonts 5 sec. 4.4: a [<percentage [0,inf]>] glyph size multiplier. No
    descriptor grammar takes a [var()] (CSS Fonts 4 sec. 4.1), so [Var] parks a
    reference until the inline pass substitutes it. *)
type size_adjust = Pct of float | Var of size_adjust Values.var

let rec pp_size_adjust ctx : size_adjust -> unit = function
  | Pct p ->
      Pp.string ctx (Pp.string_of_float p);
      Pp.char ctx '%'
  | Var var -> Values.pp_var pp_size_adjust ctx var

let string_of_size_adjust = Pp.to_string ~minify:false pp_size_adjust

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

let equal_metric_override (a : metric_override) b = a = b
let equal_size_adjust (a : size_adjust) b = a = b
let compare_size_adjust (a : size_adjust) b = compare a b
let equal_src (a : src) b = a = b

(* Emit the optional [format(...)] / [tech(...)] modifiers after a [url()] base.
   Under minify the modifiers run together with the [url()] - CSS Fonts 4 6.3.3
   doesn't require whitespace between the function calls. *)
let known_format_keywords =
  [
    "woff2";
    "woff";
    "truetype";
    "opentype";
    "embedded-opentype";
    "svg";
    "collection";
  ]

let pp_src_modifiers ctx ~format ~tech =
  (match format with
  | Some f ->
      Pp.sp ctx ();
      Pp.string ctx "format(";
      (* CSS Fonts 4 sec. 4.3: [format()] accepts a [<font-format>] keyword or a
         [<string>]; under minify the unquoted keyword form is shorter for the
         known formats ([woff2], [woff], [truetype], [opentype], ...). *)
      if
        Pp.minified ctx
        && List.mem (String.lowercase_ascii f) known_format_keywords
      then Pp.string ctx (String.lowercase_ascii f)
      else (
        Pp.char ctx '"';
        Pp.string ctx f;
        Pp.char ctx '"');
      Pp.char ctx ')'
  | None -> ());
  match tech with
  | Some t ->
      Pp.sp ctx ();
      Pp.string ctx "tech(";
      Pp.string ctx t;
      Pp.char ctx ')'
  | None -> ()

(* Emit a [url()] argument: quote when the body contains characters that the
   bare form can't hold, drop quotes otherwise. CSS Values 4 3.4 makes the two
   notations equivalent and the bare form is shorter under minify. *)
let pp_url_arg ctx s =
  if url_needs_quotes s then (
    Pp.char ctx '"';
    Pp.string ctx s;
    Pp.char ctx '"')
  else Pp.string ctx s

(* CSS Values 4 sec. 3.3: the CSS-wide keywords are not valid [<custom-ident>]s,
   and neither is the reserved [default]. Both exclusions hold in every ASCII
   case permutation, so such a name has only its [<string>] spelling. *)
let excluded_from_custom_ident s =
  match String.lowercase_ascii s with
  | "initial" | "inherit" | "unset" | "revert" | "revert-layer" | "default" ->
      true
  | _ -> false

let local_name_can_unquote name =
  String.length name > 0
  && (not (excluded_from_custom_ident name))
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> true | _ -> false)
       name
  && match name.[0] with 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false

let rec pp_src ctx entries = Pp.list ~sep:Pp.comma pp_src_entry ctx entries

and pp_src_entry ctx = function
  | Var var -> Values.pp_var pp_src ctx var
  | Url { url; format; tech } ->
      Pp.string ctx "url(";
      pp_url_arg ctx url;
      Pp.char ctx ')';
      pp_src_modifiers ctx ~format ~tech
  | Quoted_url { url; quote; format; tech } ->
      Pp.string ctx "url(";
      if Pp.minified ctx && not (url_needs_quotes url) then Pp.string ctx url
      else (
        Pp.char ctx quote;
        Pp.string ctx url;
        Pp.char ctx quote);
      Pp.char ctx ')';
      pp_src_modifiers ctx ~format ~tech
  | Local name ->
      Pp.string ctx "local(";
      (* CSS Fonts 4 sec. 4.3: [local(<family-name>)] accepts a [<custom-ident>]
         sequence or a [<string>]; under minify the unquoted ident form is
         shorter when the family name parses as a valid [<custom-ident>]
         (alphanumeric + dashes, no leading digit, not a CSS-wide keyword). *)
      if Pp.minified ctx && local_name_can_unquote name then Pp.string ctx name
      else (
        Pp.char ctx '"';
        Pp.string ctx name;
        Pp.char ctx '"');
      Pp.char ctx ')'

let string_of_src_entry entry =
  Pp.to_string ~minify:false (fun ctx e -> pp_src_entry ctx e) entry

let string_of_src ?(minify = false) entries =
  Pp.to_string ~minify pp_src entries

let pp = pp_src
let to_string = string_of_src

(** {1 Parsing} *)

let valid_percentage p = Float.is_finite p && p >= 0.

(* CSS Fonts 4 sec. 4.10: [normal | <percentage [0,inf]>]. The token is left in
   place on a mismatch, so the error carries the offending value's span rather
   than whatever follows it. *)
let rec read_metric_override t : metric_override =
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Ident "normal"; _ }) ->
      Cursor.skip t;
      Normal
  | Some (Component.Preserved { kind = Token.Percentage number; _ })
    when valid_percentage number.Token.value ->
      Cursor.skip t;
      Percent number.Token.value
  | Some (Component.Func { node = { name; _ }; _ })
    when String.lowercase_ascii name = "var" ->
      Var (Values.read_var read_metric_override t)
  | Some _ | None -> Cursor.err_invalid t "metric override"

(* CSS Fonts 5 sec. 4.4: [<percentage [0,inf]>]. *)
let rec read_size_adjust t : size_adjust =
  match Cursor.peek t with
  | Some (Component.Func { node = { name; _ }; _ })
    when String.lowercase_ascii name = "var" ->
      Var (Values.read_var read_size_adjust t)
  | Some (Component.Preserved { kind = Token.Percentage number; _ })
    when valid_percentage number.Token.value ->
      Cursor.skip t;
      Pct number.Token.value
  | Some _ | None -> Cursor.err_invalid t "size-adjust"

let read_whole read s =
  let t = Cursor.of_string s in
  let value = read t in
  Cursor.ws t;
  Cursor.expect_eof t;
  value

let metric_override_of_string = read_whole read_metric_override
let size_adjust_of_string = read_whole read_size_adjust

let read_function_arg name t =
  Cursor.call name t @@ fun inner ->
  Cursor.ws inner;
  let value, from_string =
    match Cursor.string_opt inner with
    | Some s -> (s, true)
    | None -> (Cursor.consume_remaining_as_string ~trim:true inner, false)
  in
  Cursor.expect_eof inner;
  (* CSS Fonts 4 sec. 4.3: each of [local()] / [format()] / [tech()] takes
     exactly one argument, so an empty body ([format()], [local()]) is invalid.
     The one exception browsers accept is [local("")] - an explicit empty
     <string> family name - so keep that. *)
  let is_empty_local_string =
    from_string && value = "" && String.lowercase_ascii name = "local"
  in
  if value = "" && not is_empty_local_string then
    Cursor.err_invalid inner ("empty " ^ name ^ "()");
  value

(* CSS Fonts 4 (ED) sec. 4.3.1 writes [<font-src>] as [<url> [format(...)]?
   [tech(...)]? | local(...)], and puts no emptiness rule on the [<url>]. sec.
   4.3 leaves an unusable reference to loading, where the user agent "loads the
   next font in the list", so an empty url is a font that never arrives rather
   than a descriptor that does not parse. Chrome 151 keeps it. *)
let read_url t =
  match Cursor.url_opt t with
  | Some url -> `Bare url
  | None ->
      Cursor.call "url" t @@ fun inner ->
      Cursor.ws inner;
      let value =
        match Cursor.string_with_quote_opt inner with
        | Some (url, quote) -> `Quoted (url, quote)
        | None -> `Bare (Cursor.consume_remaining_as_string ~trim:true inner)
      in
      Cursor.expect_eof inner;
      value

let read_src_modifier t =
  Cursor.ws t;
  match Cursor.peek_raw t with
  | Some (Component.Func { node = { name; _ }; _ })
    when String.lowercase_ascii name = "format" ->
      `Format (read_function_arg "format" t)
  | Some (Component.Func { node = { name; _ }; _ })
    when String.lowercase_ascii name = "tech" ->
      `Tech (read_function_arg "tech" t)
  | _ -> Cursor.err_expected t "font source modifier"

let finalise_src_url url format tech =
  match url with
  | `Bare url -> Url { url; format; tech }
  | `Quoted (url, quote) -> Quoted_url { url; quote; format; tech }

let read_src_url_modifiers t url =
  let rec loop format tech =
    Cursor.ws t;
    match Cursor.option read_src_modifier t with
    | None -> finalise_src_url url format tech
    | Some (`Format value) ->
        if Option.is_some format then
          Cursor.err_invalid t "duplicate font source format()";
        loop (Some value) tech
    | Some (`Tech value) ->
        if Option.is_some tech then
          Cursor.err_invalid t "duplicate font source tech()";
        loop format (Some value)
  in
  loop None None

let rec read_src_entry t =
  Cursor.ws t;
  match Cursor.peek_raw t with
  | Some (Component.Func { node = { name; _ }; _ })
    when String.lowercase_ascii name = "local" ->
      Local (read_function_arg "local" t)
  | Some (Component.Func { node = { name; _ }; _ })
    when String.lowercase_ascii name = "var" ->
      Var (Values.read_var read_src t)
  | _ ->
      let url = read_url t in
      read_src_url_modifiers t url

(** Parse a src string into a list of typed entries. CSS Fonts 4 sec. 4.3 spells
    [src] as a comma-separated list, but real-world input occasionally drops the
    comma between entries ([src: local("") url(test.woff)]). Match cleancss /
    lightningcss / esbuild and accept the whitespace-only form too, treating it
    as if a comma were present. *)
(* sec. 4.3.1: an entry whose component value does not parse, or whose format or
   tech the agent does not support, is left out of the list rather than failing
   the descriptor, and only an empty list at the end is the parse error. The
   component value is the whole comma-separated item, so an item is kept or
   dropped as one: [url(f.woff2) garbage] is a single item that does not parse
   and takes the descriptor with it, while a trailing comma and an unreadable
   neighbour cost themselves alone. Chrome 151 answers the same way on each. *)
and read_src t =
  let at_end () = Cursor.is_done t || Cursor.peek_semicolon t in
  let rec skip_item () =
    if at_end () || Cursor.comma_opt t then ()
    else (
      Cursor.skip t;
      skip_item ())
  in
  (* [kept] is the items that parsed; [item] the entries of the one being read,
     which the whitespace form above can make several. *)
  let rec loop kept item =
    Cursor.ws t;
    if at_end () then item @ kept
    else if Cursor.comma_opt t then loop (item @ kept) []
    else
      match read_src_entry t with
      | entry -> loop kept (entry :: item)
      | exception Cursor.Parse_error _ ->
          skip_item ();
          loop kept []
  in
  let entries = List.rev (loop [] []) in
  if entries = [] then Cursor.err_invalid t "no readable font src";
  entries

let src_of_string s =
  let normalize_entry = function
    | Quoted_url { url; format; tech; _ } -> Url { url; format; tech }
    | entry -> entry
  in
  let t = Cursor.of_string s in
  let entries = read_src t in
  Cursor.expect_eof t;
  List.map normalize_entry entries
