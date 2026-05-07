(** Font-face descriptor types for type-safe [\@font-face] construction. *)

(** {1 Metric Override Types} *)

(** Metric override value - either "normal" or a percentage. Used for
    ascent-override, descent-override, line-gap-override. *)
type metric_override = Normal | Percent of float

let string_of_metric_override = function
  | Normal -> "normal"
  | Percent p -> Pp.string_of_float p ^ "%"

(** {1 Size Adjust} *)

type size_adjust = float
(** Size adjustment percentage. *)

let string_of_size_adjust p = Pp.string_of_float p ^ "%"

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

let url_needs_quotes s =
  String.exists
    (fun c -> c = ' ' || c = ')' || c = '"' || c = '\'' || c = '(' || c = '\\')
    s

(* Emit the optional [format(...)] / [tech(...)] modifiers after a [url()] base.
   Under minify the modifiers run together with the [url()] - CSS Fonts 4 6.3.3
   doesn't require whitespace between the function calls. *)
let pp_src_modifiers ctx ~format ~tech =
  (match format with
  | Some f ->
      Pp.sp ctx ();
      Pp.string ctx "format(";
      Pp.char ctx '"';
      Pp.string ctx f;
      Pp.char ctx '"';
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
      Pp.string ctx "local(\"";
      Pp.string ctx name;
      Pp.string ctx "\")"

and string_of_src_entry entry =
  Pp.to_string ~minify:false (fun ctx e -> pp_src_entry ctx e) entry

let string_of_src ?(minify = false) entries =
  Pp.to_string ~minify pp_src entries

(** {1 Parsing} *)

(** Parse a metric override string like "normal" or "90%". *)
let metric_override_of_string s =
  let s = String.trim s in
  if String.equal s "normal" then Normal
  else if String.length s > 0 && s.[String.length s - 1] = '%' then (
    let p = float_of_string (String.sub s 0 (String.length s - 1)) in
    if p < 0. then failwith "negative metric override";
    Percent p)
  else failwith "invalid metric override"

(** Parse a size-adjust percentage like "90%". *)
let size_adjust_of_string s =
  let s = String.trim s in
  if String.length s > 0 && s.[String.length s - 1] = '%' then (
    let p = float_of_string (String.sub s 0 (String.length s - 1)) in
    if p < 0. then failwith "negative size-adjust";
    p)
  else failwith "invalid size-adjust"

let read_function_arg name t =
  Cursor.call name t @@ fun inner ->
  Cursor.ws inner;
  let value =
    match Cursor.string_opt inner with
    | Some s -> s
    | None -> Cursor.consume_remaining_as_string ~trim:true inner
  in
  Cursor.expect_eof inner;
  let _ = name in
  value

let read_url t =
  match Cursor.url_opt t with
  | Some url -> `Bare url
  | None -> (
      Cursor.call "url" t @@ fun inner ->
      Cursor.ws inner;
      let value =
        match Cursor.string_with_quote_opt inner with
        | Some (url, quote) -> `Quoted (url, quote)
        | None -> `Bare (Cursor.consume_remaining_as_string ~trim:true inner)
      in
      Cursor.expect_eof inner;
      match value with
      | `Bare "" | `Quoted ("", _) -> Cursor.err_invalid inner "url() argument"
      | _ -> value)

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
      let rec modifiers format tech =
        Cursor.ws t;
        match Cursor.option read_src_modifier t with
        | None -> (
            match url with
            | `Bare url -> Url { url; format; tech }
            | `Quoted (url, quote) -> Quoted_url { url; quote; format; tech })
        | Some (`Format value) -> modifiers (Some value) tech
        | Some (`Tech value) -> modifiers format (Some value)
      in
      modifiers None None

(** Parse a src string into a list of typed entries. CSS Fonts 4 §4.3 spells
    [src] as a comma-separated list, but real-world input occasionally drops the
    comma between entries ([src: local("") url(test.woff)]). Match cleancss /
    lightningcss / esbuild and accept the whitespace-only form too, treating it
    as if a comma were present. *)
and read_src t =
  let sep t =
    Cursor.ws t;
    if not (Cursor.comma_opt t) then
      (* No comma - whitespace alone separates entries; succeed silently so
         [Cursor.list]'s peeker calls back into [read_src_entry]. *)
      ()
  in
  let entries = Cursor.list ~at_least:1 ~sep read_src_entry t in
  Cursor.ws t;
  if (not (Cursor.is_done t)) && not (Cursor.peek_semicolon t) then
    Cursor.err t "unexpected tokens after font src";
  entries

let src_of_string s =
  let normalize_entry = function
    | Quoted_url { url; format; tech; _ } -> Url { url; format; tech }
    | entry -> entry
  in
  try
    let t = Cursor.of_string s in
    let entries = read_src t in
    Cursor.expect_eof t;
    List.map normalize_entry entries
  with Cursor.Parse_error _ -> failwith "invalid font src"
