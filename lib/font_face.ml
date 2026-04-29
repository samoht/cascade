(** Font-face descriptor types for type-safe [\@font-face] construction. *)

(** {1 Metric Override Types} *)

(** Metric override value - either "normal" or a percentage. Used for
    ascent-override, descent-override, line-gap-override. *)
type metric_override = Normal | Percent of float

let metric_override_to_string = function
  | Normal -> "normal"
  | Percent p -> Pp.float_to_string p ^ "%"

(** {1 Size Adjust} *)

type size_adjust = float
(** Size adjustment percentage. *)

let size_adjust_to_string p = Pp.float_to_string p ^ "%"

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

type src = src_entry list
(** Font source list. *)

let url_needs_quotes s =
  String.exists
    (fun c -> c = ' ' || c = ')' || c = '"' || c = '\'' || c = '(' || c = '\\')
    s

let format_url s =
  if url_needs_quotes s then String.concat "" [ "url(\""; s; "\")" ]
  else String.concat "" [ "url("; s; ")" ]

let format_quoted_url ~quote s =
  String.concat "" [ "url("; String.make 1 quote; s; String.make 1 quote; ")" ]

let format_src_url base format tech =
  let with_format =
    match format with Some f -> base ^ " format(\"" ^ f ^ "\")" | None -> base
  in
  match tech with
  | Some t -> with_format ^ " tech(" ^ t ^ ")"
  | None -> with_format

let src_entry_to_string = function
  | Url { url; format; tech } -> format_src_url (format_url url) format tech
  | Quoted_url { url; quote; format; tech } ->
      format_src_url (format_quoted_url ~quote url) format tech
  | Local name -> "local(\"" ^ name ^ "\")"

let src_to_string ?(minify = false) entries =
  String.concat
    (if minify then "," else ", ")
    (List.map src_entry_to_string entries)

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
    | None -> Cursor.consume_remaining_to_string ~trim:true inner
  in
  Cursor.expect_eof inner;
  if value = "" then Cursor.err_invalid inner (name ^ "() argument");
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
        | None -> `Bare (Cursor.consume_remaining_to_string ~trim:true inner)
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

let read_src_entry t =
  Cursor.ws t;
  match Cursor.peek_raw t with
  | Some (Component.Func { node = { name; _ }; _ })
    when String.lowercase_ascii name = "local" ->
      Local (read_function_arg "local" t)
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

(** Parse a src string into a list of typed entries. *)
let read_src t =
  let entries = Cursor.list ~at_least:1 ~sep:Cursor.comma read_src_entry t in
  Cursor.ws t;
  if (not (Cursor.is_done t)) && not (Cursor.peek_semicolon t) then
    Cursor.err t "unexpected tokens after font src";
  entries

let src_of_string s =
  try
    let t = Cursor.of_string s in
    let entries = read_src t in
    Cursor.expect_eof t;
    entries
  with Cursor.Parse_error _ -> failwith "invalid font src"
