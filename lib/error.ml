module Recovery = struct
  type construct = Declaration | Rule

  type t =
    | Dropped of { construct : construct; text : string option }
    | Recovered

  let equal_construct a b =
    match (a, b) with
    | Declaration, Declaration | Rule, Rule -> true
    | (Declaration | Rule), _ -> false

  let equal a b =
    match (a, b) with
    | Dropped a, Dropped b ->
        equal_construct a.construct b.construct
        && Option.equal String.equal a.text b.text
    | Recovered, Recovered -> true
    | (Dropped _ | Recovered), _ -> false

  (* A span outside the source names nothing, and a zero-width one names no
     bytes, so neither yields text. *)
  let text_of source { Loc.start_pos; end_pos } =
    if start_pos >= 0 && end_pos > start_pos && end_pos <= String.length source
    then Some (String.sub source start_pos (end_pos - start_pos))
    else None

  let dropped ?source ?loc construct =
    let text =
      match (source, loc) with
      | Some source, Some loc -> text_of source loc
      | Some _, None | None, Some _ | None, None -> None
    in
    Dropped { construct; text }
end

type kind =
  | Sort_mismatch of { expected : Sort.t; found : Sort.t }
  | Unexpected_token of Token.kind
  | Missing_token of string
  | Bad_selector of string
  | Bad_value of { property : string; reason : string }
  | Bad_condition of { at_rule : string; reason : string }
  | Unknown_at_rule of string
  | Invalid_statement of string
  | Unterminated of Sort.t
  | Too_deep of int

(* [source] carries the raw input string so [snippet] can be materialised lazily
   by the user-facing pretty-printer. Most raised errors are caught and
   discarded inside speculative parsers ([Cursor.option] / [Cursor.one_of]), so
   building the snippet eagerly was pure waste in the hot path. *)
module Source = struct
  type t = Line_col.t

  let v = Line_col.v
  let text = Line_col.source
  let locate t pos = Line_col.locate t pos
end

type t = {
  loc : Loc.t;
  sort : Sort.t;
  path : string list;
  kind : kind;
  source : Source.t option;
  filename : string option;
  recovery : Recovery.t;
}

let snippet t =
  match t.source with
  | None -> None
  | Some source -> Some (Loc.snippet (Source.text source) t.loc)

let position t =
  match t.source with
  | None -> None
  | Some source -> Some (Source.locate source t.loc.start_pos)

let context t =
  {
    Loc.Context.path = Loc.Path.of_labels t.path;
    loc = t.loc;
    sort = t.sort;
    snippet = snippet t;
  }

let pp_kind : kind Pp.t =
 fun ctx -> function
  | Sort_mismatch { expected; found } ->
      Pp.string ctx "expected ";
      Sort.pp ctx expected;
      Pp.string ctx " but found ";
      Sort.pp ctx found
  | Unexpected_token k ->
      Pp.string ctx "unexpected ";
      Token.pp_kind ctx k
  | Missing_token what ->
      Pp.string ctx "missing ";
      Pp.string ctx what
  | Bad_selector reason ->
      Pp.string ctx "bad selector: ";
      Pp.string ctx reason
  | Bad_value { property = ""; reason } ->
      Pp.string ctx "bad value: ";
      Pp.string ctx reason
  | Bad_value { property; reason } ->
      Pp.string ctx "bad value for ";
      Pp.string ctx property;
      Pp.string ctx ": ";
      Pp.string ctx reason
  | Bad_condition { at_rule; reason } ->
      Pp.string ctx "bad condition for ";
      Pp.string ctx at_rule;
      Pp.string ctx ": ";
      Pp.string ctx reason
  | Unknown_at_rule name ->
      Pp.string ctx "unknown at-rule @";
      Pp.string ctx name
  | Invalid_statement reason -> Pp.string ctx reason
  | Unterminated s ->
      Pp.string ctx "unterminated ";
      Sort.pp ctx s
  | Too_deep limit ->
      Pp.string ctx "nested deeper than ";
      Pp.int ctx limit;
      Pp.string ctx " levels"

(* A kind that names what was being read says enough; the others say it after
   the reason. *)
let pp_message : t Pp.t =
 fun ctx t ->
  pp_kind ctx t.kind;
  match t.kind with
  | Unexpected_token _ | Missing_token _ | Too_deep _ ->
      Pp.string ctx " in ";
      Sort.pp ctx t.sort
  | Sort_mismatch _ | Bad_selector _ | Bad_value _ | Bad_condition _
  | Unknown_at_rule _ | Invalid_statement _ | Unterminated _ ->
      ()

(* [source:line:column], the form an editor jumps to. An error without a source
   has no line to give, and a byte range means nothing to a reader without the
   text, so it names the source alone. *)
let pp_location ctx t =
  Pp.string ctx
    (match t.filename with Some f when f <> "" -> f | _ -> "<CSS input>");
  match position t with
  | Some (line, col) ->
      Pp.char ctx ':';
      Pp.string ctx (Int.to_string line);
      Pp.char ctx ':';
      Pp.string ctx (Int.to_string col)
  | None -> ()

(* The snippet is one line of the source, indented under the location, with the
   caret under the marked span. Both counts are columns rather than bytes, so a
   space and a caret apiece line the marker up under a multibyte snippet. *)
let pp_snippet ctx t =
  match snippet t with
  | None -> ()
  | Some { text; marker_pos; marker_len } ->
      Pp.cut ctx ();
      Pp.string ctx "  ";
      Pp.string ctx text;
      Pp.cut ctx ();
      Pp.string ctx "  ";
      Pp.string ctx (String.make marker_pos ' ');
      Pp.string ctx (String.make marker_len '^')

type severity = [ `Error | `Warning ]

let pp_with ?severity ctx t =
  pp_location ctx t;
  Pp.string ctx ": ";
  (match severity with
  | Some `Error -> Pp.string ctx "error: "
  | Some `Warning -> Pp.string ctx "warning: "
  | None -> ());
  pp_message ctx t;
  pp_snippet ctx t

let pp ctx t = pp_with ctx t
let to_string ?severity t = Pp.to_string (pp_with ?severity) t

exception Parse_error of t

(* A raised error is about a construct nothing has decided about yet, so it
   starts at [Recovered] and the site that catches it stamps what it did. *)
let v ?(path = Loc.Path.empty) ?source ?filename ~loc ~sort kind =
  {
    loc;
    sort;
    path = Loc.Path.to_labels path;
    kind;
    source = Option.map Source.v source;
    filename;
    recovery = Recovery.Recovered;
  }

let in_source source ~loc ~sort kind =
  {
    loc;
    sort;
    path = [];
    kind;
    source = Some source;
    filename = None;
    recovery = Recovery.Recovered;
  }

let with_recovery recovery t = { t with recovery }

let with_filename ?filename t =
  match (filename, t.filename) with
  | None, _ | _, Some _ -> t
  | Some _, None -> { t with filename }

let with_property property t =
  match t.kind with
  | Bad_value { property = ""; reason } ->
      { t with kind = Bad_value { property; reason } }
  | _ -> t

let as_selector t =
  match t.kind with
  | Bad_value { property = ""; reason } ->
      { t with kind = Bad_selector reason; sort = Sort.Selector }
  | _ -> t

let fail e = Stdlib.raise (Parse_error e)
let catch f = match f () with v -> Ok v | exception Parse_error e -> Error e

let with_context label f =
  try f ()
  with Parse_error e ->
    let path = label :: e.path in
    Stdlib.raise (Parse_error { e with path })

let sort_mismatch loc ~sort ~expected ~found =
  v ~loc ~sort (Sort_mismatch { expected; found })

let unexpected_token loc ~sort k = v ~loc ~sort (Unexpected_token k)
let missing_token loc ~sort what = v ~loc ~sort (Missing_token what)
let bad_selector loc reason = v ~loc ~sort:Sort.Selector (Bad_selector reason)

let bad_value loc ~property ~reason =
  v ~loc ~sort:Sort.Property_value (Bad_value { property; reason })

let bad_condition loc ~at_rule ~reason =
  v ~loc ~sort:Sort.At_rule (Bad_condition { at_rule; reason })

let unknown_at_rule loc name = v ~loc ~sort:Sort.At_rule (Unknown_at_rule name)
let unterminated loc s = v ~loc ~sort:s (Unterminated s)
