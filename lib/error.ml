type kind =
  | Sort_mismatch of { expected : Sort.t; found : Sort.t }
  | Unexpected_token of Token.kind
  | Missing_token of string
  | Bad_selector of string
  | Bad_value of { property : string; reason : string }
  | Bad_condition of { at_rule : string; reason : string }
  | Unknown_at_rule of string
  | Unterminated of Sort.t

(* [source] carries the raw input string so [snippet] can be materialised lazily
   by the user-facing pretty-printer. Most raised errors are caught and
   discarded inside speculative parsers ([Cursor.option] / [Cursor.one_of]), so
   building the snippet eagerly was pure waste in the hot path. *)
type t = {
  loc : Loc.t;
  sort : Sort.t;
  path : string list;
  kind : kind;
  source : string option;
  filename : string option;
}

let snippet t =
  match t.source with
  | None -> None
  | Some source -> Some (Loc.snippet source t.loc)

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
  | Unterminated s ->
      Pp.string ctx "unterminated ";
      Sort.pp ctx s

let pp : t Pp.t =
 fun ctx ({ loc; sort; path; kind; source = _; filename } as t) ->
  (match filename with
  | Some f when f <> "" ->
      Pp.string ctx f;
      Pp.string ctx ": "
  | _ -> ());
  (match path with
  | [] -> ()
  | _ ->
      Pp.string ctx (String.concat "/" path);
      Pp.string ctx ": ");
  pp_kind ctx kind;
  Pp.string ctx " at ";
  Loc.pp ctx loc;
  Pp.string ctx " (in ";
  Sort.pp ctx sort;
  Pp.char ctx ')';
  match snippet t with
  | None -> ()
  | Some { text; marker_pos; marker_len } ->
      Pp.cut ctx ();
      Pp.string ctx text;
      Pp.cut ctx ();
      Pp.string ctx (String.make marker_pos ' ');
      Pp.string ctx (String.make (max 1 marker_len) '^')

let to_string t = Pp.to_string pp t

exception Parse_error of t

let v ?(path = Loc.Path.empty) ?source ?filename ~loc ~sort kind =
  { loc; sort; path = Loc.Path.to_labels path; kind; source; filename }

let with_filename ?filename t =
  match (filename, t.filename) with
  | None, _ | _, Some _ -> t
  | Some _, None -> { t with filename }

let with_property property t =
  match t.kind with
  | Bad_value { property = ""; reason } ->
      { t with kind = Bad_value { property; reason } }
  | _ -> t

let fail e = Stdlib.raise (Parse_error e)

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

let fail_sort_mismatch loc ~sort ~expected ~found =
  fail (sort_mismatch loc ~sort ~expected ~found)

let fail_unexpected_token loc ~sort k = fail (unexpected_token loc ~sort k)
let fail_missing_token loc ~sort what = fail (missing_token loc ~sort what)
let fail_bad_selector loc reason = fail (bad_selector loc reason)

let fail_bad_value loc ~property ~reason =
  fail (bad_value loc ~property ~reason)

let fail_bad_condition loc ~at_rule ~reason =
  fail (bad_condition loc ~at_rule ~reason)

let fail_unknown_at_rule loc name = fail (unknown_at_rule loc name)
let fail_unterminated loc s = fail (unterminated loc s)
