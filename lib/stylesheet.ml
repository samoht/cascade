(** CSS stylesheet types and construction functions *)

include Stylesheet_intf

type t = stylesheet

(** {1 Construction Functions} *)

let rule ~selector ?(nested = []) ?merge_key declarations : rule =
  { selector; declarations; nested; merge_key }

let property ~syntax ?initial_value ?(inherits = false) name =
  Property { name; syntax; inherits; initial_value }

let layer_decl names = Layer_decl names
let layer ?name content = Layer (name, content)
let media ~condition content = Media (condition, content)

let media_nested ~condition declarations =
  Media (condition, [ Declarations declarations ])

let container ?name ?condition content = Container (name, condition, content)
let supports ~condition content = Supports (condition, content)
let starting_style content = Starting_style content
let with_origin cascade_origin content = Origin (cascade_origin, content)

let starting_style_nested declarations =
  Starting_style [ Declarations declarations ]

let keyframes name frames = Keyframes (name, frames)
let v statements : stylesheet = statements
let empty : stylesheet = []

include Stylesheet_cascade
include Stylesheet_equal
include Stylesheet_walk
include Stylesheet_pp
include Stylesheet_at_rule
include Stylesheet_descriptor
include Stylesheet_read
include Stylesheet_partial
module Source = Stylesheet_source

(** {1 Rendering} *)

(* One walk of the tree. Presizing the buffer from a [Pp.size] prepass would
   cost a second full walk - the counter sink skips the output bytes, not
   [normalise], [printable_statements] or any printer below them - to save a
   [Buffer] growth that is amortised anyway. *)
let to_string ?(minify = false) ?indent ?lossless ?enforce_spec (statements : t)
    =
  let pp ctx () = pp_stylesheet ctx statements in
  Pp.to_string ~minify ?indent ?lossless ?enforce_spec pp ()

(* [pair_from source c1 c2 i] is the first offset at or after [i] where [c1] is
   followed by [c2]. *)
let rec pair_from source c1 c2 i =
  if i + 1 >= String.length source then Option.None
  else if
    String.unsafe_get source i = c1 && String.unsafe_get source (i + 1) = c2
  then Some i
  else pair_from source c1 c2 (i + 1)

(* Scan [source] for [/*! ... */] bang comments. The lexer drops ordinary
   comments per CSS Syntax 3 (ED) sec. 4.3.2; bang comments are the minifier
   convention for license headers and need to round-trip. Returns pairs
   [(start_offset, body)] in source order. An ordinary comment is skipped whole,
   so a [/*!] inside it is not a bang comment; strings are not skipped, which
   only matters inside a rule, where {!interleave_bang_comments} drops the match
   anyway. *)
let extract_bang_comments (source : string) : (int * string) list =
  let rec loop acc i =
    match pair_from source '/' '*' i with
    | Option.None -> List.rev acc
    | Some start -> (
        let body_start = start + 2 in
        match pair_from source '*' '/' body_start with
        | Option.None -> List.rev acc
        | Some end_ ->
            let acc =
              if body_start < end_ && String.unsafe_get source body_start = '!'
              then
                (start, String.sub source body_start (end_ - body_start)) :: acc
              else acc
            in
            loop acc (end_ + 2))
  in
  loop [] 0

(* A bang comment is a statement only between top-level rules. One that starts
   inside a rule's span sits in its prelude, its block or one of its values; it
   is part of that rule's text, whose comments are discarded, so it is dropped
   rather than hoisted in front of the rule. *)
let interleave_bang_comments source rules sheet =
  let bangs = extract_bang_comments source in
  let rule_locs = List.map rule_loc rules in
  let rec interleave bangs rule_locs sheet =
    match (bangs, rule_locs, sheet) with
    | [], _, _ -> sheet
    | (_, body) :: rest_b, [], _ ->
        Bang_comment body :: interleave rest_b [] sheet
    | (offset, body) :: rest_b, { Loc.start_pos; _ } :: _, _
      when offset < start_pos ->
        Bang_comment body :: interleave rest_b rule_locs sheet
    | (offset, _) :: rest_b, { Loc.end_pos; _ } :: _, _ when offset < end_pos ->
        interleave rest_b rule_locs sheet
    | _, _ :: rest_s, [] -> interleave bangs rest_s []
    | _, _ :: rest_s, stmt :: rest_sheet ->
        stmt :: interleave bangs rest_s rest_sheet
  in
  interleave bangs rule_locs sheet

(* Top-level partial-recovery entry point: combine section 5.3 syntax warnings
   from [Parser.stylesheet] with per-rule typed-validation warnings. *)
let parse_stylesheet_partial ?(meta = Loc.default_meta_level)
    ?(enforce_spec = false) ?on_source (original : string) :
    stylesheet * Error.t list =
  let comments, on_comment =
    match on_source with
    | Option.None -> (Option.None, Option.None)
    | Option.Some _ ->
        let comments = ref [] in
        ( Option.Some comments,
          Option.Some (fun comment -> comments := comment :: !comments) )
  in
  let reader = Reader.of_string ~enforce_spec original in
  let out = Parser.stylesheet ~meta ?on_comment reader in
  let preprocessed = Reader.source reader in
  let sheet, typed_warnings =
    read_stylesheet_of_rules ~source:preprocessed ~meta out.value
  in
  (* Interleave preserved [/*! ... */] bang comments at their source position by
     walking the original rules and the bang-comment list in parallel; any bang
     comments after the last rule are appended at the end. The typed [sheet] may
     be shorter than [out.value] when validation drops a rule, but each typed
     statement still corresponds to the next unconsumed rule, so the position
     mapping survives. *)
  let sheet = interleave_bang_comments preprocessed out.value sheet in
  (match (on_source, comments) with
  | Option.Some emit, Option.Some comments ->
      emit
        (Source.v ~original ~preprocessed ~origin:(Reader.origin reader)
           ~comments:(List.rev !comments) out.value)
  | Option.None, Option.None -> ()
  | _ -> assert false);
  (sheet, out.warnings @ typed_warnings)

(** {1 Unknown At-Rule Construction} *)

(* An at-rule cascade has no grammar for carries its parts as raw text, so the
   constructor has to answer for text that ends the at-rule before its parts do:
   CSS Syntax 3 (ED) sec. 5.5.2 ends the prelude at the first top-level [;] or
   [{], sec. 5.5.9 ends the block at the closer matching its opener, sec. 4.3.2
   runs an unclosed [/*] to EOF, and sec. 4.3.7 lets a trailing backslash escape
   the closer written after it. Enumerating those boundaries re-derives the
   tokenizer and misses whichever one is not on the list, so read the parts back
   instead: this sits after the parser because that read is the check.

   A statement is whole when the text it prints to reads back as one unknown
   at-rule of the same name printing that same text. Nothing else is required of
   it - the printer's own separator before an escaped closer moves a byte the
   caller did not write, and that byte is what keeps the at-rule closing where
   the caller meant it to. *)
let unknown_at_rule ~name ~prelude ?block () =
  let printed statement = to_string ~minify:true [ statement ] in
  let text = printed (Unknown_at_rule { name; prelude; block }) in
  let reject reason =
    let at_rule = String.concat "" [ "@"; name ] in
    Error (Error.bad_condition Loc.dummy ~at_rule ~reason)
  in
  let read_back =
    (* A name cascade does have a grammar for reads back as that at-rule and not
       as this one, so [~name:"media"] is refused here rather than printed as a
       statement whose type disagrees with the sheet it prints to. *)
    match fst (parse_stylesheet_partial text) with
    | [ (Unknown_at_rule at as statement) ] when String.equal at.name name ->
        Option.Some statement
    | _ | (exception Error.Parse_error _) -> Option.None
  in
  match read_back with
  | Option.None -> reject "the parts do not read back as one at-rule"
  | Option.Some statement ->
      if String.equal (printed statement) text then Ok statement
      else reject "a part carries text that ends the at-rule early"
