open Stylesheet_partial

type comment = { loc : Loc.t; terminated : bool }
type rule = { syntax : Component.rule; loc : Loc.t; owned_loc : Loc.t }
type position = { byte : int; line : int; column : int }
type span = { start : position; end_ : position }

type t = {
  original_source : string;
  preprocessed_source : string;
  comments : comment list;
  rules : rule list;
  trailing_loc : Loc.t;
  original_offsets : int array option;
  line_starts : int array;
}

let contents t = t.original_source
let preprocessed t = t.preprocessed_source
let comments t = t.comments
let rules t = t.rules
let trailing_loc t = t.trailing_loc

let check_loc t ({ Loc.start_pos; end_pos } as loc) =
  let length = String.length t.preprocessed_source in
  if start_pos < 0 || end_pos < start_pos || end_pos > length then
    invalid_arg
      (String.concat ""
         [
           "Css.Source: location ";
           Loc.to_string loc;
           " is outside [0-";
           string_of_int length;
           "]";
         ])

let slice t ({ Loc.start_pos; end_pos } as loc) =
  check_loc t loc;
  String.sub t.preprocessed_source start_pos (end_pos - start_pos)

let original_offset t offset =
  if offset < 0 || offset > String.length t.preprocessed_source then
    invalid_arg
      (String.concat ""
         [
           "Css.Source: offset ";
           string_of_int offset;
           " is outside [0-";
           string_of_int (String.length t.preprocessed_source);
           "]";
         ]);
  match t.original_offsets with
  | Option.None -> offset
  | Option.Some map -> map.(offset)

let original_loc t ({ Loc.start_pos; end_pos } as loc) =
  check_loc t loc;
  Loc.v
    ~start_pos:(original_offset t start_pos)
    ~end_pos:(original_offset t end_pos)

let original_slice t loc =
  let { Loc.start_pos; end_pos } = original_loc t loc in
  String.sub t.original_source start_pos (end_pos - start_pos)

let position t offset =
  let byte = original_offset t offset in
  let starts = t.line_starts in
  let low = ref 0 in
  let high = ref (Array.length starts) in
  while !low + 1 < !high do
    let middle = (!low + !high) / 2 in
    if starts.(middle) <= offset then low := middle else high := middle
  done;
  let line_start = starts.(!low) in
  let column =
    Common.String.utf8_length ~pos:line_start ~len:(offset - line_start)
      t.preprocessed_source
    + 1
  in
  { byte; line = !low + 1; column }

let span t ({ Loc.start_pos; end_pos } as loc) =
  check_loc t loc;
  { start = position t start_pos; end_ = position t end_pos }

let has_bom input =
  String.length input >= 3
  && input.[0] = '\xEF'
  && input.[1] = '\xBB'
  && input.[2] = '\xBF'

(* Boundary map from the CSS-preprocessed buffer back to caller bytes. The two
   interior byte boundaries of a U+FFFD replacement map to the original NUL's
   start; lexer/component locations only occur at code-point boundaries, where
   the map is exact. *)
let build_original_offsets original preprocessed =
  if String.equal original preprocessed then Option.None
  else
    let original_length = String.length original in
    let preprocessed_length = String.length preprocessed in
    let map = Array.make (preprocessed_length + 1) 0 in
    let original_pos = ref (if has_bom original then 3 else 0) in
    let preprocessed_pos = ref 0 in
    map.(0) <- !original_pos;
    let emit_byte original_end =
      incr preprocessed_pos;
      if !preprocessed_pos > preprocessed_length then
        invalid_arg "Css.Source: preprocessing map exceeds parsed source";
      map.(!preprocessed_pos) <- original_end
    in
    while !original_pos < original_length do
      match original.[!original_pos] with
      | '\x00' ->
          let start = !original_pos in
          incr original_pos;
          emit_byte start;
          emit_byte start;
          emit_byte !original_pos
      | '\r' ->
          incr original_pos;
          if !original_pos < original_length && original.[!original_pos] = '\n'
          then incr original_pos;
          emit_byte !original_pos
      | '\x0C' ->
          incr original_pos;
          emit_byte !original_pos
      | _ ->
          incr original_pos;
          emit_byte !original_pos
    done;
    if !preprocessed_pos <> preprocessed_length then
      invalid_arg "Css.Source: preprocessing map disagrees with parsed source";
    Option.Some map

let line_starts source =
  let starts = ref [ 0 ] in
  String.iteri
    (fun index byte -> if byte = '\n' then starts := (index + 1) :: !starts)
    source;
  Array.of_list (List.rev !starts)

let v ~original ~preprocessed ~comments syntax_rules =
  let previous_end = ref 0 in
  let rules =
    List.map
      (fun syntax ->
        let loc = rule_loc syntax in
        let owned_loc =
          Loc.v ~start_pos:!previous_end ~end_pos:loc.Loc.end_pos
        in
        previous_end := loc.Loc.end_pos;
        { syntax; loc; owned_loc })
      syntax_rules
  in
  let trailing_loc =
    Loc.v ~start_pos:!previous_end ~end_pos:(String.length preprocessed)
  in
  {
    original_source = original;
    preprocessed_source = preprocessed;
    comments =
      List.map
        (fun ({ Lexer.loc; terminated } : Lexer.comment) -> { loc; terminated })
        comments;
    rules;
    trailing_loc;
    original_offsets = build_original_offsets original preprocessed;
    line_starts = line_starts preprocessed;
  }
