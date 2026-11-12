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
  origin : Reader.origin;
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
  Reader.original_offset t.origin offset

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

let line_starts source =
  let starts = ref [ 0 ] in
  String.iteri
    (fun index byte -> if byte = '\n' then starts := (index + 1) :: !starts)
    source;
  Array.of_list (List.rev !starts)

let v ~original ~preprocessed ~origin ~comments syntax_rules =
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
    origin;
    line_starts = line_starts preprocessed;
  }
