(** String difference analysis and formatting. *)

type config = {
  max_width : int;
  short_threshold : int;
  show_caret : bool;
  indent : int;
}

(** Default maximum width for formatted output *)
let default_max_width = 60

let default_config =
  {
    max_width = default_max_width;
    short_threshold = 30;
    show_caret = true;
    indent = 0;
  }

(* ===== List Utilities ===== *)

let rec list_take n = function
  | [] -> []
  | _ when n <= 0 -> []
  | h :: t -> h :: list_take (n - 1) t

let rec list_drop n = function
  | [] -> []
  | lst when n <= 0 -> lst
  | _ :: t -> list_drop (n - 1) t

let rec zip_with_empty l1 l2 =
  match (l1, l2) with
  | [], [] -> []
  | h1 :: t1, [] -> (h1, "") :: zip_with_empty t1 []
  | [], h2 :: t2 -> ("", h2) :: zip_with_empty [] t2
  | h1 :: t1, h2 :: t2 -> (h1, h2) :: zip_with_empty t1 t2

(* ===== Core Functions ===== *)

let check_at_end len1 len2 = if len1 <> len2 then Some len1 else None

let first_diff_pos s1 s2 =
  let len1 = String.length s1 in
  let len2 = String.length s2 in
  let rec find i =
    if i >= len1 || i >= len2 then check_at_end (min len1 len2) (max len1 len2)
    else if s1.[i] <> s2.[i] then Some i
    else find (i + 1)
  in
  find 0

let truncate_middle max_len s =
  let len = String.length s in
  if len <= max_len then s
  else
    let ellipsis_length = 3 in
    let half_len = (max_len - ellipsis_length) / 2 in
    let start_part = String.sub s 0 half_len in
    let end_part = String.sub s (len - half_len) half_len in
    start_part ^ "..." ^ end_part

(* ===== Main Diff Type ===== *)

type t = {
  position : int; (* Character position of first difference *)
  line_expected : int;
  column_expected : int;
  line_actual : int;
  column_actual : int;
  context_before : (string * string) list;
  diff_lines : string * string;
  context_after : (string * string) list;
}

(* Find line number and column for a character position *)
let line_and_column lines pos =
  let rec find line_num char_count = function
    | [] -> (line_num - 1, pos - char_count, [])
    | line :: rest ->
        let line_len = String.length line + 1 in
        if char_count + line_len > pos then
          (line_num, pos - char_count, line :: rest)
        else find (line_num + 1) (char_count + line_len) rest
  in
  find 0 0 lines

(* Extract context lines before a given line number *)
let context_before lines line_num context_size =
  let before_lines = list_take line_num lines in
  let context_start = max 0 (List.length before_lines - context_size) in
  list_drop context_start before_lines

let diff ?(context_size = 3) ~expected actual =
  match first_diff_pos expected actual with
  | None -> None
  | Some pos ->
      let lines_expected = String.split_on_char '\n' expected in
      let lines_actual = String.split_on_char '\n' actual in

      (* Find line and column for the diff position *)
      let line_exp, col_exp, remaining_exp =
        line_and_column lines_expected pos
      in
      let line_act, col_act, remaining_act = line_and_column lines_actual pos in

      (* Get context lines before the diff *)
      let context_before_exp =
        context_before lines_expected line_exp context_size
      in
      let context_before_act =
        context_before lines_actual line_act context_size
      in
      let context_before =
        zip_with_empty context_before_exp context_before_act
      in

      (* Get the lines containing the diff *)
      let diff_line_exp = match remaining_exp with [] -> "" | h :: _ -> h in
      let diff_line_act = match remaining_act with [] -> "" | h :: _ -> h in

      (* Get context lines after the diff *)
      let context_after_exp =
        match remaining_exp with [] -> [] | _ :: t -> list_take context_size t
      in
      let context_after_act =
        match remaining_act with [] -> [] | _ :: t -> list_take context_size t
      in
      let context_after = zip_with_empty context_after_exp context_after_act in

      Some
        {
          position = pos;
          line_expected = line_exp;
          column_expected = col_exp;
          line_actual = line_act;
          column_actual = col_act;
          context_before;
          diff_lines = (diff_line_exp, diff_line_act);
          context_after;
        }

(* ===== Pretty-printing ===== *)

let pp_caret ?(indent = 0) fmt pos =
  Fmt.pf fmt "%s^@," (String.make (pos + indent) ' ')

(* Pretty-print a line pair in unified diff format *)
let pp_line_pair fmt (exp, act) =
  if exp = act then Fmt.pf fmt " %s@," exp
  else (
    if exp <> "" then Fmt.pf fmt "-%s@," exp;
    if act <> "" then Fmt.pf fmt "+%s@," act)

(* Helper to format diff lines based on their length *)
let format_diff_line ?(config = default_config) expected actual =
  match first_diff_pos expected actual with
  | None -> `Equal
  | Some diff_pos ->
      let len1 = String.length expected in
      let len2 = String.length actual in

      if len1 <= config.short_threshold && len2 <= config.short_threshold then
        `Short (expected, actual)
      else if len1 <= config.max_width && len2 <= config.max_width then
        `Medium (expected, actual, diff_pos)
      else
        (* Calculate window centered on the diff position *)
        let half = config.max_width / 2 in
        let window_start = max 0 (diff_pos - half) in
        let window_end =
          min (max len1 len2) (window_start + config.max_width)
        in

        (* Extract and format windows from strings *)
        let extract_window s len =
          if window_start >= len then ("...", 3)
          else
            let actual_end = min len window_end in
            let snippet =
              String.sub s window_start (actual_end - window_start)
            in
            let has_prefix = window_start > 0 in
            let has_suffix = window_end < len in
            let prefix = if has_prefix then "..." else "" in
            let suffix = if has_suffix then "..." else "" in
            let full_string = prefix ^ snippet ^ suffix in
            let prefix_len = if has_prefix then 3 else 0 in
            (full_string, prefix_len)
        in

        let s1_display, prefix_len1 = extract_window expected len1 in
        let s2_display, _prefix_len2 = extract_window actual len2 in

        let adjusted_pos =
          if diff_pos < window_start then 0
          else if diff_pos >= window_end then String.length s1_display - 1
          else prefix_len1 + (diff_pos - window_start)
        in
        `Long (s1_display, s2_display, adjusted_pos)

let pp ?(config = default_config) ?(expected_label = "Expected")
    ?(actual_label = "Actual") fmt t =
  Fmt.pf fmt "@[<v>Strings differ at position %d (line %d, col %d)@,@,"
    t.position t.line_expected t.column_expected;

  (* Git-style diff header *)
  Fmt.pf fmt "--- %s@," expected_label;
  Fmt.pf fmt "+++ %s@," actual_label;
  Fmt.pf fmt "@@ position %d @@@," t.position;

  (* Print context before *)
  List.iter (pp_line_pair fmt) t.context_before;

  (* Print the diff lines with appropriate formatting *)
  let diff_exp, diff_act = t.diff_lines in
  match format_diff_line ~config diff_exp diff_act with
  | `Equal ->
      Fmt.pf fmt "-%s@," diff_exp;
      Fmt.pf fmt "+%s@," diff_act
  | `Short (exp, act) ->
      Fmt.pf fmt "-%s@," exp;
      Fmt.pf fmt "+%s@," act;
      if t.line_expected = t.line_actual then
        pp_caret ~indent:1 fmt t.column_expected
  | `Medium (exp, act, pos) | `Long (exp, act, pos) ->
      Fmt.pf fmt "-%s@," exp;
      Fmt.pf fmt "+%s@," act;
      if t.line_expected = t.line_actual then pp_caret ~indent:1 fmt pos;

      (* Print context after *)
      List.iter (pp_line_pair fmt) t.context_after;
      Fmt.pf fmt "@]"
