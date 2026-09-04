(* Emit the property names [Properties.read_any_property] dispatches on, as an
   OCaml list, from the marked block of lib/properties.ml.

   The population of the accept-set differential is "every property cascade
   models", and the dispatch table is the only place that says what that is. A
   name is a population member, never an expectation: what the reader does with
   the name is what the browser arbitrates.

   scripts/check_properties.ml reads the same markers with Re. This does it
   without a dependency so the rule stays inside this directory. *)

let read_lines filename =
  let ic = open_in filename in
  let rec loop acc =
    match input_line ic with
    | line -> loop (line :: acc)
    | exception End_of_file ->
        close_in ic;
        List.rev acc
  in
  loop []

let contains line needle =
  let n = String.length needle and h = String.length line in
  let rec at i =
    i + n <= h && (String.equal (String.sub line i n) needle || at (i + 1))
  in
  n = 0 || at 0

(* The quoted names of one match arm. Only the pattern is read: a comment after
   the arrow may quote anything. *)
let arm_names line =
  let n = String.length line in
  let arrow =
    let rec at i =
      if i + 2 > n then n
      else if String.equal (String.sub line i 2) "->" then i
      else at (i + 1)
    in
    at 0
  in
  let pattern = String.sub line 0 arrow in
  let trimmed = String.trim pattern in
  if String.length trimmed = 0 || not (Char.equal trimmed.[0] '|') then []
  else
    let names = ref [] in
    let buf = Buffer.create 32 in
    let inside = ref false in
    String.iter
      (fun c ->
        if Char.equal c '"' then
          if !inside then (
            names := Buffer.contents buf :: !names;
            Buffer.clear buf;
            inside := false)
          else inside := true
        else if !inside then Buffer.add_char buf c)
      pattern;
    List.rev !names

let names_of lines =
  let rec loop inside acc = function
    | [] -> List.rev acc
    | line :: rest ->
        if contains line "PROPERTY_MATCHING_START" then loop true acc rest
        else if contains line "PROPERTY_MATCHING_END" then List.rev acc
        else if inside then
          loop true (List.rev_append (arm_names line) acc) rest
        else loop false acc rest
  in
  loop false [] lines

(* A rename in the library would otherwise empty the population without failing
   anything. *)
let minimum = 400

let () =
  let source = Sys.argv.(1) in
  let names = names_of (read_lines source) in
  if List.length names < minimum then (
    prerr_endline
      (String.concat ""
         [
           "gen_reader_properties: ";
           source;
           " yielded ";
           string_of_int (List.length names);
           " property names, fewer than the ";
           string_of_int minimum;
           " expected; the PROPERTY_MATCHING markers moved";
         ]);
    exit 1);
  let buf = Buffer.create 16384 in
  Buffer.add_string buf
    "(* Generated from lib/properties.ml by gen_reader_properties.ml. *)\n\n\
     let all =\n\
    \  [\n";
  List.iter
    (fun name ->
      Buffer.add_string buf "    \"";
      Buffer.add_string buf name;
      Buffer.add_string buf "\";\n")
    names;
  Buffer.add_string buf "  ]\n";
  print_string (Buffer.contents buf)
