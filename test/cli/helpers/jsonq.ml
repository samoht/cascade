(* jsonq: decode one JSON text from standard input with jsont, exit 1 with
   jsont's message if it is malformed, and print selected values the way
   Python's print() prints what json.load returns.

   Usage: jsonq.exe [LINE | --each PATH LINE]...

   Each LINE prints one output line: its space-separated words are printed
   separated by one space, a word starting with '.' being a path into the
   document and any other word printed as written. [--each PATH LINE] prints
   LINE once per element of the array at PATH, with LINE's paths starting at the
   element. A path is [.] (the value itself) or a sequence of [.key] and [[n]]
   steps, optionally followed by [:first-line] (the text before the first line
   break of a string). With no LINE the document is only checked. *)

exception Error of string

let error parts = raise (Error (String.concat "" parts))

(* An object member that repeats a name keeps the first position and takes the
   last value, as a Python dict built by json.load does. *)
let members (mems : Jsont.object') =
  List.fold_left
    (fun acc ((k, _), v) ->
      if List.mem_assoc k acc then
        List.map (fun (k', v') -> if k' = k then (k, v) else (k', v')) acc
      else acc @ [ (k, v) ])
    [] mems

(* Python rendering *)

let hex = "0123456789abcdef"

let add_repr_string b s =
  let quote =
    if String.contains s '\'' && not (String.contains s '"') then '"' else '\''
  in
  Buffer.add_char b quote;
  String.iter
    (fun c ->
      match c with
      | '\\' -> Buffer.add_string b "\\\\"
      | '\t' -> Buffer.add_string b "\\t"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | c when c = quote ->
          Buffer.add_char b '\\';
          Buffer.add_char b c
      | c when Char.code c < 0x20 || Char.code c = 0x7f ->
          Buffer.add_string b "\\x";
          Buffer.add_char b hex.[Char.code c lsr 4];
          Buffer.add_char b hex.[Char.code c land 15]
      | c when Char.code c >= 0x80 ->
          error [ "cannot render a non-ASCII string inside a container" ]
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b quote

(* jsont decodes every number to a float, so the literal is read back from the
   source: Python prints an integer literal as an int and anything else as a
   float, whose repr is not reproduced here. *)
let add_number src b meta =
  let loc = Jsont.Meta.textloc meta in
  let first = Jsont.Textloc.first_byte loc in
  let text = String.sub src first (Jsont.Textloc.last_byte loc - first + 1) in
  if String.exists (function '.' | 'e' | 'E' -> true | _ -> false) text then
    error [ "cannot render the non-integer number "; text ]
  else Buffer.add_string b (if text = "-0" then "0" else text)

let rec add_repr src b : Jsont.json -> unit = function
  | Null _ -> Buffer.add_string b "None"
  | Bool (true, _) -> Buffer.add_string b "True"
  | Bool (false, _) -> Buffer.add_string b "False"
  | Number (_, meta) -> add_number src b meta
  | String (s, _) -> add_repr_string b s
  | Array (vs, _) ->
      Buffer.add_char b '[';
      List.iteri
        (fun i v ->
          if i > 0 then Buffer.add_string b ", ";
          add_repr src b v)
        vs;
      Buffer.add_char b ']'
  | Object (mems, _) ->
      Buffer.add_char b '{';
      List.iteri
        (fun i (k, v) ->
          if i > 0 then Buffer.add_string b ", ";
          add_repr_string b k;
          Buffer.add_string b ": ";
          add_repr src b v)
        (members mems);
      Buffer.add_char b '}'

let add_str src b : Jsont.json -> unit = function
  | String (s, _) -> Buffer.add_string b s
  | v -> add_repr src b v

(* Paths *)

(* The line boundaries are those of Python's str.splitlines, and like
   [s.splitlines()[0]] the empty string has no first line. *)
let first_line path s =
  let n = String.length s in
  let has i sub =
    i + String.length sub <= n && String.sub s i (String.length sub) = sub
  in
  let rec stop i =
    if i >= n then n
    else
      match s.[i] with
      | '\n' | '\r' | '\011' | '\012' | '\028' | '\029' | '\030' -> i
      | _ when has i "\xc2\x85" || has i "\xe2\x80\xa8" || has i "\xe2\x80\xa9"
        ->
          i
      | _ -> stop (i + 1)
  in
  if s = "" then error [ path; ": the empty string has no first line" ]
  else String.sub s 0 (stop 0)

let rec select path i (v : Jsont.json) : Jsont.json =
  let n = String.length path in
  if i >= n then v
  else
    match path.[i] with
    | ':' -> (
        if String.sub path i (n - i) <> ":first-line" then
          error [ path; ": the only filter is :first-line, at the end" ];
        match v with
        | String (s, meta) -> String (first_line path s, meta)
        | _ -> error [ path; ": :first-line applies to a string" ])
    | '.' when i = 0 && (n = 1 || path.[1] = ':') -> select path 1 v
    | '.' when i + 1 >= n || List.mem path.[i + 1] [ '.'; '['; ':' ] ->
        error [ path; ": empty key" ]
    | '.' ->
        let j = ref (i + 1) in
        while !j < n && not (List.mem path.[!j] [ '.'; '['; ':' ]) do
          incr j
        done;
        let key = String.sub path (i + 1) (!j - i - 1) in
        let v =
          match v with
          | Object (mems, _) -> (
              match List.assoc_opt key (members mems) with
              | Some v -> v
              | None -> error [ path; ": no key "; key ])
          | _ -> error [ path; ": "; key; " indexes a non-object" ]
        in
        select path !j v
    | '[' ->
        let j =
          match String.index_from_opt path i ']' with
          | Some j -> j
          | None -> error [ path; ": unclosed [" ]
        in
        let idx =
          match int_of_string_opt (String.sub path (i + 1) (j - i - 1)) with
          | Some k when k >= 0 -> k
          | _ -> error [ path; ": bad index" ]
        in
        let v =
          match v with
          | Array (vs, _) -> (
              match List.nth_opt vs idx with
              | Some v -> v
              | None -> error [ path; ": index out of range" ])
          | _ -> error [ path; ": index into a non-array" ]
        in
        select path (j + 1) v
    | _ -> error [ path; ": a path step starts with '.' or '['" ]

let print_line src doc line =
  let b = Buffer.create 64 in
  List.iteri
    (fun i word ->
      if i > 0 then Buffer.add_char b ' ';
      if word <> "" && word.[0] = '.' then add_str src b (select word 0 doc)
      else Buffer.add_string b word)
    (String.split_on_char ' ' line);
  Buffer.add_char b '\n';
  print_string (Buffer.contents b)

let rec run src doc = function
  | [] -> ()
  | "--each" :: path :: line :: rest ->
      (match select path 0 doc with
      | Array (vs, _) -> List.iter (fun v -> print_line src v line) vs
      | _ -> error [ path; ": --each needs an array" ]);
      run src doc rest
  | [ "--each" ] | [ "--each"; _ ] -> error [ "--each takes PATH and LINE" ]
  | line :: rest ->
      print_line src doc line;
      run src doc rest

let () =
  set_binary_mode_in stdin true;
  let src = In_channel.input_all stdin in
  let args = List.tl (Array.to_list Sys.argv) in
  match Jsont_bytesrw.decode_string ~locs:true Jsont.json src with
  | Error msg ->
      prerr_endline msg;
      exit 1
  | Ok doc -> (
      match run src doc args with
      | () -> ()
      | exception Error msg ->
          flush stdout;
          prerr_endline (String.concat "" [ "jsonq: "; msg ]);
          exit 1)
