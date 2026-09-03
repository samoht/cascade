type t =
  | Bool of bool
  | Int of int
  | String of string
  | List of t list
  | Obj of (string * t) list

let hex_digits = "0123456789abcdef"

(* JSON has no literal for a control byte, so spell it \u00XX; the four with a
   short escape take it because it reads. *)
let add_control buf c =
  let code = Char.code c in
  Buffer.add_string buf "\\u00";
  Buffer.add_char buf hex_digits.[code lsr 4];
  Buffer.add_char buf hex_digits.[code land 0xf]

(* Bytes at or above 0x80 pass through: the input is a CSS file, so they are the
   continuation bytes of a UTF-8 sequence, and escaping them one at a time would
   spell a different character. *)
let add_escaped buf s =
  let add = Buffer.add_string buf in
  String.iter
    (fun c ->
      match c with
      | '"' -> add "\\\""
      | '\\' -> add "\\\\"
      | '\n' -> add "\\n"
      | '\r' -> add "\\r"
      | '\t' -> add "\\t"
      | '\b' -> add "\\b"
      | '\012' -> add "\\f"
      | c when Char.code c < 0x20 -> add_control buf c
      | c -> Buffer.add_char buf c)
    s

let add_indent buf level =
  Buffer.add_char buf '\n';
  for _ = 1 to 2 * level do
    Buffer.add_char buf ' '
  done

let rec add_value buf level = function
  | Bool b -> Buffer.add_string buf (if b then "true" else "false")
  | Int n -> Buffer.add_string buf (string_of_int n)
  | String s ->
      Buffer.add_char buf '"';
      add_escaped buf s;
      Buffer.add_char buf '"'
  | List [] -> Buffer.add_string buf "[]"
  | List items ->
      Buffer.add_char buf '[';
      List.iteri
        (fun i item ->
          if i > 0 then Buffer.add_char buf ',';
          add_indent buf (level + 1);
          add_value buf (level + 1) item)
        items;
      add_indent buf level;
      Buffer.add_char buf ']'
  | Obj [] -> Buffer.add_string buf "{}"
  | Obj members ->
      Buffer.add_char buf '{';
      List.iteri
        (fun i (name, value) ->
          if i > 0 then Buffer.add_char buf ',';
          add_indent buf (level + 1);
          Buffer.add_char buf '"';
          add_escaped buf name;
          Buffer.add_string buf "\": ";
          add_value buf (level + 1) value)
        members;
      add_indent buf level;
      Buffer.add_char buf '}'

let to_string t =
  let buf = Buffer.create 1024 in
  add_value buf 0 t;
  Buffer.contents buf

let pp ppf t = Fmt.string ppf (to_string t)
