type t = Str of string | Int of int | Arr of t list | Obj of (string * t) list

let escape buf s =
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      (* The driver embeds the payload in a <script> element, so no character
         that can close it or start an entity may survive as itself. *)
      | '<' -> Buffer.add_string buf "\\u003c"
      | '>' -> Buffer.add_string buf "\\u003e"
      | '&' -> Buffer.add_string buf "\\u0026"
      | c when Char.code c < 0x20 ->
          let hex n = "0123456789abcdef".[n land 0xf] in
          Buffer.add_string buf "\\u00";
          Buffer.add_char buf (hex (Char.code c lsr 4));
          Buffer.add_char buf (hex (Char.code c))
      | c -> Buffer.add_char buf c)
    s

let rec write buf = function
  | Str s ->
      Buffer.add_char buf '"';
      escape buf s;
      Buffer.add_char buf '"'
  | Int i -> Buffer.add_string buf (string_of_int i)
  | Arr l ->
      Buffer.add_char buf '[';
      List.iteri
        (fun i v ->
          if i > 0 then Buffer.add_char buf ',';
          write buf v)
        l;
      Buffer.add_char buf ']'
  | Obj fields ->
      Buffer.add_char buf '{';
      List.iteri
        (fun i (k, v) ->
          if i > 0 then Buffer.add_char buf ',';
          Buffer.add_char buf '"';
          escape buf k;
          Buffer.add_string buf "\":";
          write buf v)
        fields;
      Buffer.add_char buf '}'

let to_string v =
  let buf = Buffer.create 1024 in
  write buf v;
  Buffer.contents buf

let pp ppf v = Fmt.string ppf (to_string v)
