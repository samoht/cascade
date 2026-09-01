(** Shared helpers for fuzz test modules. *)

open Cascade
open Alcobar

let assert_invalid_declaration_contract label input =
  let css = ".x{" ^ input ^ "}" in
  match Css.of_string ~strict:true css with
  | Ok parsed ->
      failf "%s parsed strictly as invalid declaration: %S -> %S" label input
        (Css.to_string ~minify:true parsed.stylesheet)
  | Error _ ->
      let { Css.warnings; stylesheet; _ } =
        match Css.of_string ~strict:false css with
        | Ok parsed -> parsed
        | Error err ->
            failf "%s did not recover leniently: %s" label
              (Cascade.Error.to_string err)
      in
      ignore (Css.to_string ~minify:true stylesheet : string);
      if warnings = [] then
        failf "%s recovered without a lenient warning: %S" label input

let shapes_with_rule_runs ~boundary_shape ss =
  let rec loop acc seen_rule = function
    | [] -> if seen_rule then List.rev ("rules" :: acc) else List.rev acc
    | Css.Stylesheet.Rule _ :: rest -> loop acc true rest
    | other :: rest ->
        let acc = if seen_rule then "rules" :: acc else acc in
        loop (List.rev_append (boundary_shape other) acc) false rest
  in
  loop [] false ss

(* Byte shapes an ASCII alphabet cannot produce. Kept as literals rather than
   generated, so each one names a decoder path: well-formed multi-byte, the
   three ways a sequence is malformed, and the bytes no UTF-8 uses at all. *)
let byte_shapes =
  [
    "\xef\xbb\xbf" (* BOM *);
    "\xc3\xa9" (* U+00E9, two bytes *);
    "\xe2\x86\x97" (* U+2197, three bytes *);
    "\xf0\x9d\x84\x9e" (* U+1D11E, four bytes *);
    "\x80" (* continuation byte with nothing to continue *);
    "\xe2\x86" (* three-byte sequence cut short *);
    "\xc0\xaf" (* overlong encoding of [/] *);
    "\xed\xa0\x80" (* surrogate, which UTF-8 does not encode *);
    "\xff\xfe" (* never valid UTF-8 *);
    "\x00" (* NUL, which sec. 3.3 also replaces *);
  ]

(* [U+] ranges, including the spellings that end where a token boundary has to
   be decided. *)
let unicode_ranges =
  [
    "U+0-7f"; "u+41"; "U+1234-"; "U+??"; "U+"; "U+a?b"; "U+0-10FFFF"; "U+FF,U+0";
  ]

let unicodish buf =
  let at i = if buf = "" then 0 else Char.code buf.[i mod String.length buf] in
  let pick xs i = List.nth xs (at i mod List.length xs) in
  let shape = pick byte_shapes in
  let range = pick unicode_ranges in
  let b = Buffer.create 128 in
  if at 0 land 1 = 0 then Buffer.add_string b (List.hd byte_shapes);
  Buffer.add_string b ".";
  Buffer.add_string b (shape 1);
  Buffer.add_string b "x{";
  Buffer.add_string b (shape 2);
  Buffer.add_string b "-color:";
  Buffer.add_string b (shape 3);
  Buffer.add_string b ";content:\"";
  Buffer.add_string b (shape 4);
  Buffer.add_string b "\"}@font-face{unicode-range:";
  Buffer.add_string b (range 5);
  Buffer.add_string b ";src:local(";
  Buffer.add_string b (shape 6);
  Buffer.add_string b ")}@media (min-width:1px){#";
  Buffer.add_string b (shape 7);
  Buffer.add_string b "{--v:";
  Buffer.add_string b (range 8);
  Buffer.add_string b (shape 9);
  Buffer.add_string b "}}";
  Buffer.contents b
