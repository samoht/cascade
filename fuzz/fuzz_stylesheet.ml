(** Fuzz tests for the CSS Stylesheet module.

    Tests crash safety of stylesheet, rule, and declaration parsing. *)

open Cascade
open Alcobar

(** read_stylesheet — must not crash on arbitrary input. *)
let test_read_stylesheet buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Stylesheet.read_stylesheet r)
  with Css.Reader.Parse_error _ -> ()

(** read_rule — must not crash. *)
let test_read_rule buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Stylesheet.read_rule r)
  with Css.Reader.Parse_error _ | Invalid_argument _ -> ()

(** read_block — must not crash. *)
let test_read_block buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Stylesheet.read_block r) with Css.Reader.Parse_error _ -> ()

(** read (legacy) — must not crash. *)
let test_read buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Stylesheet.read r) with Css.Reader.Parse_error _ -> ()

(** read_import_rule — must not crash. *)
let test_read_import_rule buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Stylesheet.read_import_rule r)
  with Css.Reader.Parse_error _ -> ()

(** read_config — must not crash. *)
let test_read_config buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Stylesheet.read_config r)
  with Css.Reader.Parse_error _ -> ()

(** read_declaration — must not crash. *)
let test_read_declaration buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Declaration.read_declaration r)
  with Css.Reader.Parse_error _ -> ()

(** read_declarations — must not crash. *)
let test_read_declarations buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Declaration.read_declarations r)
  with Css.Reader.Parse_error _ -> ()

(** read_property_name — must not crash. *)
let test_read_property_name buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Declaration.read_property_name r)
  with Css.Reader.Parse_error _ -> ()

(** read_property_value — must not crash. *)
let test_read_property_value buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Declaration.read_property_value r)
  with Css.Reader.Parse_error _ -> ()

(** Stylesheet roundtrip: parse → to_string → parse should not crash. *)
let test_stylesheet_roundtrip buf =
  let r = Css.Reader.of_string buf in
  match
    try Some (Css.Stylesheet.read_stylesheet r)
    with Css.Reader.Parse_error _ -> None
  with
  | None -> ()
  | Some ss -> (
      let s = Css.Stylesheet.to_string ss in
      let r2 = Css.Reader.of_string s in
      try ignore (Css.Stylesheet.read_stylesheet r2)
      with Css.Reader.Parse_error _ ->
        fail "stylesheet roundtrip re-parse failed")

let suite =
  ( "stylesheet",
    [
      test_case "read_stylesheet crash safety" [ bytes ] test_read_stylesheet;
      test_case "read_rule crash safety" [ bytes ] test_read_rule;
      test_case "read_block crash safety" [ bytes ] test_read_block;
      test_case "read crash safety" [ bytes ] test_read;
      test_case "read_import_rule crash safety" [ bytes ] test_read_import_rule;
      test_case "read_config crash safety" [ bytes ] test_read_config;
      test_case "read_declaration crash safety" [ bytes ] test_read_declaration;
      test_case "read_declarations crash safety" [ bytes ]
        test_read_declarations;
      test_case "read_property_name crash safety" [ bytes ]
        test_read_property_name;
      test_case "read_property_value crash safety" [ bytes ]
        test_read_property_value;
      test_case "roundtrip" [ bytes ] test_stylesheet_roundtrip;
    ] )
