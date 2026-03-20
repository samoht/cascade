(** Fuzz tests for the CSS Reader module.

    Tests crash safety of the core parser combinators on arbitrary input. *)

open Cascade
open Alcobar

(** Reader.of_string + is_done — must not crash. *)
let test_of_string buf =
  let r = Css.Reader.of_string buf in
  ignore (Css.Reader.is_done r)

(** Reader.ident — must not crash on arbitrary input. *)
let test_ident buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Reader.ident r) with Css.Reader.Parse_error _ -> ()

(** Reader.token — must not crash. *)
let test_token buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Reader.token r) with Css.Reader.Parse_error _ -> ()

(** Reader.number — must not crash. *)
let test_number buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Reader.number r) with Css.Reader.Parse_error _ -> ()

(** Reader.int — must not crash. *)
let test_int buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Reader.int r) with Css.Reader.Parse_error _ -> ()

(** Reader.string — must not crash (quoted string parser). *)
let test_string buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Reader.string r) with Css.Reader.Parse_error _ -> ()

(** Reader.hex — must not crash. *)
let test_hex buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Reader.hex r) with Css.Reader.Parse_error _ -> ()

(** Reader.css_value — must not crash. *)
let test_css_value buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Reader.css_value ~stops:[ ';'; '}' ] r)
  with Css.Reader.Parse_error _ -> ()

(** Reader.url — must not crash. *)
let test_url buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Reader.url r) with Css.Reader.Parse_error _ -> ()

(** Reader.bool — must not crash. *)
let test_bool buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Reader.bool r) with Css.Reader.Parse_error _ -> ()

(** Reader.pct — must not crash. *)
let test_pct buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Reader.pct r) with Css.Reader.Parse_error _ -> ()

let suite =
  ( "reader",
    [
      test_case "of_string crash safety" [ bytes ] test_of_string;
      test_case "ident crash safety" [ bytes ] test_ident;
      test_case "token crash safety" [ bytes ] test_token;
      test_case "number crash safety" [ bytes ] test_number;
      test_case "int crash safety" [ bytes ] test_int;
      test_case "string crash safety" [ bytes ] test_string;
      test_case "hex crash safety" [ bytes ] test_hex;
      test_case "css_value crash safety" [ bytes ] test_css_value;
      test_case "url crash safety" [ bytes ] test_url;
      test_case "bool crash safety" [ bytes ] test_bool;
      test_case "pct crash safety" [ bytes ] test_pct;
    ] )
