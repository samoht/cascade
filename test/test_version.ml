open Alcotest
open Css

let test_version () =
  let v = Version.version in
  check bool "version not empty" true (String.length v > 0)

let test_header () =
  let h = Version.header in
  check bool "header contains tw" true (Astring.String.is_infix ~affix:"tw" h);
  check bool "header contains MIT" true (Astring.String.is_infix ~affix:"MIT" h);
  check bool "header is CSS comment" true
    (Astring.String.is_prefix ~affix:"/*!" h
    && Astring.String.is_suffix ~affix:"*/" h)

let tests =
  [
    test_case "version string" `Quick test_version;
    test_case "header format" `Quick test_header;
  ]

let suite = ("version", tests)
