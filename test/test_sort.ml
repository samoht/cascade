(** Sort module tests. *)

open Cascade

let check name s expected =
  Alcotest.(check string) name expected (Sort.to_string s)

let names () =
  check "token" Sort.Token "token";
  check "component" Sort.Component "component";
  check "block" Sort.Block "block";
  check "function" Sort.Function "function";
  check "at-rule" Sort.At_rule "at-rule";
  check "qualified-rule" Sort.Qualified_rule "qualified-rule";
  check "declaration" Sort.Declaration "declaration";
  check "selector" Sort.Selector "selector";
  check "property-value" Sort.Property_value "property-value";
  check "stylesheet" Sort.Stylesheet "stylesheet"

let suite = ("sort", [ Alcotest.test_case "names" `Quick names ])
