(** Sort module tests. *)

open Cascade

let check name s expected =
  Alcotest.(check string) name expected (Css.Sort.to_string s)

let names () =
  check "token" Css.Sort.Token "token";
  check "component" Css.Sort.Component "component";
  check "block" Css.Sort.Block "block";
  check "function" Css.Sort.Function "function";
  check "at-rule" Css.Sort.At_rule "at-rule";
  check "qualified-rule" Css.Sort.Qualified_rule "qualified-rule";
  check "declaration" Css.Sort.Declaration "declaration";
  check "selector" Css.Sort.Selector "selector";
  check "property-value" Css.Sort.Property_value "property-value";
  check "stylesheet" Css.Sort.Stylesheet "stylesheet"

let suite = ("sort", [ Alcotest.test_case "names" `Quick names ])
