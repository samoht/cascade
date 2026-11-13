(** Tests for how deep the parser builds component values, and what it keeps of
    input nested deeper. *)

open Cascade

(* [opener] [n] times, then [inner], then [closer] [n] times. *)
let nested n ~opener ~inner ~closer =
  let b = Buffer.create ((n * 4) + String.length inner) in
  for _ = 1 to n do
    Buffer.add_string b opener
  done;
  Buffer.add_string b inner;
  for _ = 1 to n do
    Buffer.add_string b closer
  done;
  Buffer.contents b

let is_too_deep (e : Error.t) =
  match e.kind with Error.Too_deep _ -> true | _ -> false

(* [Parser.max_nesting_depth]: the most blocks and functions the parser builds
   into a tree. *)
let limit = 128

(* Run [f] with the OCaml stack capped at 128K words. Before the parser had a
   depth limit, the stack each level of input cost grew linearly: nested rules
   overflowed OCaml 5's default stack of 128M words at about 4.8 million levels
   and this one at under 5,000, nested parens at under 22,000, so the cap
   reproduces the overflow on an input small enough for a quick test. *)
let with_small_stack f =
  let previous = Gc.get () in
  Gc.set { previous with Gc.stack_limit = 1 lsl 17 };
  Fun.protect ~finally:(fun () -> Gc.set previous) f

let parse_ok css =
  match Css.of_string css with
  | Ok parse -> parse
  | Error e ->
      Alcotest.failf "lenient parse must recover: %s" (Error.to_string e)

let minified (parse : Css.parse) = Css.to_string ~minify:true parse.stylesheet
let has s affix = Astring.String.is_infix ~affix s

let check_one_too_deep what ~construct warnings =
  match List.filter is_too_deep warnings with
  | [ { Error.recovery = Error.Recovery.Dropped d; _ } ] ->
      Alcotest.(check bool)
        (String.concat "" [ what; ": the warning names the dropped construct" ])
        true
        (Error.Recovery.equal_construct construct d.construct)
  | ws ->
      Alcotest.failf "%s: expected one dropped too-deep warning, got %d" what
        (List.length ws)

(* What one rule keeps at [depth] levels of a value: CSS Syntax 3 (ED) sets no
   nesting limit, sec. 5.5.9 and 5.5.10 consuming a block and a function to
   their closer whatever they hold, and Chrome keeps a custom property nested
   100,000 deep in [(], [[], [{] or [a(] whole (the [deep_value_] cases of
   test/render/parse_recovery.ml). The rule's block is one level, so [depth]
   openers nest the innermost value [depth + 1] deep. *)
let value_openers = [ ("(", ")"); ("[", "]"); ("{", "}"); ("a(", ")") ]

let check_value_kept what ~depth (opener, closer) =
  let value = nested depth ~opener ~inner:"x" ~closer in
  let css = String.concat "" [ ".a{--x:"; value; "}.b{color:red}" ] in
  let parse = parse_ok css in
  let label s = String.concat "" [ what; " "; opener; ": "; s ] in
  Alcotest.(check int) (label "no warning") 0 (List.length parse.warnings);
  Alcotest.(check string) (label "printed back as written") css (minified parse)

let nesting_at_the_limit () =
  (* The innermost value sits at exactly the limit and is built as a tree,
     printed and optimised as before. *)
  List.iter (check_value_kept "at the limit" ~depth:(limit - 1)) value_openers;
  let rules = nested (limit - 1) ~opener:".a{" ~inner:"color:red" ~closer:"}" in
  let parse = parse_ok (String.concat "" [ ".a{"; rules; "}" ]) in
  Alcotest.(check int) "nested rules: no warning" 0 (List.length parse.warnings);
  with_small_stack (fun () ->
      let pretty = Css.to_string parse.stylesheet in
      let optimised = Css.optimize parse.stylesheet in
      Alcotest.(check bool) "printed" true (pretty <> "");
      Alcotest.(check bool)
        "the innermost declaration is printed" true
        (has (minified parse) "color:red");
      Alcotest.(check int)
        "optimised" 1
        (List.length (Css.statements optimised)))

let nesting_past_the_limit () =
  (* One level past the limit, for each opener: the value is kept as the run of
     tokens it is, and prints back byte for byte. *)
  List.iter (check_value_kept "one past" ~depth:limit) value_openers;
  (* A [var()] in the run still references its property, and one nesting the
     value of a standard property keeps the declaration: CSS Variables 1 sec. 3
     leaves such a value to computed-value time, and Chrome keeps one nested 200
     deep ([deep_var_]). *)
  let css =
    String.concat ""
      [
        ".a{--y:red;--x:";
        nested limit ~opener:"(" ~inner:"var(--y)" ~closer:")";
        "}";
      ]
  in
  Alcotest.(check (list string))
    "the reference in the run" [ "y" ]
    (List.map
       (fun (Css.V v) -> v.Values.name)
       (Css.vars_of_stylesheet (parse_ok css).stylesheet));
  let css =
    String.concat ""
      [
        ".a{color:";
        nested (limit + 1) ~opener:"var(--a," ~inner:"red" ~closer:")";
        ";z-index:1}";
      ]
  in
  let parse = parse_ok css in
  Alcotest.(check int) "var(): no warning" 0 (List.length parse.warnings);
  Alcotest.(check string) "var(): printed back as written" css (minified parse);
  (* Without a rule's block around it, the value is one level shallower. A [}]
     inside a paren is the paren's to keep (sec. 5.5.9), so it neither ends the
     list nor the declaration early, and both declarations are kept. *)
  let value = nested (limit + 1) ~opener:"(" ~inner:"x}" ~closer:")" in
  let decls =
    Parser.list_of_declarations
      (Reader.of_string (String.concat "" [ "a:"; value; ";b:c" ]))
  in
  Alcotest.(check int) "both declarations" 2 (List.length decls.value);
  Alcotest.(check int) "no warning" 0 (List.length decls.warnings);
  (* The stream returns it as one component value, which prints back. *)
  let cv = Parser.next (Parser.of_string value) in
  Alcotest.(check string)
    "the stream's value prints back" value
    (Parser.string_of_components [ cv ])

let mismatched_closer_past_the_limit () =
  (* Sec. 5.5.9 ends a simple block only at the closer of its own opener, so the
     [)] in [[)]] is a token of the [[ ]] block, not the end of the paren around
     it, and sec. 7.2 bars that unmatched [)] from a <declaration-value>. The
     custom property goes at any depth, and Chrome drops it at 10 and at 129
     alike ([deep_mismatched_closer]); the empty [.a] it leaves prints nothing,
     and [.b] after it is read where it starts. *)
  List.iter
    (fun n ->
      let value = nested n ~opener:"(" ~inner:"[)]x" ~closer:")" in
      let parse =
        parse_ok (String.concat "" [ ".a{--x:"; value; "}.b{color:red}" ])
      in
      Alcotest.(check string)
        (String.concat "" [ "depth "; string_of_int n; ": only .b survives" ])
        ".b{color:red}" (minified parse))
    [ 10; limit + 1 ]

let rule_nested_past_the_limit () =
  (* A style rule nested one level past the limit loses what its block holds:
     the depth a reader goes into is the UA's to choose (w3c/csswg-drafts#2881),
     and a rule tree as deep as the input is what the limit keeps off the stack.
     Chrome keeps 1,000 levels ([deep_rules_999]), so this is cascade's limit
     and not the browser's. Only the construct past it goes: the declarations of
     the rule holding it, a sibling before and a rule after all stay. *)
  let css =
    String.concat ""
      [
        ".s{color:blue}.o{color:blue;";
        nested limit ~opener:"a{" ~inner:"color:green" ~closer:"}";
        "z-index:1}.b{color:red}";
      ]
  in
  let parse = parse_ok css in
  let out = minified parse in
  List.iter
    (fun kept ->
      Alcotest.(check bool)
        (String.concat "" [ "keeps "; kept ])
        true (has out kept))
    [ ".s{color:blue}"; ".o{color:blue;"; "z-index:1"; ".b{color:red}" ];
  Alcotest.(check bool)
    "drops the declaration past the limit" false (has out "color:green");
  check_one_too_deep "nested rule" ~construct:Error.Recovery.Declaration
    parse.warnings;
  (* The same through conditional group rules. *)
  let css =
    String.concat ""
      [
        ".s{color:blue}";
        nested limit ~opener:"@media x{" ~inner:".a{color:green}" ~closer:"}";
        ".b{color:red}";
      ]
  in
  let parse = parse_ok css in
  let out = minified parse in
  Alcotest.(check bool)
    "@media: keeps the sibling before" true (has out ".s{color:blue}");
  Alcotest.(check bool)
    "@media: keeps the rule after" true (has out ".b{color:red}");
  Alcotest.(check bool)
    "@media: drops the declaration past the limit" false (has out "color:green");
  Alcotest.(check int)
    "@media: one warning" 1
    (List.length (List.filter is_too_deep parse.warnings))

let selector_nested_past_its_limit () =
  (* A selector's reader recurses at each level and stops at 100 [:is()] deep,
     short of the parser's limit. Selectors 4 sec. 16.1 drops an invalid
     selector from a forgiving list, but one cascade does not read that far is
     not invalid: the rule fails with a warning rather than reading as the
     [:is()] that matches nothing. *)
  let rule n =
    String.concat ""
      [ nested n ~opener:":is(" ~inner:".a" ~closer:")"; "{color:red}" ]
  in
  let parse = parse_ok (rule 100) in
  Alcotest.(check string)
    "100 deep: kept as written" (rule 100) (minified parse);
  let parse = parse_ok (rule 101) in
  Alcotest.(check string) "101 deep: dropped" "" (minified parse);
  check_one_too_deep "101 deep" ~construct:Error.Recovery.Rule parse.warnings

let nesting_far_past_the_limit () =
  (* The depth that overflowed the stack. A value is kept whole and prints back
     as written; the run past the limit is walked without recursing, so nothing
     deeper reaches the printer, the optimiser or the typed readers. *)
  let depth = 50_000 in
  with_small_stack (fun () ->
      List.iter
        (fun (opener, closer) ->
          let value = nested depth ~opener ~inner:"x" ~closer in
          let css = String.concat "" [ ".a{--x:"; value; "}" ] in
          let parse = parse_ok css in
          Alcotest.(check int)
            (String.concat "" [ opener; ": no warning" ])
            0
            (List.length parse.warnings);
          Alcotest.(check bool)
            (String.concat "" [ opener; ": printed back as written" ])
            true
            (String.equal css (minified parse));
          ignore (Css.to_string parse.stylesheet);
          ignore (Css.optimize parse.stylesheet))
        value_openers;
      (* A rule, a group rule and a selector nest a declaration that the depth
         cut drops, with one warning, whatever is under it; the empty rules left
         around it print nothing. The selector's grammar recurses at each level,
         so its reader stops at its own, smaller limit. *)
      List.iter
        (fun (what, css) ->
          let parse = parse_ok css in
          Alcotest.(check bool)
            (String.concat "" [ what; ": the declaration is cut" ])
            false
            (has (minified parse) "color:red");
          Alcotest.(check int)
            (String.concat "" [ what; ": one warning" ])
            1
            (List.length (List.filter is_too_deep parse.warnings));
          ignore (Css.to_string parse.stylesheet))
        [
          (".a{", nested depth ~opener:".a{" ~inner:"color:red" ~closer:"}");
          ( "@media x{",
            nested depth ~opener:"@media x{" ~inner:".a{color:red}" ~closer:"}"
          );
          ( ":is(",
            String.concat ""
              [
                nested depth ~opener:":is(" ~inner:".a" ~closer:")";
                "{color:red}";
              ] );
        ];
      let listed =
        Parser.list_of_component_values
          (Reader.of_string (nested depth ~opener:"(" ~inner:"" ~closer:")"))
      in
      Alcotest.(check int) "the list keeps it" 1 (List.length listed.value);
      Alcotest.(check int) "with no warning" 0 (List.length listed.warnings))

let suite =
  ( "parser_depth",
    [
      Alcotest.test_case "nesting at the limit" `Quick nesting_at_the_limit;
      Alcotest.test_case "nesting past the limit" `Quick nesting_past_the_limit;
      Alcotest.test_case "mismatched closer past the limit" `Quick
        mismatched_closer_past_the_limit;
      Alcotest.test_case "rule nested past the limit" `Quick
        rule_nested_past_the_limit;
      Alcotest.test_case "selector nested past its limit" `Quick
        selector_nested_past_its_limit;
      Alcotest.test_case "nesting far past the limit" `Quick
        nesting_far_past_the_limit;
    ] )
