(** Declaration tests: forms no specification grants that some browser in B
    parses and another drops, which docs/invariants.md keeps as written. *)

open Alcotest
open Cascade
open Css.Declaration
open Css_test_helpers
open Declaration_helpers

(* Forms no specification grants that a browser in B parses, kept as written
   like a trailing contain-intrinsic-size [auto].

   CSS Fonts 4 sec. 2.1.1 gives an unquoted family a <custom-ident>+, and CSS
   Values 4 sec. 4.2 excludes a CSS-wide keyword and [default] from
   <custom-ident>. Chromium 153 and WebKit 26.5 still read either inside a
   family of several words, and a generic keyword that does not head one: they
   take [inherit foo], [default foo] and [ui-serif Foo], and drop [serif Foo],
   [foo, inherit] and [x "y"].

   CSS Backgrounds 3 sec. 4.5 builds [border] from a [||] with no comma.
   Chromium reads a comma between single components of [border], [border-block]
   and [border-inline] as the space between them, a trailing one included, and
   WebKit drops it; both drop [red solid, none], an empty entry and a comma in
   [border-left]. *)
let engine_splits_keep_as_written () =
  List.iter
    (fun (input, expected) -> check_declaration ~expected input)
    [
      ("font-family: inherit foo", "font-family:inherit foo");
      ("font-family: Foo inherit", "font-family:Foo inherit");
      ("font-family: default foo", "font-family:default foo");
      ("font-family: ui-serif Foo", "font-family:ui-serif Foo");
      ("font-family: serif,inherit x", "font-family:serif,inherit x");
      ("border: red,none", "border:red,none");
      ("border: none,red,1px", "border:none,red,1px");
      ("border-block: 1px,red", "border-block:1px,red");
      ("border-inline: red,solid", "border-inline:red,solid");
      ("border: 10px,", "border:10px,");
      ("border: red,none,", "border:red,none,");
    ];
  (* Dropped: refused by the reader, or read as a declaration every
     serialisation removes. *)
  List.iter
    (fun decl ->
      Alcotest.(check string)
        decl ""
        (Css.minify (Css.stylesheet (String.concat "" [ ".a{"; decl; "}" ]))))
    [
      "font-family: serif Foo";
      "font-family: sans-serif x";
      "font-family: foo, inherit";
      "font-family: foo, default";
      "font-family: x \"y\"";
      "font-family: x 1";
      "border: none, none";
      "border: red, blue";
      "border: red solid, none";
      "border: 1px solid red, none";
      "border-left: red, none";
      "border: ,10px";
      "border: red,,none";
    ];
  (* WebKit paints the declaration before each, so it is no dead fallback. *)
  List.iter
    (fun (css, expected) ->
      Alcotest.(check string) css expected (Css.minify (Css.stylesheet css)))
    [
      ( ".a{border:1px solid blue;border:red,none}",
        ".a{border:1px solid#00f;border:red,none}" );
      ( ".a{font-family:serif;font-family:inherit foo}",
        ".a{font-family:serif;font-family:inherit foo}" );
    ]

(* More forms no specification grants that Chromium 153 reads and WebKit 26.5
   drops, kept as written: [resize: auto] (CSS UI 4 sec. 4.1 has none),
   [text-orientation: sideways-right] (CSS Writing Modes 4 sec. 5.1 has
   sideways), [alignment-baseline: auto] (CSS Inline 3 sec. 4.2.2 has none), a
   trailing comma after a column-rule list (CSS Gaps 1 sec. 4.4 has no empty
   entry), and [historical-forms] repeated (CSS Fonts 4 takes it once). Each
   drops in both once anything else is wrong. *)
let engine_splits_keep_as_written_2 () =
  List.iter
    (fun (input, expected) -> check_declaration ~expected input)
    [
      ("resize: auto", "resize:auto");
      ("text-orientation: sideways-right", "text-orientation:sideways-right");
      ("alignment-baseline: auto", "alignment-baseline:auto");
      ("column-rule: red,", "column-rule:red,");
      ("column-rule: 1px solid red,", "column-rule:1px solid red,");
      ("column-rule-width: 1px,", "column-rule-width:1px,");
      ("column-rule-style: solid,", "column-rule-style:solid,");
      ("column-rule-color: red,", "column-rule-color:red,");
      ( "font-variant-alternates: historical-forms historical-forms",
        "font-variant-alternates:historical-forms historical-forms" );
    ];
  List.iter
    (fun decl -> neg_cursor read_declaration decl)
    [
      "resize: auto auto";
      "text-orientation: sideways-right upright";
      "column-rule: red,,";
      "column-rule-width: ,1px";
      "font-variant-alternates: historical-forms, historical-forms";
      "font-variant-alternates: stylistic(a) stylistic(a)";
    ];
  (* WebKit paints the declaration before, so it is no dead fallback. *)
  Alcotest.(check string)
    "a fallback before resize: auto" ".a{resize:both;resize:auto}"
    (Css.minify (Css.stylesheet ".a{resize:both;resize:auto}"))

let suite =
  ( "declaration_engine_splits",
    [
      test_case "engine splits keep as written" `Quick
        engine_splits_keep_as_written;
      test_case "engine splits keep as written, second set" `Quick
        engine_splits_keep_as_written_2;
    ] )
