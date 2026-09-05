(** Tests for CSS Syntax tokenizer algorithms. *)

open Cascade

let tokens_of ?enforce_spec ?unicode_ranges css =
  let lexer = Lexer.of_string ?enforce_spec ?unicode_ranges css in
  let rec loop acc =
    let tok = Lexer.next lexer in
    match tok.Token.kind with
    | Token.Eof -> List.rev acc
    | _ -> loop (tok.Token.kind :: acc)
  in
  loop []

let pp_tokens kinds =
  String.concat " " (List.map (Css.Pp.to_string Token.pp_kind) kinds)

let check ?enforce_spec ?unicode_ranges input expected_summary =
  let got = pp_tokens (tokens_of ?enforce_spec ?unicode_ranges input) in
  Alcotest.(check string) (Fmt.str "tokenize %S" input) expected_summary got

(* CSS Syntax 3 sec. 4.3.14: the tokenizer produces a unicode-range only for the
   value of a unicode-range descriptor, so these vectors ask for it. *)
let check_range input expected_summary =
  check ~unicode_ranges:true input expected_summary

let check_first_hash input expected_value expected_flag =
  match tokens_of input with
  | [ Token.Hash { value; hash_flag } ] ->
      Alcotest.(check string)
        (Fmt.str "hash value for %S" input)
        expected_value value;
      Alcotest.(check bool)
        (Fmt.str "hash flag for %S" input)
        true
        (Token.equal_hash_flag hash_flag expected_flag)
  | kinds ->
      Alcotest.failf "tokenize %S: expected one hash token, got %s" input
        (pp_tokens kinds)

let check_first_number_flag input expected_repr expected_flag =
  match tokens_of input with
  | [ Token.Number_tok { repr; number_flag; _ } ] ->
      Alcotest.(check string)
        (Fmt.str "number repr for %S" input)
        expected_repr repr;
      Alcotest.(check bool)
        (Fmt.str "number flag for %S" input)
        true
        (Token.equal_number_flag number_flag expected_flag)
  | kinds ->
      Alcotest.failf "tokenize %S: expected one number token, got %s" input
        (pp_tokens kinds)

let spec_preprocessing () =
  (* CSS Syntax Level 3 section 3.3: preprocess the input stream before
     tokenization. *)
  check "\xEF\xBB\xBFfoo" "<ident foo>";
  check "a\rb" "<ident a> <ws> <ident b>";
  check "a\r\nb" "<ident a> <ws> <ident b>";
  check "a\nb" "<ident a> <ws> <ident b>";
  check "a\tb" "<ident a> <ws> <ident b>";
  check "a\012b" "<ident a> <ws> <ident b>";
  check "a\x00b" ("<ident a" ^ "\xEF\xBF\xBD" ^ "b>")

let spec_token_railroad_diagrams () =
  (* CSS Syntax Level 3 section 4.1 token railroad diagrams: one compact pass
     over the token categories. Unicode-range has its own focused vectors below
     because its syntax has a branchy range grammar. *)
  check
    "foo calc( @media #id \"str\" url(a.png) url(a b) ? 1 2% 3px \t <!-- --> : \
     ; , [ ] ( ) { }"
    "<ident foo> <ws> <function calc(> <ws> <@media> <ws> <#id> <ws> <string \
     str> <ws> <url a.png> <ws> <bad-url> <ws> <delim '?'> <ws> <number 1> \
     <ws> <percentage 2%> <ws> <dimension 3px> <ws> <CDO> <ws> <CDC> <ws> <:> \
     <ws> <;> <ws> <,> <ws> <[> <ws> <]> <ws> <(> <ws> <)> <ws> <{> <ws> <}>"

let spec_unicode_range_tokens () =
  (* CSS Syntax Level 3 sections 4.1, 4.3.11, and 4.3.14. *)
  check_range "U+26" "<unicode-range U+26>";
  check_range "u+0-7f" "<unicode-range U+0-7F>";
  check_range "U+0025-00FF" "<unicode-range U+25-FF>";
  check_range "U+4??" "<unicode-range U+400-4FF>";
  check_range "U+10????" "<unicode-range U+100000-10FFFF>";
  check_range "U+??????" "<unicode-range U+0-FFFFFF>";
  check_range "U+1234567" "<unicode-range U+123456> <number 7>";
  check_range "U+12??-f" "<unicode-range U+1200-12FF> <ident -f>";
  check_range "u+???????" "<unicode-range U+0-FFFFFF> <delim '?'>";
  check "u+-1" "<ident u> <delim '+'> <number -1>";
  check "u+" "<ident u> <delim '+'>";
  check "u+g" "<ident u> <delim '+'> <ident g>"

let spec_definitions () =
  (* CSS Syntax Level 3 section 4.2 definitions that affect tokenizer branch
     decisions before the section 4.3 algorithms run. *)
  check "url(a\x07b)" "<bad-url>";
  check "a\u{00B7}b" "<ident a\u{00B7}b>";
  (* U+200B is not in the section 4.2 ranges, so under [~enforce_spec:true] it
     falls through to a single delim token with that code point. Reading
     defaults to any code point >= U+0080, where it is an ident. *)
  check ~enforce_spec:true "\u{200B}" "<delim '\u{200B}'>";
  check "\u{200B}" "<ident \u{200B}>"

let spec_hash_flags () =
  (* CSS Syntax Level 3 section 4.3.1: hash tokens remember whether the
     following code points would start an ident sequence. *)
  check_first_hash "#abc" "abc" Token.Id;
  check_first_hash "#123" "123" Token.Unrestricted;
  check_first_hash "#\\31 a" "1a" Token.Id

let spec_delim_fallbacks () =
  (* CSS Syntax Level 3 section 4.3.1 fallback cases for punctuation that does
     not start a more specific token. *)
  check "+ - . @ # \\"
    "<delim '+'> <ws> <delim '-'> <ws> <delim '.'> <ws> <delim '@'> <ws> \
     <delim '#'> <ws> <delim '\\'>";
  check "\\\n" "<delim '\\'> <ws>"

let idents () =
  check "foo" "<ident foo>";
  check "foo-bar" "<ident foo-bar>";
  check "--custom" "<ident --custom>";
  check "-foo" "<ident -foo>"

let functions () =
  check "rgb(1,2,3)"
    "<function rgb(> <number 1> <,> <number 2> <,> <number 3> <)>"

let at_keyword () =
  check "@media" "<@media>";
  check "@-webkit-foo" "<@-webkit-foo>"

let hashes () =
  check "#abc" "<#abc>";
  check "#123" "<#123>"

let strings () =
  check {|"hello"|} "<string hello>";
  check "'hello'" "<string hello>";
  check {|"un\"escaped"|} {|<string un"escaped>|}

let delims () = check "* + ~" "<delim '*'> <ws> <delim '+'> <ws> <delim '~'>"

let braces () =
  check "{}" "<{> <}>";
  check "()" "<(> <)>";
  check "[]" "<[> <]>"

let numbers () =
  check "1 2.5 -3 1e2"
    "<number 1> <ws> <number 2.5> <ws> <number -3> <ws> <number 1e2>"

let percentage () = check "50%" "<percentage 50%>"

let dimension () =
  check "10px 1.5rem" "<dimension 10px> <ws> <dimension 1.5rem>"

let comments_skipped () = check "/* hi */ foo" "<ws> <ident foo>"
let cdo_cdc () = check "<!-- foo -->" "<CDO> <ws> <ident foo> <ws> <CDC>"

let spec_escaping () =
  (* CSS Syntax Level 3 section 2.1: hex escapes are one to six hex digits
     followed by optional whitespace, so both forms spell the ident "&B". *)
  check {|\26 B \000026B|} "<ident &B> <ws> <ident &B>";
  check {|"B\26 W"|} "<string B&W>"

let spec_consume_token () =
  (* CSS Syntax Level 3 section 4.3.1: punctuation and malformed escapes map
     directly to the token taxonomy from section 4. *)
  check "():;,.[]" "<(> <)> <:> <;> <,> <delim '.'> <[> <]>";
  check {|@ # \|} {|<delim '@'> <ws> <delim '#'> <ws> <delim '\'>|}

let spec_comments () =
  (* CSS Syntax Level 3 section 4.3.2: comments are consumed before the next
     token and do not synthesize whitespace. *)
  check "a/*x*/b" "<ident a> <ident b>";
  check "a/*x*/ b" "<ident a> <ws> <ident b>";
  check "/* one *//* two */a" "<ident a>";
  check "a/*x" "<ident a>"

let spec_numeric_tokens () =
  (* CSS Syntax Level 3 sections 4.3.3, 4.3.10, and 4.3.13. *)
  check "+10 -2.5 .5 1e-2"
    "<number +10> <ws> <number -2.5> <ws> <number .5> <ws> <number 1e-2>";
  check "+.5 -.5" "<number +.5> <ws> <number -.5>";
  check "1e+ 1e- 1e"
    "<dimension 1e> <delim '+'> <ws> <dimension 1e-> <ws> <dimension 1e>";
  check "+. -. .e1"
    "<delim '+'> <delim '.'> <ws> <delim '-'> <delim '.'> <ws> <delim '.'> \
     <ident e1>";
  check "10px 5% 1e3ms"
    "<dimension 10px> <ws> <percentage 5%> <ws> <dimension 1e3ms>"

let spec_number_flags () =
  (* CSS Syntax Level 3 section 4.3.13 returns a number type of "integer" or
     "number" in addition to the numeric value. *)
  check_first_number_flag "10" "10" Token.Integer;
  check_first_number_flag "+10" "+10" Token.Integer;
  check_first_number_flag "10.0" "10.0" Token.Number;
  check_first_number_flag "1e2" "1e2" Token.Number

let spec_ident_like_tokens () =
  (* CSS Syntax Level 3 sections 4.3.4, 4.3.9, and 4.3.12. *)
  check "-- <!-- -x \\26 B"
    "<ident --> <ws> <CDO> <ws> <ident -x> <ws> <ident &B>";
  check "\\! -\\!" "<ident !> <ws> <ident -!>";
  check "calc(1) url(\"a.png\")"
    "<function calc(> <number 1> <)> <ws> <function url(> <string a.png> <)>"

let spec_string_tokens () =
  (* CSS Syntax Level 3 sections 4.3.5, 4.3.7, and 4.3.8. *)
  check "\"a\\\nb\"" "<string ab>";
  check "\"a\\\r\nb\"" "<string ab>";
  check {|"a\26 b"|} "<string a&b>";
  check {|"a\\b"|} {|<string a\b>|};
  check {|'a"b'|} {|<string a"b>|};
  check "\"abc" "<string abc>";
  check "\"abc\\" "<string abc>";
  check "\"oops\n" "<bad-string> <ws>"

let url () =
  check "url(a.png)" "<url a.png>";
  check {|url("a.png")|} {|<function url(> <string a.png> <)>|}

let spec_url_tokens () =
  (* CSS Syntax Level 3 sections 4.3.6 and 4.3.15. *)
  check "url(  a.png  )" "<url a.png>";
  check "url(\t a.png\n )" "<url a.png>";
  check "url(a\\ b.png)" "<url a b.png>";
  check "url(a\x07b)" "<bad-url>";
  check "url(a\\\nb)" "<bad-url>";
  check "url(a " "<url a>";
  check "url(a b\\) c) next" "<bad-url> <ws> <ident next>";
  check "url(a b) foo" "<bad-url> <ws> <ident foo>"

let spec_escaped_code_points () =
  (* CSS Syntax Level 3 section 4.3.7: null, surrogate, and out-of-range escapes
     are replaced with U+FFFD. *)
  check "\\0 " "<ident \u{FFFD}>";
  check "\\D800 " "<ident \u{FFFD}>";
  check "\\110000 " "<ident \u{FFFD}>"

let spec_security_comment_confusion_regressions () =
  (* CSS Syntax Level 3 section 11: regression vectors for parser-confusion
     issues like CVE-2023-44270, where a CSS parser discrepancy around CR and
     comments caused content written inside a comment to surface as active CSS
     nodes downstream. *)
  check "@font-face{ font:(\r/*); color:red */}"
    "<@font-face> <{> <ws> <ident font> <:> <(> <ws> <}>";
  check "safe/* .evil { color: red } */end" "<ident safe> <ident end>";
  check "safe/* unterminated .evil { color: red }" "<ident safe>"

let spec12_tokenization_checklist () =
  (* CSS Syntax Level 3 section 12 is non-normative. These are regression
     vectors for the tokenization behaviors it calls out as changed from older
     syntax definitions. *)
  check "a\x00b \\0 \\D800 \\110000 "
    ("<ident a" ^ "\xEF\xBF\xBD" ^ "b> <ws> <ident " ^ "\xEF\xBF\xBD"
   ^ "\xEF\xBF\xBD" ^ "\xEF\xBF\xBD" ^ ">");
  check "-- --x -->" "<ident --> <ws> <ident --x> <ws> <CDC>";
  check "url(\"a.png\") url(a.png) url(foo\"bar[still ignored]) next"
    "<function url(> <string a.png> <)> <ws> <url a.png> <ws> <bad-url> <ws> \
     <ident next>";
  check "\"eof string" "<string eof string>";
  check "url(eof-url" "<url eof-url>";
  check "+10 -2.5 1e2 1e+2 1e-2 4e5px"
    "<number +10> <ws> <number -2.5> <ws> <number 1e2> <ws> <number 1e+2> <ws> \
     <number 1e-2> <ws> <dimension 4e5px>";
  check_range "U+26 u+a" "<unicode-range U+26> <ws> <unicode-range U+A>"

let spec_token_boundary_edges () =
  (* CSS Syntax Level 3 section 9 serialization constraints are driven by
     tokenizer ambiguities. These vectors pin down pairs that must remain
     distinct after any serializer inserts a boundary. *)
  check "a b" "<ident a> <ws> <ident b>";
  check "a\\ b" "<ident a b>";
  check "url(a)url(b)" "<url a> <function url(> <ident b> <)>";
  check "@media@supports" "<@media> <@supports>";
  check "#id.class" "<#id> <delim '.'> <ident class>";
  check "1 2" "<number 1> <ws> <number 2>";
  check "1/**/2" "<number 1> <number 2>";
  check "1px/**/solid" "<dimension 1px> <ident solid>";
  check "-->--" "<CDC> <ident -->";
  check "<!---->" "<CDO> <CDC>"

let spec_escape_boundary_edges () =
  (* Escapes consume at most six hex digits and one following whitespace. *)
  check "\\31 0" "<ident 10>";
  check "\\0000310" "<ident 10>";
  check "\\000031 0" "<ident 10>";
  check "\\26#id" "<ident &#id>";
  check "\\26 #id" "<ident &> <#id>";
  check "\"\\26#\"" "<string &#>";
  check "\"\\26 #\"" "<string &#>"

let spec_wpt_numeric_token_edges () =
  (* WPT css-syntax decimal-points-in-numbers vectors. *)
  check ".5 -.5 +.5 .e1"
    "<number .5> <ws> <number -.5> <ws> <number +.5> <ws> <delim '.'> <ident \
     e1>"

let spec_wpt_cdc_escape_edges () =
  (* WPT cdc-vs-ident-tokens and escaped-eof vectors. *)
  check "--> -->a --a" "<CDC> <ws> <CDC> <ident a> <ws> <ident --a>";
  check "\\\n\\\r\n\\\r\\\012"
    "<delim '\\'> <ws> <delim '\\'> <ws> <delim '\\'> <ws> <delim '\\'> <ws>"

let spec_wpt_url_unicode_edges () =
  (* WPT url-whitespace-consumption and inclusive-ranges vectors. *)
  check "url(  \tfoo\\ bar\n )" "<url foo bar>";
  check "url(foo)/**/url(bar)" "<url foo> <url bar>";
  check_range "U+000000-10FFFF U+10FFFF U+110000"
    "<unicode-range U+0-10FFFF> <ws> <unicode-range U+10FFFF> <ws> \
     <unicode-range U+110000>";
  check "a<!--b-->c" "<ident a> <CDO> <ident b--> <delim '>'> <ident c>"

(* Per 4.3.5, a newline inside a string produces a <bad-string> token; the
   newline is not consumed and becomes a subsequent <whitespace>. *)
let bad_string () = check "\"oops\n" "<bad-string> <ws>"
let bad_url () = check "url(a b)" "<bad-url>"

let suite =
  ( "lexer",
    [
      Alcotest.test_case "spec section 3.3 preprocessing" `Quick
        spec_preprocessing;
      Alcotest.test_case "spec section 4.1 token railroad diagrams" `Quick
        spec_token_railroad_diagrams;
      Alcotest.test_case "spec section 4.1 unicode-range tokens" `Quick
        spec_unicode_range_tokens;
      Alcotest.test_case "spec section 4.2 definitions" `Quick spec_definitions;
      Alcotest.test_case "spec section 4.3.1 hash flags" `Quick spec_hash_flags;
      Alcotest.test_case "spec section 4.3.1 delimiter fallbacks" `Quick
        spec_delim_fallbacks;
      Alcotest.test_case "idents" `Quick idents;
      Alcotest.test_case "functions" `Quick functions;
      Alcotest.test_case "at-keyword" `Quick at_keyword;
      Alcotest.test_case "hashes" `Quick hashes;
      Alcotest.test_case "strings" `Quick strings;
      Alcotest.test_case "delims" `Quick delims;
      Alcotest.test_case "braces" `Quick braces;
      Alcotest.test_case "numbers" `Quick numbers;
      Alcotest.test_case "percentage" `Quick percentage;
      Alcotest.test_case "dimension" `Quick dimension;
      Alcotest.test_case "comments skipped" `Quick comments_skipped;
      Alcotest.test_case "CDO/CDC" `Quick cdo_cdc;
      Alcotest.test_case "spec section 2.1 escaping" `Quick spec_escaping;
      Alcotest.test_case "spec section 4.3.1 consume token" `Quick
        spec_consume_token;
      Alcotest.test_case "spec section 4.3.2 comments" `Quick spec_comments;
      Alcotest.test_case "spec section 4.3.3 numeric tokens" `Quick
        spec_numeric_tokens;
      Alcotest.test_case "spec section 4.3.13 number flags" `Quick
        spec_number_flags;
      Alcotest.test_case "spec section 4.3.4 ident-like tokens" `Quick
        spec_ident_like_tokens;
      Alcotest.test_case "spec section 4.3.5 string tokens" `Quick
        spec_string_tokens;
      Alcotest.test_case "url" `Quick url;
      Alcotest.test_case "spec section 4.3.6 url tokens" `Quick spec_url_tokens;
      Alcotest.test_case "spec section 4.3.7 escaped code points" `Quick
        spec_escaped_code_points;
      Alcotest.test_case
        "spec section 11 comment-confusion security regressions" `Quick
        spec_security_comment_confusion_regressions;
      Alcotest.test_case "spec section 12 tokenization change checklist" `Quick
        spec12_tokenization_checklist;
      Alcotest.test_case "spec token boundary edges" `Quick
        spec_token_boundary_edges;
      Alcotest.test_case "spec escape boundary edges" `Quick
        spec_escape_boundary_edges;
      Alcotest.test_case "spec WPT numeric token edges" `Quick
        spec_wpt_numeric_token_edges;
      Alcotest.test_case "spec WPT CDC and escape edges" `Quick
        spec_wpt_cdc_escape_edges;
      Alcotest.test_case "spec WPT URL and unicode-range edges" `Quick
        spec_wpt_url_unicode_edges;
      Alcotest.test_case "bad string" `Quick bad_string;
      Alcotest.test_case "bad url" `Quick bad_url;
    ] )
