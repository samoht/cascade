type token_row = { branch : string; input : string; expected : string list }
type parser_row = { branch : string; input : string }

let token branch input expected = { branch; input; expected }
let parser branch input = { branch; input }

let token_rows =
  [
    (* CSS Syntax 3 sec. 4.3.1 leaves "unicode ranges allowed" unset outside the
       value of a unicode-range descriptor (sec. 4.3.14), so this is what the
       tokenizer gives everywhere else. *)
    token "unicode range wildcard" "U+4??"
      [ "<ident U>"; "<number +4>"; "<delim '?'>"; "<delim '?'>" ];
    token "bad string newline" "\"oops\n" [ "<bad-string>"; "<ws>" ];
    token "bad url whitespace" "url(a b) foo"
      [ "<bad-url>"; "<ws>"; "<ident foo>" ];
    token "CDC and ident split" "--> -->a --a"
      [ "<CDC>"; "<ws>"; "<CDC>"; "<ident a>"; "<ws>"; "<ident --a>" ];
    token "number signs and delim" ".5 -.5 +.5 .e1"
      [
        "<number .5>";
        "<ws>";
        "<number -.5>";
        "<ws>";
        "<number +.5>";
        "<ws>";
        "<delim '.'>";
        "<ident e1>";
      ];
    token "escape followed by hash" "\\26 #id" [ "<ident &>"; "<#id>" ];
    token "comment skipping" "a/*x*/ b" [ "<ident a>"; "<ws>"; "<ident b>" ];
    token "escaped replacement code point" "\\110000 " [ "<ident \u{FFFD}>" ];
    token "at keyword hash dimension percentage" "@media #123 #abc 10px 50%"
      [
        "<@media>";
        "<ws>";
        "<#123>";
        "<ws>";
        "<#abc>";
        "<ws>";
        "<dimension 10px>";
        "<ws>";
        "<percentage 50%>";
      ];
    token "bracket punctuation" "{}[]():;,"
      [ "<{>"; "<}>"; "<[>"; "<]>"; "<(>"; "<)>"; "<:>"; "<;>"; "<,>" ];
    token "CDO CDC pair" "<!-- -->" [ "<CDO>"; "<ws>"; "<CDC>" ];
    token "function token boundary" "calc(1px)"
      [ "<function calc(>"; "<dimension 1px>"; "<)>" ];
    token "plain url token" "url(example.png)" [ "<url example.png>" ];
  ]

let parser_rows =
  [
    parser "unclosed function and block" "calc(1px + [2em";
    parser "extra closing braces" "a}}";
    parser "top-level comma groups" "a,(b,c),f(x,y),d";
    parser "bad url with quote" "url(foo\"bar) next";
    parser "nested unclosed blocks" "[a {b (c";
    parser "comment elision" "a/* ignored { color: red } */b";
    parser "comment token boundary" "foo/**/bar";
    parser "unclosed at-rule block" "@media screen { .a { color: red }";
    parser "semicolon separates declarations" "color: red; background: blue";
    parser "custom property block value" "--tokens: { color: red; }";
    parser "custom property rule ambiguity" "--x: {}; .next { color: red }";
    parser "unicode range selector ambiguity" "u+a { color: red }";
    parser "escaped eof in identifier" ".foo\\";
    parser "escaped newline continuation" ".foo\\\nbar";
    parser "nested var fallback blocks" "var(--x, { color: red; [a, b] })";
    parser "function comma nesting" "color-mix(in oklab, red, rgb(0 0 0 / .5))";
    parser "declaration important tokenization" "color: red ! important";
    parser "bad url recovery boundary" "url(foo bar) color(red)";
    parser "at-rule with declaration prelude" "@supports (display: grid)";
    parser "nested conditional functions"
      "@when media(width >= 40em) and supports(display: grid)";
  ]

let mutate_parser_input row salt =
  match salt mod 5 with
  | 0 -> row.input ^ "}"
  | 1 -> "(" ^ row.input
  | 2 -> row.input ^ ",,"
  | 3 -> row.input ^ "/*unterminated"
  | _ -> row.input ^ "\\\n"
