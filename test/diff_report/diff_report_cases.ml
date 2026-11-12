(* ===== The neutral arm: transforms README says canonical must see past
   ===== *)

(* Each pair is one stylesheet written two ways. README: "canonical reports a
   difference only when some element would compute a different value", and the
   moves below change no computed value - the same declarations, under the same
   selectors, in an order or a grouping the cascade cannot tell apart. A
   difference reported here is an over-report by that sentence. *)
let neutral_pairs =
  [
    ("split-rule", ".a{color:red;width:1px}", ".a{color:red}.a{width:1px}");
    ("join-rule", ".a{color:red}.a{width:1px}", ".a{color:red;width:1px}");
    ("selector-list-split", ".a,.b{color:red}", ".a{color:red}.b{color:red}");
    ("selector-list-join", ".a{color:red}.b{color:red}", ".a,.b{color:red}");
    ("disjoint-decl-swap", ".a{color:red;width:1px}", ".a{width:1px;color:red}");
    ( "disjoint-rule-swap",
      ".a{color:red}.b{width:1px}",
      ".b{width:1px}.a{color:red}" );
    ("empty-rule-added", ".a{color:red}", ".a{color:red}.b{}");
    ("colour-respelling", ".a{color:#ff0000}", ".a{color:red}");
    ("transparent-respelling", ".a{color:transparent}", ".a{color:#0000}");
    ("factored-out", ".a{color:red}.b{color:red}", ".a,.b{color:red}");
    ( "hoisted-declaration",
      ".a{color:red;width:1px}.b{color:red}",
      ".a,.b{color:red}.a{width:1px}" );
    ("whitespace", ".a{color:red}", ".a {\n  color: red;\n}\n");
    ("comment", ".a{color:red}", "/* c */.a{color:red}");
    ("zero-unit", ".a{margin-top:0px}", ".a{margin-top:0}");
  ]

(* ===== One-site changes the model generator cannot write ===== *)

(* Nesting, [@keyframes], [@font-face], [@page], [@property] and [@layer]: the
   at-rules README says the comparison detects. Each pair changes one thing, and
   [names] is what a report has to name for it - read off the two files, not off
   a run. [holds] is the property that changed, when one did. *)
type text_case = {
  case : string;
  before : string;
  after : string;
  names : string list;
  under : string list;
  holds : (string * string * string * string) option;
}

let text_cases =
  [
    {
      case = "keyframe-value";
      before = "@keyframes fade{0%{opacity:0}100%{opacity:1}}";
      after = "@keyframes fade{0%{opacity:0.7}100%{opacity:1}}";
      names = [ "0%" ];
      under = [ "@keyframes fade" ];
      (* The report spells a value the way cascade prints it, which for a
         fractional number is CSS Values 4 sec. 6.7.2 without the leading zero.
         No raw token text is kept anywhere in the AST, so the digits the file
         holds are not cascade's to echo; [.7] and [0.7] are one value. *)
      holds = Some ("0%", "opacity", "0", ".7");
    };
    {
      case = "keyframe-added";
      before = "@keyframes fade{0%{opacity:0}}";
      after = "@keyframes fade{0%{opacity:0}50%{opacity:0.5}}";
      names = [ "50%" ];
      under = [ "@keyframes fade" ];
      holds = None;
    };
    {
      case = "nested-value";
      before = ".a{color:red;&:hover{color:blue}}";
      after = ".a{color:red;&:hover{color:lime}}";
      names = [ "&:hover" ];
      under = [];
      holds = Some ("&:hover", "color", "blue", "lime");
    };
    {
      case = "nested-added";
      before = ".a{color:red}";
      after = ".a{color:red;&:hover{color:blue}}";
      names = [ "&:hover" ];
      under = [];
      holds = None;
    };
    {
      case = "font-face-src";
      before = "@font-face{font-family:x;src:url(a.woff2)}";
      after = "@font-face{font-family:x;src:url(b.woff2)}";
      names = [];
      under = [ "@font-face" ];
      holds = None;
    };
    {
      case = "page-margin";
      before = "@page{margin:1cm}";
      after = "@page{margin:2cm}";
      names = [];
      under = [ "@page" ];
      holds = None;
    };
    {
      case = "property-initial";
      before =
        "@property --x{syntax:\"<length>\";inherits:false;initial-value:0px}";
      after =
        "@property --x{syntax:\"<length>\";inherits:false;initial-value:4px}";
      names = [];
      under = [ "@property --x" ];
      holds = None;
    };
    {
      case = "media-condition";
      before = "@media (min-width:400px){.a{color:red}}";
      after = "@media (min-width:500px){.a{color:red}}";
      names = [ ".a" ];
      under = [ "@media (min-width: 400px)"; "@media (min-width: 500px)" ];
      holds = None;
    };
    {
      case = "layer-value";
      before = "@layer base{.a{color:red}}";
      after = "@layer base{.a{color:blue}}";
      names = [ ".a" ];
      under = [ "@layer base" ];
      holds = Some (".a", "color", "red", "blue");
    };
    {
      case = "supports-value";
      before = "@supports (display:grid){.a{color:red}}";
      after = "@supports (display:grid){.a{color:blue}}";
      names = [ ".a" ];
      under = [ "@supports (display: grid)" ];
      holds = Some (".a", "color", "red", "blue");
    };
    {
      case = "container-value";
      before = "@container (min-width:400px){.a{color:red}}";
      after = "@container (min-width:400px){.a{color:blue}}";
      names = [ ".a" ];
      under = [ "@container (min-width: 400px)" ];
      holds = Some (".a", "color", "red", "blue");
    };
    {
      case = "important-added";
      before = ".a{color:red}";
      after = ".a{color:red!important}";
      names = [ ".a" ];
      under = [];
      holds = Some (".a", "color", "red", "red !important");
    };
    {
      case = "fallback-chain-second";
      before = ".a{color:red;color:blue}";
      after = ".a{color:red;color:lime}";
      names = [ ".a" ];
      under = [];
      holds = Some (".a", "color", "blue", "lime");
    };
  ]

(* Pairs that differ, so a run that reports nothing at all is visibly blind. *)
let differing_pairs =
  [
    ("value", ".a{color:red}", ".a{color:blue}");
    ("rule-gone", ".a{color:red}.b{color:red}", ".a{color:red}");
    ("important", ".a{color:red}", ".a{color:red!important}");
    ( "overlapping-swap",
      ".a{color:red}.a{color:blue}",
      ".a{color:blue}.a{color:red}" );
  ]
