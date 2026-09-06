(* Compare the element set {!Cascade.Resolve} says a selector matches against
   the set [document.querySelectorAll] returns for the same selector over the
   same document. The browser decides; nothing here derives an expectation from
   the library under test.

   Three answers are told apart, because they carry different weight. Cascade
   matching a different set from the browser is wrong. Cascade refusing to read
   a selector the browser reads leaves every verdict over a sheet holding it
   unproven. Cascade reading a selector and declining to decide it
   ([Unsupported]) is a documented limit - but {!Cascade.Resolve.matches} folds
   that answer in with [No_match], so a direct [resolve] caller cannot tell the
   two apart, and the run counts how often that silence would be wrong.

   {!Cascade.Resolve.supported} is checked against those answers in the one
   direction a document can settle. It promises an answer for every node, so a
   node left undecided under it is a broken promise and fails the run. The other
   way round it promises only that some node goes undecided, and the node it
   means need not be in this document: [:empty] is declined per rule and
   answered per element, and a tree built with createElement alone holds no
   element the readings part over. So a document that decided everything is
   counted and named, never fatal.

   Skips cleanly, with status 0, when node or a headless Chromium is missing,
   and fails when CASCADE_NO_BROWSER asks for silence without acknowledging
   it. *)

open Cascade

let ( // ) = Filename.concat

(* ===== Seeded generation ===== *)

module Rng = struct
  type t = { mutable state : int }

  let v seed = { state = seed * 2654435761 land 0x3FFFFFFF }

  let int t n =
    t.state <- ((t.state * 1103515245) + 12345) land 0x3FFFFFFF;
    t.state mod n

  let pick t l = List.nth l (int t (List.length l))
end

(* ===== The document, as the browser builds it and as NODE reads it ===== *)

type element = {
  tag : string;
  attrs : (string * string) list;
  text : string list;
  mutable up : element option;
  kids : element list;
}

let elt ?(attrs = []) ?(text = []) tag kids =
  let e = { tag; attrs; text; up = None; kids } in
  List.iter (fun k -> k.up <- Some e) kids;
  e

(* HTML splits a class attribute on ASCII whitespace (infra, "split a string on
   ASCII whitespace"), so a class attribute holding a tab carries two names. *)
let is_ascii_space = function
  | ' ' | '\t' | '\n' | '\r' | '\012' -> true
  | _ -> false

let tokens s =
  let out = ref [] and buf = Buffer.create 16 in
  let flush () =
    if Buffer.length buf > 0 then (
      out := Buffer.contents buf :: !out;
      Buffer.clear buf)
  in
  String.iter
    (fun c -> if is_ascii_space c then flush () else Buffer.add_char buf c)
    s;
  flush ();
  List.rev !out

(* The adapter under test. [id] and [classes] are read out of the attribute
   list, as the DOM reads them, so no accessor can report something the
   attributes do not say. *)
module Node = struct
  type t = element

  let equal = ( == )
  let name e = Some e.tag
  let attribute e k = List.assoc_opt k e.attrs
  let id e = attribute e "id"

  let classes e =
    match attribute e "class" with None -> [] | Some v -> tokens v

  let parent e = e.up
  let children e = e.kids
  let text_children e = e.text
end

module R = Resolve.Make (Node)

type doc = { did : string; body : element list; root : element }

let document did body =
  { did; body; root = elt "html" [ elt "head" []; elt "body" body ] }

let rec preorder e = e :: List.concat_map preorder e.kids
let elements d = preorder d.root

let rec label e =
  match e.up with
  | None -> e.tag
  | Some p ->
      let rec index i = function
        | [] -> i
        | x :: _ when x == e -> i
        | _ :: rest -> index (i + 1) rest
      in
      String.concat ""
        [
          label p;
          ">";
          e.tag;
          ":nth-child(";
          string_of_int (index 1 p.kids);
          ")";
        ]

(* The fingerprint the browser is asked to confirm: everything a {!Node}
   accessor reports, in one string. Attributes are sorted by name, so the two
   sides need not agree on insertion order. *)
let unit_separator = "\031"

let fingerprint e =
  let names = List.sort String.compare (List.map fst e.attrs) in
  let attr n =
    String.concat ""
      [ n; "="; Option.value ~default:"" (List.assoc_opt n e.attrs) ]
  in
  String.concat "|"
    [
      e.tag;
      String.concat unit_separator (List.map attr names);
      String.concat unit_separator (Node.classes e);
      String.concat unit_separator e.text;
    ]

let hex s =
  let digits = "0123456789abcdef" in
  let buf = Buffer.create (2 * String.length s) in
  String.iter
    (fun c ->
      Buffer.add_char buf digits.[Char.code c lsr 4];
      Buffer.add_char buf digits.[Char.code c land 0xf])
    s;
  Buffer.contents buf

let rec json_of_element e =
  let field name = function [] -> [] | l -> [ (name, Json.Arr l) ] in
  Json.Obj
    (("t", Json.Str e.tag)
    :: (field "a"
          (List.map (fun (n, v) -> Json.Arr [ Json.Str n; Json.Str v ]) e.attrs)
       @ field "k" (List.map json_of_element e.kids)
       @ field "x" (List.map (fun t -> Json.Str t) e.text)))

(* ===== The documents =====

   Tags are drawn from the ones the HTML parser leaves alone when nested, since
   the question here is selectors and not parser recovery. The other side builds
   with createElement for the same reason. *)

let doc_basic =
  document "basic"
    [
      elt "div"
        ~attrs:
          [
            ("id", "one");
            ("class", "a b");
            ("title", "hello world");
            ("data-x", "alpha-beta");
            ("type", "text");
            ("lang", "en-US");
          ]
        [
          elt "span" ~attrs:[ ("class", "a"); ("title", "hello") ] [];
          elt "u" ~attrs:[ ("rel", "a b") ] ~text:[ "text" ] [];
          elt "a"
            ~attrs:[ ("class", "c"); ("href", "http://x/y") ]
            ~text:[ "link" ] [];
        ];
      elt "section"
        ~attrs:[ ("class", "a") ]
        [
          elt "div" ~attrs:[ ("class", "b") ] [];
          elt "div" ~attrs:[ ("class", "b c"); ("id", "two") ] [];
          elt "span" [];
        ];
      elt "div" [];
      elt "div" ~text:[ " " ] [];
      elt "div" ~text:[ "x" ] [];
    ]

let doc_types =
  document "types"
    [
      elt "section"
        [
          elt "div" ~attrs:[ ("class", "a") ] [];
          elt "span" [];
          elt "div" ~attrs:[ ("class", "b") ] [];
          elt "u" [];
          elt "span" ~attrs:[ ("class", "a") ] [];
          elt "div" [];
          elt "a" [];
          elt "span" ~attrs:[ ("class", "b") ] [];
          elt "div" ~attrs:[ ("class", "a b") ] [];
          elt "img" [];
          elt "input" [];
        ];
    ]

let doc_attrs =
  document "attrs"
    [
      elt "div" ~attrs:[ ("title", "") ] [];
      elt "div" ~attrs:[ ("title", "a") ] [];
      elt "div" ~attrs:[ ("title", "a b") ] [];
      elt "div" ~attrs:[ ("title", "a\tb") ] [];
      elt "div" ~attrs:[ ("title", "a\nb") ] [];
      elt "div" ~attrs:[ ("title", "hello world") ] [];
      elt "div" ~attrs:[ ("title", "HELLO") ] [];
      elt "div" ~attrs:[ ("title", "en-US") ] [];
      elt "div" ~attrs:[ ("data-x", "alpha") ] [];
      elt "div" ~attrs:[ ("data-x", "alpha-beta") ] [];
      elt "div" ~attrs:[ ("data-x", "ALPHA") ] [];
      elt "div" ~attrs:[ ("hidden", "") ] [];
      elt "div" ~attrs:[ ("nope", "a") ] [];
      elt "a" ~attrs:[ ("href", "http://x/y"); ("rel", "a b") ] [];
    ]

(* The HTML standard lists the attributes whose values a selector matches ASCII
   case-insensitively in an HTML document; [type], [lang], [rel] and [dir] are
   four of them. Class and id values are case-sensitive here, which is a fact
   about standards mode alone. *)
let doc_case =
  document "case"
    [
      elt "div" ~attrs:[ ("class", "a"); ("id", "one") ] [];
      elt "div" ~attrs:[ ("class", "A"); ("id", "One") ] [];
      elt "input" ~attrs:[ ("type", "text") ] [];
      elt "input" ~attrs:[ ("type", "TEXT") ] [];
      elt "div" ~attrs:[ ("lang", "EN") ] [];
      elt "div" ~attrs:[ ("lang", "en") ] [];
      elt "a" ~attrs:[ ("href", "x"); ("rel", "NOFOLLOW") ] [];
      elt "div" ~attrs:[ ("dir", "LTR") ] [];
      elt "div" ~attrs:[ ("title", "Mixed") ] [];
    ]

let doc_esc =
  document "esc"
    [
      elt "div" ~attrs:[ ("id", "123"); ("class", "a.b") ] [];
      elt "div" ~attrs:[ ("class", "caf\xc3\xa9") ] [];
      elt "div" ~attrs:[ ("id", "a:b"); ("class", "--x") ] [];
      elt "div" ~attrs:[ ("class", "x y"); ("id", "_z") ] [];
      elt "u" [];
      elt "a" [];
      elt "u" [];
      elt "span" [];
      elt "a" [];
    ]

let doc_deep =
  document "deep"
    [
      elt "div"
        ~attrs:[ ("class", "l1") ]
        [
          elt "div"
            ~attrs:[ ("class", "l2 a") ]
            [
              elt "span"
                ~attrs:[ ("class", "l3") ]
                [
                  elt "div"
                    ~attrs:[ ("class", "l4 b") ]
                    [ elt "span" ~attrs:[ ("class", "l5"); ("id", "deep") ] [] ];
                ];
            ];
        ];
      elt "div"
        ~attrs:[ ("class", "l1 b") ]
        [ elt "span" ~attrs:[ ("class", "l2") ] [] ];
      (* A span a div reaches only through something else, so the descendant and
         child combinators have somewhere to disagree. *)
      elt "div"
        ~attrs:[ ("class", "l1 c") ]
        [ elt "u" [ elt "span" ~attrs:[ ("class", "l6") ] [] ] ];
    ]

let doc_has =
  document "has"
    [
      elt "div"
        ~attrs:[ ("class", "h1") ]
        [ elt "span" ~attrs:[ ("class", "t") ] [] ];
      elt "div"
        ~attrs:[ ("class", "h2") ]
        [ elt "div" [ elt "span" ~attrs:[ ("class", "t") ] [] ] ];
      elt "div" ~attrs:[ ("class", "h3") ] [];
      elt "div" ~attrs:[ ("class", "h4") ] [];
      elt "span" ~attrs:[ ("class", "t") ] [];
      elt "div" ~attrs:[ ("class", "h5") ] [];
      elt "div" [];
      elt "span" ~attrs:[ ("class", "t"); ("id", "one") ] [];
    ]

let doc_empty =
  document "empty"
    [
      elt "div" [];
      elt "div" ~text:[ " " ] [];
      elt "div" ~text:[ "\n\t" ] [];
      elt "div" ~text:[ "\xc2\xa0" ] [];
      elt "div" ~text:[ "x" ] [];
      elt "div" [ elt "span" [] ];
      elt "div" ~text:[ " " ] [ elt "span" [] ];
      elt "span" ~attrs:[ ("class", "a") ] [];
    ]

(* Generated documents keep the run from only ever asking about the shapes the
   written ones happen to have. *)
let gen_tags = [ "div"; "span"; "section"; "u"; "b"; "i"; "em"; "strong" ]
let leaf_tags = [ "a"; "h1"; "h2"; "input"; "img" ]
let gen_classes = [ "a"; "b"; "c"; "x"; "y"; "a b"; "b c"; "" ]

let gen_attrs =
  [
    [];
    [ ("title", "hello") ];
    [ ("title", "a b") ];
    [ ("data-x", "alpha-beta") ];
    [ ("type", "text") ];
    [ ("lang", "en-US") ];
    [ ("title", "HELLO"); ("data-x", "ALPHA") ];
  ]

let generated_document rng n =
  let ids = ref 0 in
  let rec node depth =
    let leaf = depth <= 0 || Rng.int rng 4 = 0 in
    let tag =
      if leaf then Rng.pick rng (gen_tags @ leaf_tags)
      else Rng.pick rng gen_tags
    in
    let attrs = Rng.pick rng gen_attrs in
    let attrs =
      match Rng.pick rng gen_classes with
      | "" -> attrs
      | c -> ("class", c) :: attrs
    in
    let attrs =
      if Rng.int rng 5 = 0 then (
        incr ids;
        ("id", String.concat "" [ "g"; string_of_int !ids ]) :: attrs)
      else attrs
    in
    let text = if Rng.int rng 3 = 0 then [ "t" ] else [] in
    let kids =
      if leaf then []
      else List.init (1 + Rng.int rng 4) (fun _ -> node (depth - 1))
    in
    elt tag ~attrs ~text kids
  in
  document
    (String.concat "" [ "gen-"; string_of_int n ])
    (List.init (2 + Rng.int rng 3) (fun _ -> node 3))

(* ===== The selectors ===== *)

type expect = Same | Differs of string | Cascade_rejects of string

type probe = {
  sid : string;
  family : string;
  text : string;  (** what the browser is asked *)
  read : string;  (** what cascade is asked to read *)
  expect : expect;
}

let is_control p = match p.expect with Same -> false | _ -> true

let probe ?(expect = Same) ?read family sid text =
  { sid; family; text; read = Option.value ~default:text read; expect }

let numbered family texts =
  List.mapi
    (fun i t ->
      probe family (String.concat "" [ family; "-"; string_of_int i ]) t)
    texts

let type_selectors =
  [
    "*";
    "div";
    "DIV";
    "Div";
    "span";
    "u";
    "a";
    "section";
    "b";
    "i";
    "input";
    "img";
    "unknown";
    "html";
    "body";
    "head";
    "\\64 iv";
    "*|div";
    "|div";
  ]

let class_selectors =
  [
    ".a";
    ".A";
    ".b";
    ".c";
    ".a.b";
    ".a.c";
    ".nope";
    ".caf\xc3\xa9";
    ".caf\\e9 ";
    ".a\\.b";
    ".\\--x";
    "._z";
    ".x.y";
    ".b.a";
    ".t";
  ]

let id_selectors =
  [ "#one"; "#One"; "#two"; "#\\31 23"; "#a\\:b"; "#nope"; "#_z"; "#deep" ]

(* An unquoted attribute value has to be an identifier, so anything else is
   quoted. This is the ident production of css-syntax-3 (ED) sec. 4.3.11,
   restricted to the ASCII these values hold. *)
let is_ident v =
  let ok_start c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || Char.equal c '_'
  in
  let ok_rest c = ok_start c || (c >= '0' && c <= '9') || Char.equal c '-' in
  match String.length v with
  | 0 -> false
  | _ ->
      let head = if Char.equal v.[0] '-' then 1 else 0 in
      String.length v > head
      && ok_start v.[head]
      && String.for_all ok_rest (String.sub v head (String.length v - head))

let quoted v = String.concat "" [ "\""; v; "\"" ]
let attr_value v = if is_ident v then v else quoted v

let attr_names =
  [
    "title";
    "TITLE";
    "Title";
    "data-x";
    "DATA-X";
    "type";
    "lang";
    "rel";
    "href";
    "id";
    "class";
    "hidden";
    "nope";
  ]

let attr_ops = [ "="; "~="; "|="; "^="; "$="; "*=" ]
let attr_flags = [ ""; " i"; " s" ]

let attr_values = function
  | "title" | "TITLE" | "Title" -> [ "hello"; "HELLO"; "a" ]
  | "data-x" | "DATA-X" -> [ "alpha"; "alpha-beta"; "ALPHA" ]
  | "type" -> [ "text"; "TEXT"; "t" ]
  | "lang" -> [ "en"; "EN"; "en-US" ]
  | "rel" -> [ "a"; "A"; "b" ]
  | "href" -> [ "http://x/y"; "HTTP://X/Y"; "x" ]
  | "id" -> [ "one"; "One"; "two" ]
  | "class" -> [ "a"; "A"; "b" ]
  | "hidden" -> [ ""; "a"; "hidden" ]
  | _ -> [ "a"; "b"; "c" ]

let attribute_selectors =
  let over f = List.concat_map f attr_names in
  let matched =
    over (fun name ->
        List.concat_map
          (fun op ->
            List.concat_map
              (fun v ->
                List.map
                  (fun flag ->
                    String.concat "" [ "["; name; op; attr_value v; flag; "]" ])
                  attr_flags)
              (attr_values name))
          attr_ops)
  in
  (* The same tests written with an explicit quote, which the parser reaches
     through a different token. *)
  let requoted =
    over (fun name ->
        List.concat_map
          (fun op ->
            List.map
              (fun v -> String.concat "" [ "["; name; op; quoted v; "]" ])
              (attr_values name))
          attr_ops)
  in
  let presence =
    over (fun name ->
        List.map
          (fun flag -> String.concat "" [ "["; name; flag; "]" ])
          attr_flags)
  in
  matched @ requoted @ presence

let comb_pool = [ "div"; ".a"; "#one"; "span"; "u"; "*"; "[title]"; ".b" ]
let combinators = [ " "; ">"; "+"; "~" ]

let combinator_selectors rng =
  let pairs =
    List.concat_map
      (fun l ->
        List.concat_map
          (fun c -> List.map (fun r -> String.concat "" [ l; c; r ]) comb_pool)
          combinators)
      comb_pool
  in
  let triples =
    List.init 128 (fun _ ->
        String.concat ""
          [
            Rng.pick rng comb_pool;
            Rng.pick rng combinators;
            Rng.pick rng comb_pool;
            Rng.pick rng combinators;
            Rng.pick rng comb_pool;
          ])
  in
  pairs @ triples

let list_selectors rng =
  let pool =
    comb_pool @ [ ".c"; "#two"; ":first-child"; ":empty"; "div>span"; "u+a" ]
  in
  List.init 96 (fun _ ->
      String.concat ","
        (List.init (2 + Rng.int rng 2) (fun _ -> Rng.pick rng pool)))

let logic_args =
  [
    "div";
    ".a";
    "#one";
    "span";
    "[title]";
    ".b";
    "u";
    ":first-child";
    ".a.b";
    "div.b";
  ]

let logic_templates =
  [
    ":is(X)";
    ":where(X)";
    ":not(X)";
    ":has(X)";
    "div:has(> X)";
    "div:has(+ X)";
    "div:has(~ X)";
    ":is(X, .c)";
    ":not(X, .c)";
    ":has(X, .c)";
    ":is(:not(X))";
    ":not(:is(X))";
    ":has(:is(X))";
    ":is(:has(X))";
    ":not(:has(X))";
    "div:has(span:not(X))";
    ":is(X) > span";
    "section :is(X)";
  ]

let substitute template arg =
  let buf = Buffer.create (String.length template + String.length arg) in
  String.iter
    (fun c ->
      if Char.equal c 'X' then Buffer.add_string buf arg
      else Buffer.add_char buf c)
    template;
  Buffer.contents buf

let logic_selectors =
  List.concat_map (fun t -> List.map (substitute t) logic_args) logic_templates

let nths =
  [
    "odd";
    "even";
    "2n";
    "2n+1";
    "3n-1";
    "-n+3";
    "n";
    "n+2";
    "0n+2";
    "2";
    "1";
    "-1";
    "0";
    "+3";
    "10n-1";
    "-2n+5";
  ]

let nth_functions =
  [ ":nth-child"; ":nth-last-child"; ":nth-of-type"; ":nth-last-of-type" ]

let nth_selectors =
  let plain =
    List.concat_map
      (fun f -> List.map (fun n -> String.concat "" [ f; "("; n; ")" ]) nths)
      nth_functions
  in
  let of_forms =
    List.concat_map
      (fun f ->
        List.concat_map
          (fun n ->
            List.map
              (fun s -> String.concat "" [ f; "("; n; " of "; s; ")" ])
              [ ".a"; "div"; ":not(.b)" ])
          [ "2"; "2n"; "odd"; "-n+2"; "n+1"; "even" ])
      nth_functions
  in
  plain @ of_forms

let structural_selectors =
  let simple =
    [
      ":first-child";
      ":last-child";
      ":only-child";
      ":empty";
      ":root";
      ":first-of-type";
      ":last-of-type";
      ":only-of-type";
      ":scope";
    ]
  in
  List.concat_map
    (fun p -> List.map (fun s -> String.concat "" [ p; s ]) simple)
    [ ""; "div"; ".a"; "span" ]
  @ [
      ":root > *";
      "* :empty";
      "body > :last-child";
      ":root:first-child";
      "div :only-child";
      ":not(:empty)";
    ]

(* Forms the matcher says it has no model for. The browser still answers, so the
   run can say what a caller reading [Unsupported] as "matches nothing" would
   have got wrong. *)
let stateful_selectors =
  [
    ":hover";
    ":focus";
    ":active";
    ":checked";
    ":disabled";
    ":enabled";
    ":target";
    ":link";
    ":visited";
    ":any-link";
    ":lang(en)";
    ":lang(en-US)";
    ":dir(ltr)";
    ":defined";
    ":required";
    ":optional";
    ":read-only";
    ":read-write";
    "::before";
    "::after";
    "::first-line";
    "::first-letter";
    "::marker";
    "::selection";
    "::placeholder";
    "::backdrop";
    "div::before";
    ".a:hover";
    ":not(:hover)";
    ":is(:hover, .a)";
    ":has(:hover)";
    "div:nth-of-type(2n of .a)";
    "div:nth-last-of-type(2 of .a)";
    "[title i]";
    "[title s]";
    ":host";
    ":state(x)";
    "::part(p)";
    "::slotted(span)";
    ":open";
    ":popover-open";
    "div::-webkit-scrollbar";
    ":nth-col(2n)";
    ":-moz-any(div)";
  ]

(* Where [u+a] sits: an ident, a delimiter and an ident, which is also the shape
   a unicode range is read out of. *)
let lexical_selectors =
  [
    "u+a";
    "U+a";
    "u+b";
    "u+span";
    "u+*";
    "u+#one";
    "u+.a";
    "u+ a";
    "u +a";
    "u + a";
    "div u+a";
    ":is(u+a)";
    ":has(u+a)";
    "u+a,div";
    "u~a";
    "u>a";
    "a+u";
    "u+a+u";
    ".a+u";
    "[title]+u";
    ".caf\\e9";
    "#\\31 23";
    "div.a\\.b";
    "[title=\\68 ello]";
    "[data-x=alpha\\-beta]";
    ".\\61";
    ".\\61 bc";
    "\\64 iv.a";
    "[\\74 itle]";
    ".x\\ y";
    "[title=\"\\68 ello\"]";
    "[\\74 itle=hello]";
    ":is(u+a, .a)";
    ":where(u+a)";
    ":not(u+a)";
    "[title~=b]";
    "[class~=a]";
  ]

let generated_probes rng =
  numbered "type" type_selectors
  @ numbered "class" class_selectors
  @ numbered "id" id_selectors
  @ numbered "attr" attribute_selectors
  @ numbered "comb" (combinator_selectors rng)
  @ numbered "list" (list_selectors rng)
  @ numbered "logic" logic_selectors
  @ numbered "nth" nth_selectors
  @ numbered "struct" structural_selectors
  @ numbered "state" stateful_selectors
  @ numbered "lex" lexical_selectors

(* ===== Calibration =====

   Both directions, planted in this file's own input. A control that agrees
   proves only that the run is quiet; one that must disagree, and does, proves
   the comparison can see a wrong answer at all. The perturbation is always in
   what this file hands the library, never in the library. *)
let controls =
  [
    probe "calib" "calib-agree-type" "div";
    probe "calib" "calib-agree-class" ".a";
    probe "calib" "calib-agree-id" "#one";
    probe "calib" "calib-agree-attr" "[title]";
    probe "calib" "calib-agree-sibling" "u + a";
    probe "calib" "calib-agree-nth" ":nth-child(2)";
    probe "calib" "calib-agree-descendant" "section span";
    probe "calib" "calib-wrong-class" ~read:".b"
      ~expect:(Differs "cascade is asked about .b and the browser about .a")
      ".a";
    probe "calib" "calib-wrong-combinator" ~read:"div>span"
      ~expect:
        (Differs "cascade is asked about a child and the browser a descendant")
      "div span";
    probe "calib" "calib-wrong-nth" ~read:":nth-child(3)"
      ~expect:
        (Differs
           "cascade is asked for the third child and the browser the second")
      ":nth-child(2)";
    probe "calib" "calib-wrong-negation" ~read:".a"
      ~expect:
        (Differs "cascade is asked about .a and the browser about its negation")
      ":not(.a)";
    (* The refusal path is driven the way the other controls drive theirs, by
       handing cascade a different text: an empty string is not a selector and
       never will be, where a text cascade happens to refuse today stops
       calibrating anything the day that gap is closed. [u+a] was such a text
       until the reader learned to tokenise it as the sibling combinator it
       is. *)
    probe "calib" "calib-rejected" ~read:""
      ~expect:(Cascade_rejects "the empty string") "div";
  ]

(* A selector the browser answers against its own specification. The finding is
   real and it is not cascade's, so it is named here with the text that settles
   it, and an entry that stops excusing anything is reported the way a stale
   control is: a browser that fixes its bug takes its excuse with it. *)
let browser_disagrees = []

(* ===== What each side answers ===== *)

type browser_answer = Matched of int list | Refused
type cascade_answer = Unreadable of string | Undecided | Decided of int list

let read_selector text =
  match Selector.of_string text with
  | sel -> Ok sel
  | exception Error.Parse_error e -> Error (Error.to_string e)
  | exception Invalid_argument m ->
      Error (String.concat "" [ "Invalid_argument: "; m ])

(* The path a caller actually walks: a rule read out of a sheet. A selector the
   selector API refuses and the sheet parser keeps, or the other way round, is
   worth saying out loud. *)
let sheet_keeps text =
  match
    Css.of_string ~strict:false (String.concat "" [ text; "{color:red}" ])
  with
  | Ok { stylesheet; _ } ->
      not (String.equal "" (Css.to_string ~minify:true stylesheet))
  | Error _ -> false

let cascade_answer sel els =
  let results = List.mapi (fun i e -> (i, R.match_selector sel e)) els in
  let undecided =
    List.exists
      (fun (_, r) -> match r with Resolve.Unsupported -> true | _ -> false)
      results
  in
  if undecided then Undecided
  else
    Decided
      (List.filter_map
         (fun (i, r) -> match r with Resolve.Matches -> Some i | _ -> None)
         results)

(* ===== The driver ===== *)

let read_lines ic =
  let rec loop acc =
    match input_line ic with
    | line -> loop (line :: acc)
    | exception End_of_file -> List.rev acc
  in
  loop []

let run_driver ~node ~chrome ~script ~jobs ~work =
  let errors = work // "driver.err" in
  let cmd =
    String.concat " "
      [
        String.concat "" [ "CHROME="; Filename.quote chrome ];
        Filename.quote node;
        Filename.quote script;
        Filename.quote jobs;
        Filename.quote work;
        String.concat "" [ "2>"; Filename.quote errors ];
      ]
  in
  let ic = Unix.open_process_in cmd in
  let lines = read_lines ic in
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> Ok lines
  | _ ->
      let ic = open_in errors in
      let text = read_lines ic in
      close_in ic;
      Error (String.concat "\n" text)

let rec mkdir_p dir =
  if not (Sys.file_exists dir) then (
    mkdir_p (Filename.dirname dir);
    try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let write file contents =
  let oc = open_out file in
  output_string oc contents;
  close_out oc

let int_of s = try int_of_string (String.trim s) with Failure _ -> -1

let indices s =
  match String.trim s with
  | "" -> []
  | s -> List.map int_of (String.split_on_char ',' s)

type adapter = {
  browser_els : int;
  cascade_els : int;
  bad : int;
  mode : string;
}

type driver_report = {
  mutable user_agent : string;
  answers : (string, browser_answer) Hashtbl.t;
  adapters : (string, adapter) Hashtbl.t;
  mutable prints : (string * int * string) list;
  mutable errors : (string * string) list;
}

let key did sid = String.concat "\000" [ did; sid ]

let parse_driver_output lines =
  let r =
    {
      user_agent = "";
      answers = Hashtbl.create 4096;
      adapters = Hashtbl.create 64;
      prints = [];
      errors = [];
    }
  in
  List.iter
    (fun line ->
      match String.split_on_char '\t' line with
      | [ "V"; ua ] -> r.user_agent <- ua
      | [ "A"; did; browser; cascade; bad; mode ] ->
          Hashtbl.replace r.adapters did
            {
              browser_els = int_of browser;
              cascade_els = int_of cascade;
              bad = int_of bad;
              mode;
            }
      | [ "P"; did; index; fp ] ->
          r.prints <- (did, int_of index, fp) :: r.prints
      | [ "M"; did; sid; hits ] ->
          Hashtbl.replace r.answers (key did sid) (Matched (indices hits))
      | [ "E"; did; sid ] -> Hashtbl.replace r.answers (key did sid) Refused
      | "X" :: did :: rest ->
          r.errors <- (did, String.concat "\t" rest) :: r.errors
      | _ -> ())
    lines;
  r.prints <- List.rev r.prints;
  r.errors <- List.rev r.errors;
  r

(* ===== Findings ===== *)

type finding = { probe : probe; doc_id : string; detail : string list }

let show_set els is =
  match is with
  | [] -> "(nothing)"
  | _ ->
      String.concat ", "
        (List.map
           (fun i ->
             match List.nth_opt els i with
             | Some e -> label e
             | None ->
                 String.concat "" [ "#"; string_of_int i; " out of range" ])
           is)

let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec at i =
    i + n <= h && (String.equal (String.sub hay i n) needle || at (i + 1))
  in
  n = 0 || at 0

(* ===== Main ===== *)

let skip reason = Browser.skip "selector_match" reason

let artefact_root () =
  let dir =
    match Browser.getenv "CASCADE_RENDER_ARTIFACTS" with
    | Some d -> d // "selector"
    | None -> "tmp" // "selector-match"
  in
  if Filename.is_relative dir then Sys.getcwd () // dir else dir

let () =
  let seed = ref 1 in
  let only = ref None in
  let generated = ref 3 in
  let max_shown = ref 60 in
  let args =
    [
      ("--seed", Arg.Set_int seed, "N generation seed (default 1)");
      ( "--only",
        Arg.String (fun s -> only := Some s),
        "SUBSTRING run the probes whose id or text holds SUBSTRING" );
      ( "--generated-docs",
        Arg.Set_int generated,
        "N generated documents beside the written ones (default 3)" );
      ( "--max",
        Arg.Set_int max_shown,
        "N selectors to detail per class (default 60)" );
    ]
  in
  Arg.parse args
    (fun a -> raise (Arg.Bad (String.concat "" [ "unexpected argument "; a ])))
    "selector_match [--seed N] [--only SUBSTRING] [--generated-docs N] [--max \
     N]";
  Browser.suppressed "selector_match";
  let node =
    match Browser.node_binary () with Some n -> n | None -> skip "no node"
  in
  let chrome =
    match Browser.chrome_binary () with
    | Some c -> c
    | None -> skip "no headless browser"
  in
  let rng = Rng.v !seed in
  let docs =
    [
      doc_basic;
      doc_types;
      doc_attrs;
      doc_case;
      doc_esc;
      doc_deep;
      doc_has;
      doc_empty;
    ]
    @ List.init !generated (fun i -> generated_document rng (i + 1))
  in
  let probes = controls @ generated_probes rng in
  let probes =
    match !only with
    | None -> probes
    | Some s ->
        List.filter (fun p -> contains p.sid s || contains p.text s) probes
  in
  let trees = List.map (fun d -> (d, elements d)) docs in
  let root = artefact_root () in
  let work = root // ".work" in
  mkdir_p work;
  let jobs =
    List.map
      (fun (d, els) ->
        Json.Obj
          [
            ("id", Json.Str d.did);
            ("body", Json.Arr (List.map json_of_element d.body));
            ( "fp",
              Json.Arr (List.map (fun e -> Json.Str (hex (fingerprint e))) els)
            );
            ( "sels",
              Json.Arr
                (List.map
                   (fun p ->
                     Json.Obj [ ("id", Json.Str p.sid); ("t", Json.Str p.text) ])
                   probes) );
          ])
      trees
  in
  let jobs_file = work // "jobs.json" in
  write jobs_file (Json.to_string (Json.Arr jobs));
  let script = Filename.dirname Sys.executable_name // "selector_match.js" in
  let lines =
    match run_driver ~node ~chrome ~script ~jobs:jobs_file ~work with
    | Ok lines -> lines
    | Error err ->
        prerr_endline
          (String.concat "\n"
             [ "FAIL: selector_match: the browser driver failed:"; err ]);
        exit 1
  in
  let r = parse_driver_output lines in
  let buf = Buffer.create 65536 in
  let line s = Buffer.add_string buf (String.concat "" [ s; "\n" ]) in

  (* --- The document adapter, before anything is read through it --- *)
  let adapter_bad = ref 0 in
  let quirks = ref [] in
  List.iter
    (fun (d, els) ->
      match Hashtbl.find_opt r.adapters d.did with
      | None ->
          incr adapter_bad;
          line
            (String.concat "" [ "  "; d.did; ": the browser reported nothing" ])
      | Some a ->
          if not (String.equal a.mode "CSS1Compat") then
            quirks := (d.did, a.mode) :: !quirks;
          if
            (not (Int.equal a.browser_els a.cascade_els))
            || a.bad > 0
            || not (Int.equal a.cascade_els (List.length els))
          then (
            incr adapter_bad;
            line
              (String.concat ""
                 [
                   "  ";
                   d.did;
                   ": browser ";
                   string_of_int a.browser_els;
                   " elements, adapter ";
                   string_of_int (List.length els);
                   ", ";
                   string_of_int a.bad;
                   " fingerprint mismatch(es)";
                 ]);
            List.iter
              (fun (did, index, fp) ->
                if String.equal did d.did then
                  line
                    (String.concat ""
                       [
                         "    element ";
                         string_of_int index;
                         " browser ";
                         fp;
                         " adapter ";
                         (match List.nth_opt els index with
                         | Some e -> hex (fingerprint e)
                         | None -> "(no such element)");
                       ]))
              r.prints))
    trees;
  let adapter_report = Buffer.contents buf in
  Buffer.clear buf;

  (* --- Classify every pair --- *)
  let wrong = ref [] and rejected = ref [] and undecided = ref [] in
  let browser_excused = ref [] in
  let browser_excused_used = Hashtbl.create 8 in
  let accepted = ref [] and guard = ref [] and conservative = ref [] in
  let missing = ref 0 and pairs = ref 0 and agreed = ref 0 in
  let blind_unsupported = ref 0 in
  let control_status = ref [] in
  let by_family = Hashtbl.create 32 in
  let bump family =
    Hashtbl.replace by_family family
      (1 + Option.value ~default:0 (Hashtbl.find_opt by_family family))
  in
  List.iter
    (fun p ->
      let sel = read_selector p.read in
      let supported =
        match sel with Ok s -> Resolve.supported s | Error _ -> false
      in
      let disagreements = ref 0 and refused_by_cascade = ref false in
      let record where f = if not (is_control p) then where := f :: !where in
      List.iter
        (fun (d, els) ->
          incr pairs;
          bump p.family;
          match Hashtbl.find_opt r.answers (key d.did p.sid) with
          | None -> incr missing
          | Some browser -> (
              let cascade =
                match sel with
                | Error m -> Unreadable m
                | Ok s -> (
                    match cascade_answer s els with
                    | a -> a
                    | exception e -> Unreadable (Printexc.to_string e))
              in
              let finding detail = { probe = p; doc_id = d.did; detail } in
              let browser_says =
                match browser with
                | Refused -> "browser: refuses the selector"
                | Matched hits ->
                    String.concat ""
                      [
                        "browser: matches ";
                        string_of_int (List.length hits);
                        " element(s): ";
                        show_set els hits;
                      ]
              in
              match (cascade, browser) with
              | Unreadable _, Refused -> incr agreed
              | Unreadable m, Matched _ ->
                  refused_by_cascade := true;
                  record rejected
                    (finding
                       [
                         String.concat "" [ "cascade: "; m ];
                         browser_says;
                         String.concat ""
                           [
                             "the sheet parser keeps it as a rule prelude: ";
                             (if sheet_keeps p.read then "yes" else "no");
                           ];
                       ])
              | Undecided, _ ->
                  (match browser with
                  | Matched (_ :: _) -> incr blind_unsupported
                  | Matched [] | Refused -> ());
                  if supported then
                    record guard
                      (finding
                         [
                           "Resolve.supported says yes, match_selector answers \
                            Unsupported over this document";
                           browser_says;
                         ])
                  else record undecided (finding [ browser_says ])
              | Decided _, Refused -> record accepted (finding [ browser_says ])
              | Decided mine, Matched hits ->
                  if List.equal Int.equal mine hits then (
                    incr agreed;
                    if not supported then
                      record conservative
                        (finding
                           [
                             "Resolve.supported says no, match_selector \
                              decided every node of this document";
                           ]))
                  else if List.mem_assoc p.text browser_disagrees then (
                    incr disagreements;
                    Hashtbl.replace browser_excused_used p.text ();
                    record browser_excused
                      (finding
                         [
                           String.concat "" [ "browser: "; show_set els hits ];
                           String.concat "" [ "cascade: "; show_set els mine ];
                           String.concat ""
                             [
                               "excused: "; List.assoc p.text browser_disagrees;
                             ];
                         ]))
                  else (
                    incr disagreements;
                    record wrong
                      (finding
                         [
                           String.concat "" [ "browser: "; show_set els hits ];
                           String.concat "" [ "cascade: "; show_set els mine ];
                         ]))))
        trees;
      match p.expect with
      | Same -> ()
      | Differs why ->
          control_status :=
            ( p.sid,
              if !disagreements > 0 then Ok why
              else
                Error (String.concat "" [ "no disagreement reported: "; why ])
            )
            :: !control_status
      | Cascade_rejects what ->
          control_status :=
            ( p.sid,
              if !refused_by_cascade then
                String.concat ""
                  [ "cascade refuses "; what; ", the browser accepts it" ]
                |> Result.ok
              else
                Error
                  (String.concat ""
                     [
                       "cascade was expected to refuse ";
                       what;
                       ", which the browser accepts";
                     ]) )
            :: !control_status)
    probes;

  (* --- Report --- *)
  let take n l = List.filteri (fun i _ -> i < n) l in
  (* One selector disagreeing over eleven documents is one finding, not eleven,
     so the section groups by selector and details the first document it showed
     up in. *)
  let group l =
    List.fold_left
      (fun acc f ->
        match acc with
        | (sid, first, docs) :: rest when String.equal sid f.probe.sid ->
            (sid, first, f.doc_id :: docs) :: rest
        | _ -> (f.probe.sid, f, [ f.doc_id ]) :: acc)
      [] (List.rev l)
    |> List.rev_map (fun (sid, first, docs) -> (sid, first, List.rev docs))
  in
  let section name l =
    let groups = group l in
    line "";
    line
      (String.concat ""
         [
           name;
           ": ";
           string_of_int (List.length groups);
           " selector(s), ";
           string_of_int (List.length l);
           " pair(s)";
         ]);
    List.iter
      (fun (_, f, docs) ->
        line (String.concat "" [ "  "; f.probe.sid; "  "; f.probe.text ]);
        line (String.concat "" [ "    documents: "; String.concat " " docs ]);
        line (String.concat "" [ "    in "; f.doc_id; ":" ]);
        List.iter (fun d -> line (String.concat "" [ "      "; d ])) f.detail;
        line
          (String.concat ""
             [
               "    regenerate: dune exec test/render/selector_match.exe -- \
                --seed ";
               string_of_int !seed;
               " --only ";
               f.probe.sid;
             ]))
      (take !max_shown groups);
    if List.length groups > !max_shown then
      line
        (String.concat ""
           [
             "  ... "; string_of_int (List.length groups - !max_shown); " more";
           ])
  in
  line "selector_match: querySelectorAll against Cascade.Resolve";
  line (String.concat "" [ "  browser:   "; r.user_agent ]);
  line
    (String.concat ""
       [
         "  exercised: ";
         string_of_int (List.length probes);
         " selectors, ";
         string_of_int (List.length docs);
         " documents, ";
         string_of_int
           (List.fold_left (fun a (_, els) -> a + List.length els) 0 trees);
         " elements, ";
         string_of_int !pairs;
         " pairs (";
         string_of_int !agreed;
         " agreed, ";
         string_of_int !missing;
         " unanswered)";
       ]);
  if not (String.equal adapter_report "") then (
    line "";
    line "DOCUMENT ADAPTER disagrees with the browser:";
    Buffer.add_string buf adapter_report);
  List.iter
    (fun (did, mode) ->
      line (String.concat "" [ "QUIRKS MODE: "; did; " reported "; mode ]))
    !quirks;
  List.iter
    (fun (did, msg) ->
      line (String.concat "" [ "DRIVER ERROR: "; did; ": "; msg ]))
    r.errors;
  line "";
  line "pairs by family:";
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) by_family []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  |> List.iter (fun (k, v) ->
      line (String.concat "" [ "  "; k; " "; string_of_int v ]));
  section "WRONG MATCH SET (cascade matched a different element set)" !wrong;
  section "REJECTED (cascade cannot read a selector the browser accepts)"
    !rejected;
  section "UNSUPPORTED (cascade reads it and declines to decide)" !undecided;
  section "ACCEPTED (cascade reads a selector the browser refuses)" !accepted;
  section "GUARD (Resolve.supported promised an answer match_selector withheld)"
    !guard;
  section
    "CONSERVATIVE (Resolve.supported declines a selector this document never \
     met a declining element for)"
    !conservative;
  section "BROWSER (the browser answers against its own specification)"
    !browser_excused;
  line "";
  line "calibration:";
  List.iter
    (fun (sid, status) ->
      line
        (String.concat ""
           [
             "  ";
             (match status with Ok _ -> "ok   " | Error _ -> "LOST ");
             sid;
             ": ";
             (match status with Ok m | Error m -> m);
           ]))
    (List.rev !control_status);
  let lost =
    List.filter
      (fun (_, s) -> match s with Error _ -> true | Ok _ -> false)
      !control_status
  in
  line "";
  line
    (String.concat ""
       [
         "unsupported pairs where the browser matched at least one element: ";
         string_of_int !blind_unsupported;
         " (Resolve.matches folds Unsupported into No_match, so a direct \
          resolve caller reads every one of them as \"matches nothing\")";
       ]);
  (* The floor is about the whole run: --only exists to reproduce one finding,
     and a reproduction is meant to be small. *)
  let population_ok =
    match !only with
    | Some _ -> !pairs >= 1 && Int.equal !missing 0
    | None ->
        List.length probes >= 500
        && List.length docs >= 5
        && !pairs >= 5000
        && !missing * 100 <= !pairs
  in
  if not population_ok then
    line
      (String.concat ""
         [
           "FAIL: the population is too small to prove anything: ";
           string_of_int (List.length probes);
           " selectors, ";
           string_of_int (List.length docs);
           " documents, ";
           string_of_int !pairs;
           " pairs, ";
           string_of_int !missing;
           " unanswered";
         ]);
  (* An excuse for a browser bug that no longer excuses anything is reported and
     not failed on: which build answers the run decides whether the bug is
     there, so the same list is stale against a browser that has fixed it and
     current against one that has not. A spec-ahead excuse is a fact about
     cascade and fails when it goes stale; this one is a fact about a
     version. *)
  let stale_excuses =
    List.filter
      (fun (text, _) -> not (Hashtbl.mem browser_excused_used text))
      browser_disagrees
  in
  List.iter
    (fun (text, _) ->
      line
        (String.concat ""
           [ "LOST browser excuse: "; text; " no longer disagrees, drop it" ]))
    stale_excuses;
  let none l = match l with [] -> true | _ -> false in
  let fatal =
    (not population_ok) || !adapter_bad > 0
    || (not (none !quirks))
    || (not (none r.errors))
    || (not (none lost))
    || (not (none !wrong))
    || (not (none !rejected))
    || not (none !guard)
  in
  line (if fatal then "FAIL: selector_match" else "PASS: selector_match");
  print_string (Buffer.contents buf);
  if fatal then exit 1
