(* Differential parse recovery: for malformed CSS, cascade must keep exactly
   what the browser keeps and drop exactly what the browser drops.

   CSS Syntax 3 error handling is normative, not best-effort: every consume
   algorithm states what it returns on a parse error and where parsing resumes
   (ED sec. 5.5.1 to 5.5.8). A minifier that over-discards silently deletes
   working CSS; one that under-discards keeps CSS the browser threw away, so its
   model of the page disagrees with the page.

   The oracle is the browser's CSSOM. Both the malformed input and cascade's
   recovery of it go through the same parser, in the same page: whatever Chrome
   normalises it normalises on both sides, so a difference is cascade's and not
   a difference of spelling. A fact is one rule, or one declaration inside one
   rule, named by its path from the sheet root.

   One deviation is the browser's, not cascade's, and the harness reports it as
   the disagreement it is rather than hiding it: Chrome stops consuming a
   conditional group rule's or an [\@layer] block's contents at a
   [<semicolon-token>] between two rules, where CSS Syntax 3 (ED) sec. 5.5.5
   discards that token and carries on. [\@scope] and a style rule's own block
   follow the spec in the same browser.

   Skips cleanly, with status 0, when node or a headless Chromium is missing.
   CASCADE_NO_BROWSER fails instead: see [Browser.suppressed]. *)

open Cascade

let ( // ) = Filename.concat
let harness = "parse_recovery"
let is_empty = function [] -> true | _ :: _ -> false

(* ===== Strings ===== *)

let delete_range s i len =
  String.concat ""
    [ String.sub s 0 i; String.sub s (i + len) (String.length s - i - len) ]

let insert_at s i t =
  String.concat "" [ String.sub s 0 i; t; String.sub s i (String.length s - i) ]

(* A single-quoted shell word: the only byte that has to leave the quotes is the
   quote itself. *)
let shell_quote s =
  let buf = Buffer.create (String.length s + 8) in
  Buffer.add_char buf '\'';
  String.iter
    (fun c ->
      if Char.equal c '\'' then Buffer.add_string buf "'\\''"
      else Buffer.add_char buf c)
    s;
  Buffer.add_char buf '\'';
  Buffer.contents buf

(* One line, so a report stays readable. *)
let one_line ?(limit = 160) s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if Char.equal c '\n' || Char.equal c '\r' || Char.equal c '\t' then
        Buffer.add_char buf ' '
      else Buffer.add_char buf c)
    s;
  let s = Buffer.contents buf in
  if String.length s <= limit then s
  else String.concat "" [ String.sub s 0 limit; "..." ]

(* ===== Seeded pseudo-randomness ===== *)

(* Its own generator, so a finding reproduces from its seed on any runtime. *)
let rand_state = ref 1

let next_rand bound =
  rand_state := ((!rand_state * 1103515245) + 12345) land 0x3FFFFFFF;
  if bound <= 0 then 0 else !rand_state mod bound

(* ===== The corpus ===== *)

type job = {
  id : string;
  family : string;
  css : string;
  control : string option; (* the well-formed sheet this is a malformation of *)
}

(* Well-formed sheets, one per construct whose recovery the spec describes
   separately. Every property is one the browser enumerates under its own name,
   so a fact names the declaration that was written. *)
let seeds =
  [
    ( "style",
      ".a { color: green; background-color: red }\n.b, .c { z-index: 3 }\n" );
    ( "nested",
      ".a { color: green; & .b { color: red } & > .c { z-index: 1 } }\n" );
    ( "media",
      "@media (min-width: 10px) { .a { color: green } }\n.b { color: red }\n" );
    ( "supports",
      "@supports (color: green) { .a { color: green } }\n.b { color: red }\n" );
    ( "container",
      "@container (min-width: 10px) { .a { color: green } }\n\
       .b { color: red }\n" );
    ("layer", "@layer base { .a { color: green } }\n.b { color: red }\n");
    ("scope", "@scope (.s) { .a { color: green } }\n.b { color: red }\n");
    ( "fontface",
      "@font-face { font-family: f; src: url(a.woff2) }\n.a { color: green }\n"
    );
    ("page", "@page { margin-top: 1px }\n.a { color: green }\n");
    ( "counter",
      "@counter-style c { system: cyclic; symbols: \"x\" }\n\
       .a { color: green }\n" );
    ( "property",
      "@property --p { syntax: \"<color>\"; inherits: false; initial-value: \
       red }\n\
       .a { color: green }\n" );
    ( "keyframes",
      "@keyframes k { from { opacity: 0 } to { opacity: 1 } }\n\
       .a { color: green }\n" );
    ( "custom",
      ".a { --x: 1px; width: var(--x); color: green }\n.b { color: red }\n" );
    ( "url",
      ".a { background-image: url(x.png); color: green }\n.b { color: red }\n"
    );
    ( "string",
      ".a::before { content: \"hi\"; color: green }\n.b { color: red }\n" );
    ("attr", "a[href^=\"x\"] { color: green }\n.b { color: red }\n");
    ("comment", ".a { color: green } /* c */ .b { color: red }\n");
    ("escape", ".\\31 a { color: green }\n.b { color: red }\n");
    ( "important",
      ".a { color: green !important; z-index: 2 }\n.b { color: red }\n" );
    ("starting", "@starting-style { .a { color: green } }\n.b { color: red }\n");
  ]

(* Positions a delimiter opens or closes at, plus both ends. *)
let structural = "{};:,()[]\"'@&"

let structural_positions s =
  let acc = ref [ 0; String.length s ] in
  String.iteri
    (fun i c -> if String.contains structural c then acc := i :: !acc)
    s;
  List.sort_uniq Int.compare !acc

(* What gets inserted: every delimiter, both halves of a comment, the SGML
   comment delimiters, an at-rule in both its forms, and a bare ident. *)
let insertions =
  [
    "(";
    ")";
    "[";
    "]";
    "{";
    "}";
    ";";
    ":";
    "\"";
    "'";
    "\\";
    "!";
    "@";
    ",";
    "&";
    "/*";
    "*/";
    "<!--";
    "-->";
    "@nope ";
    "@nope{}";
    "<bad>";
    "url(";
    " x ";
  ]

let ident_char c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || Char.equal c '-' || Char.equal c '_'

(* Maximal ident-shaped runs, as (offset, length). *)
let ident_runs s =
  let n = String.length s in
  let acc = ref [] in
  let i = ref 0 in
  while !i < n do
    if ident_char s.[!i] then begin
      let start = !i in
      while !i < n && ident_char s.[!i] do
        incr i
      done;
      acc := (start, !i - start) :: !acc
    end
    else incr i
  done;
  List.rev !acc

(* Malformations the mutation families do not reach: a construct valid per its
   own module's grammar but not per the sealed enum a parser holds, and the
   placements CSS Syntax gives a named recovery for. Each carries the
   well-formed sheet it is the malformation of, so a browser-visible loss says
   the input really is invalid. *)
let curated =
  [
    ( "media_general_enclosed",
      "@media (min-width: 0px) { .a { color: green } }",
      "@media (orientation: sideways) or (min-width: 0px) { .a { color: green \
       } }" );
    ( "media_unknown_feature",
      "@media (min-width: 0px) { .a { color: green } }",
      "@media (cascade-nope: 1) { .a { color: green } }" );
    ( "media_unknown_type",
      "@media screen { .a { color: green } }",
      "@media cascade-nope { .a { color: green } }" );
    ( "supports_font_format",
      "@supports not font-format(woff) { .a { color: green } }",
      "@supports not font-format(cascade-nope) { .a { color: green } }" );
    ( "supports_unknown_function",
      "@supports selector(.a) { .a { color: green } }",
      "@supports cascade-nope(.a) { .a { color: green } }" );
    ( "supports_general_enclosed",
      "@supports (color: green) { .a { color: green } }",
      "@supports (cascade-nope) { .a { color: green } }" );
    ( "container_unknown_feature",
      "@container (min-width: 0px) { .a { color: green } }",
      "@container (cascade-nope: 1) { .a { color: green } }" );
    ( "nested_invalid_sibling",
      ".a { color: green; & .b { color: red } & .c { color: blue } }",
      ".a { color: green; .b <::::invalid::::> { color: red } & .c { color: \
       blue } }" );
    ( "custom_prop_close_bracket",
      ".a { --x: ab; color: green }",
      ".a { --x: a]b; color: green }" );
    ( "custom_prop_close_paren",
      ".a { --x: ab; color: green }",
      ".a { --x: a)b; color: green }" );
    ( "custom_prop_bad_url",
      ".a { --x: url(ab); color: green }",
      ".a { --x: url(a b); color: green }" );
    ( "custom_prop_bad_string",
      ".a { --x: \"ab\"; color: green }",
      ".a { --x: \"ab\n; color: green }" );
    ( "custom_prop_semicolon_in_parens",
      ".a { --x: (a); color: green }",
      ".a { --x: (a;b); color: green }" );
    ( "fontface_unknown_at_rule",
      "@font-face { font-family: f; src: url(a.woff2) }",
      "@font-face { font-family: f; @nope; src: url(a.woff2) }" );
    ( "fontface_unknown_at_rule_block",
      "@font-face { font-family: f; src: url(a.woff2) }",
      "@font-face { font-family: f; @nope { y: 1 } src: url(a.woff2) }" );
    ( "fontface_bad_descriptor",
      "@font-face { font-family: f; src: url(a.woff2) }",
      "@font-face { font-family: f; font-weight: notaweight; src: url(a.woff2) \
       }" );
    ( "page_unknown_at_rule",
      "@page { margin-top: 1px; margin-left: 2px }",
      "@page { margin-top: 1px; @nope; margin-left: 2px }" );
    ( "counter_unknown_at_rule",
      "@counter-style c { system: cyclic; symbols: \"x\" }",
      "@counter-style c { system: cyclic; @nope; symbols: \"x\" }" );
    ( "property_unknown_at_rule",
      "@property --p { syntax: \"<color>\"; inherits: false; initial-value: \
       red }",
      "@property --p { syntax: \"<color>\"; @nope; inherits: false; \
       initial-value: red }" );
    ( "keyframes_bad_frame",
      "@keyframes k { from { opacity: 0 } to { opacity: 1 } }",
      "@keyframes k { bogus { opacity: 0 } to { opacity: 1 } }" );
    ( "keyframes_unknown_at_rule",
      "@keyframes k { from { opacity: 0 } to { opacity: 1 } }",
      "@keyframes k { from { opacity: 0 } @nope; to { opacity: 1 } }" );
    ( "selector_list_invalid_member",
      ".a, .b, .c { color: green }",
      ".a, <bad>, .c { color: green }" );
    ( "selector_list_forgiving",
      ":is(.a, .b, .c) { color: green }",
      ":is(.a, <bad>, .c) { color: green }" );
    ( "unknown_at_rule_statement",
      ".a { color: green }\n.b { color: red }",
      "@nope x;\n.a { color: green }\n.b { color: red }" );
    ( "unknown_at_rule_block",
      ".a { color: green }\n.b { color: red }",
      "@nope x { y: 1 }\n.a { color: green }\n.b { color: red }" );
    ( "garbage_before",
      ".a { color: green }\n.b { color: red }",
      "}\n.a { color: green }\n.b { color: red }" );
    ( "garbage_between",
      ".a { color: green }\n.b { color: red }",
      ".a { color: green }\n}\n.b { color: red }" );
    ( "garbage_after",
      ".a { color: green }\n.b { color: red }",
      ".a { color: green }\n.b { color: red }\n}" );
    ( "stray_semicolon_top",
      ".a { color: green }\n.b { color: red }",
      ";\n.a { color: green }\n;\n.b { color: red }\n;" );
    ( "stray_semicolon_block",
      ".a { color: green; z-index: 1 }",
      ".a { ; color: green ; ; z-index: 1 ; }" );
    ( "stray_semicolon_prelude",
      ".a { color: green }\n.b { color: red }",
      ".a ; { color: green }\n.b { color: red }" );
    ( "sgml_comment_top",
      ".a { color: green }\n.b { color: red }",
      "<!--\n.a { color: green }\n-->\n.b { color: red }" );
    ( "sgml_comment_block",
      ".a { color: green; z-index: 1 }",
      ".a { <!-- color: green --> ; z-index: 1 }" );
    ( "sgml_comment_prelude",
      ".a { color: green }\n.b { color: red }",
      "<!-- .a --> { color: green }\n.b { color: red }" );
    ( "bad_declaration_among_good",
      ".a { color: green; z-index: 2 }",
      ".a { color: green; : : : ; z-index: 2 }" );
    ( "bad_value_among_good",
      ".a { color: green; z-index: 3; opacity: 0.5 }",
      ".a { color: green; z-index: 3px; opacity: 0.5 }" );
    ( "missing_colon",
      ".a { color: green; z-index: 2 }",
      ".a { color green; z-index: 2 }" );
    ( "bad_at_rule_prelude",
      ".a { color: green }\n\
       @media screen { .b { color: red } }\n\
       .c { color: blue }",
      ".a { color: green }\n\
       @media ((( { .b { color: red } }\n\
       .c { color: blue }" );
    ( "eof_in_url",
      ".a { background-image: url(x.png) }",
      ".a { background-image: url(x.png" );
    ( "eof_in_string",
      ".a::before { content: \"hi\" }",
      ".a::before { content: \"hi" );
    ( "eof_in_comment",
      ".a { color: green }\n/* c */",
      ".a { color: green }\n/* unterminated" );
    ("eof_in_block", ".a { color: green }", ".a { color: green");
    ( "eof_in_parens",
      ".a { width: calc(1px + 2px) }",
      ".a { width: calc(1px + 2px" );
    ( "nested_unknown_at_rule",
      ".a { color: green; & .b { color: red } }",
      ".a { color: green; @nope; & .b { color: red } }" );
    ( "at_rule_in_declaration_list",
      ".a { color: green; @media (min-width: 0px) { color: red } z-index: 2 }",
      ".a { color: green; @media ((( { color: red } z-index: 2 }" );
    ( "late_charset",
      ".a { color: green }\n.b { color: red }",
      ".a { color: green }\n@charset \"UTF-8\";\n.b { color: red }" );
    ( "late_namespace",
      ".a { color: green }\n.b { color: red }",
      ".a { color: green }\n@namespace x url(y);\n.b { color: red }" );
    ( "unbalanced_open_bracket",
      ".a[x=y] { color: green }\n.b { color: red }",
      ".a[x { color: green }\n.b { color: red }" );
    ( "unbalanced_open_paren",
      ".a:not(.z) { color: green }\n.b { color: red }",
      ".a:not( { color: green }\n.b { color: red }" );
    ( "bang_in_custom_property",
      ".a { --x: y; color: green }",
      ".a { --x: !; color: green }" );
    ( "important_in_custom_property",
      ".a { --x: y; color: green }",
      ".a { --x: y !important; color: green }" );
  ]

let corpus ~sample =
  let jobs = ref [] in
  let add id family css control =
    jobs := { id; family; css; control } :: !jobs
  in
  List.iter
    (fun (name, css) ->
      let seed_id = String.concat "" [ name; "/seed" ] in
      add seed_id "seed" css None;
      let n = String.length css in
      let tag family k =
        String.concat "" [ name; "/"; family; "/"; string_of_int k ]
      in
      (* Truncation at every prefix length: where an unterminated url(, string,
         comment or block starts is not something a case list guesses well. *)
      for k = 1 to n - 1 do
        add (tag "truncate" k) "truncate" (String.sub css 0 k) (Some seed_id)
      done;
      (* Deleting one delimiter unbalances exactly one construct. *)
      List.iteri
        (fun k i ->
          add (tag "delete" k) "delete" (delete_range css i 1) (Some seed_id))
        (List.filter (fun i -> i < n) (structural_positions css));
      (* Inserting one delimiter, at a delimiter and at a sample of the rest. *)
      let positions =
        let sampled = ref [] in
        for _ = 1 to sample do
          sampled := next_rand (n + 1) :: !sampled
        done;
        List.sort_uniq Int.compare (structural_positions css @ !sampled)
      in
      let k = ref 0 in
      List.iter
        (fun i ->
          List.iter
            (fun t ->
              incr k;
              add (tag "insert" !k) "insert" (insert_at css i t) (Some seed_id))
            insertions)
        positions;
      (* Replacing a whole ident reaches an unknown property, an unknown
         at-keyword and an empty prelude, which a one-character edit does
         not. *)
      let k = ref 0 in
      List.iter
        (fun (i, len) ->
          List.iter
            (fun t ->
              incr k;
              add (tag "replace" !k) "replace"
                (insert_at (delete_range css i len) i t)
                (Some seed_id))
            [ "<bad>"; ""; "cascade-nope" ])
        (ident_runs css))
    seeds;
  List.iter
    (fun (label, control, case) ->
      let control_id = String.concat "" [ "curated/"; label; "/control" ] in
      add control_id "curated-control" control None;
      add
        (String.concat "" [ "curated/"; label ])
        "curated" case (Some control_id))
    curated;
  List.rev !jobs

(* ===== Cascade's side ===== *)

type parsed = { output : string; strict_ok : bool; properties : string list }

(* The property names cascade's own reading writes back, so a fact naming one
   the reading no longer holds can be told from one it does. *)
let declared_properties sheet =
  Css.fold
    (fun acc st ->
      match Css.as_rule st with
      | Some (_, declarations, _) ->
          List.fold_left
            (fun acc d -> Css.Declaration.property_name d :: acc)
            acc declarations
      | None -> acc)
    [] sheet

let cascade css =
  let output, properties =
    match Css.of_string ~strict:false css with
    | Ok { Css.stylesheet; _ } ->
        (Css.to_string stylesheet, declared_properties stylesheet)
    | Error _ -> ("", [])
  in
  let strict_ok =
    match Css.of_string ~strict:true css with Ok _ -> true | Error _ -> false
  in
  { output; strict_ok; properties }

(* ===== The browser's answers ===== *)

let read_lines ic =
  let rec loop acc =
    match input_line ic with
    | line -> loop (line :: acc)
    | exception End_of_file -> List.rev acc
  in
  loop []

let run_driver ~node ~chrome ~script ~jobs ~work =
  let errors = work // "parse-recovery.err" in
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
  | Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
      let ic = open_in errors in
      let text = read_lines ic in
      close_in ic;
      Error (String.concat "\n" text)

type answer = { input_facts : string list; output_facts : string list }

let fail message =
  prerr_endline (String.concat "" [ harness; ": "; message ]);
  exit 1

(* One driver round: every job's two sheets, read back into a table. *)
let ask ~node ~chrome ~script ~work jobs =
  let file = work // "jobs.json" in
  let payload =
    Json.Arr
      (List.map
         (fun (id, a, b) ->
           Json.Obj
             [ ("id", Json.Str id); ("a", Json.Str a); ("b", Json.Str b) ])
         jobs)
  in
  let oc = open_out file in
  output_string oc (Json.to_string payload);
  close_out oc;
  match run_driver ~node ~chrome ~script ~jobs:file ~work with
  | Error err -> fail (String.concat "" [ "the browser driver failed:\n"; err ])
  | Ok lines ->
      let table = Hashtbl.create (List.length jobs * 2) in
      let broke = ref [] in
      List.iter
        (fun line ->
          match String.split_on_char '\t' line with
          | "r" :: id :: side :: facts ->
              Hashtbl.replace table (id, side)
                (List.filter (fun f -> not (String.equal f "")) facts)
          | "x" :: id :: side :: rest ->
              broke := (id, side, String.concat "\t" rest) :: !broke
          | _ -> ())
        lines;
      List.iter
        (fun (id, side, message) ->
          prerr_endline
            (String.concat ""
               [
                 harness;
                 ": the page failed on ";
                 id;
                 " side ";
                 side;
                 ": ";
                 message;
               ]))
        !broke;
      if not (is_empty !broke) then exit 1;
      let answers = Hashtbl.create (List.length jobs) in
      List.iter
        (fun (id, _, _) ->
          match
            (Hashtbl.find_opt table (id, "a"), Hashtbl.find_opt table (id, "b"))
          with
          | Some a, Some b ->
              Hashtbl.replace answers id { input_facts = a; output_facts = b }
          | _, _ ->
              fail (String.concat "" [ "the browser said nothing for "; id ]))
        jobs;
      answers

(* ===== Fact algebra ===== *)

(* A fact is `Kind|identity` segments joined by `>`, then an optional
   `#property`. The driver escapes the three separators inside an identity, so
   every one that survives is a separator. *)

(* At-rules whose identity is a prelude the parser may rewrite without losing a
   rule. Erasing it separates the structural question - what survived - from the
   semantic one - what it now means. *)
let prelude_kinds =
  [
    "CSSMediaRule";
    "CSSSupportsRule";
    "CSSContainerRule";
    "CSSLayerBlockRule";
    "CSSLayerStatementRule";
    "CSSScopeRule";
    "CSSStartingStyleRule";
  ]

let rewrite_identities f fact =
  let n = String.length fact in
  let buf = Buffer.create n in
  let rec segment i =
    match String.index_from_opt fact i '|' with
    | None -> Buffer.add_string buf (String.sub fact i (n - i))
    | Some bar ->
        Buffer.add_string buf (String.sub fact i (bar - i));
        Buffer.add_char buf '|';
        let stop = ref (bar + 1) in
        while
          !stop < n
          && (not (Char.equal fact.[!stop] '>'))
          && not (Char.equal fact.[!stop] '#')
        do
          incr stop
        done;
        Buffer.add_string buf
          (f
             (String.sub fact i (bar - i))
             (String.sub fact (bar + 1) (!stop - bar - 1)));
        if !stop < n && Char.equal fact.[!stop] '>' then begin
          Buffer.add_char buf '>';
          segment (!stop + 1)
        end
        else Buffer.add_string buf (String.sub fact !stop (n - !stop))
  in
  if n > 0 then segment 0;
  Buffer.contents buf

let shape_of =
  rewrite_identities (fun kind id ->
      if List.exists (String.equal kind) prelude_kinds then "" else id)

(* The at-rules whose prelude is a condition, which CSS Conditional 3 sec. 6
   serialises "without any logical simplifications" while allowing the token
   stream ones: "reducing whitespace to a single space or omitting it in cases
   where it is known to be optional". So a space next to a bracket or after a
   [:] is a spelling and not a condition. It is no token boundary either: [(]
   and [)] are tokens of their own, and a [:] inside a condition is followed by
   a value or by a pseudo-class name, never by something a space keeps apart.
   The driver has already reduced every run to one space. *)
let condition_kinds = [ "CSSMediaRule"; "CSSSupportsRule"; "CSSContainerRule" ]

let squeeze id =
  let n = String.length id in
  let buf = Buffer.create n in
  String.iteri
    (fun i c ->
      let after_open =
        i > 0 && (Char.equal id.[i - 1] '(' || Char.equal id.[i - 1] ':')
      in
      let before_close = i + 1 < n && Char.equal id.[i + 1] ')' in
      if not (Char.equal c ' ' && (after_open || before_close)) then
        Buffer.add_char buf c)
    id;
  Buffer.contents buf

let condition_of =
  rewrite_identities (fun kind id ->
      if List.exists (String.equal kind) condition_kinds then squeeze id else id)

let kind_of = rewrite_identities (fun _ _ -> "")

let counts facts =
  let table = Hashtbl.create 32 in
  List.iter
    (fun f ->
      let prev = Option.value ~default:0 (Hashtbl.find_opt table f) in
      Hashtbl.replace table f (prev + 1))
    facts;
  table

(* Every fact [a] holds more copies of than [b], repeated by the excess. *)
let excess a b =
  let cb = counts b in
  Hashtbl.fold
    (fun f n acc ->
      let m = Option.value ~default:0 (Hashtbl.find_opt cb f) in
      if n > m then List.init (n - m) (fun _ -> f) @ acc else acc)
    (counts a) []
  |> List.sort String.compare

(* ===== The verdict for one job ===== *)

type verdict = {
  over : string list; (* the browser kept it, cascade did not *)
  under : string list; (* cascade kept it, the browser did not *)
  rewritten : string list; (* the same rules, under another prelude *)
}

let agreed v = is_empty v.over && is_empty v.under && is_empty v.rewritten

let verdict answer =
  let sa = List.map shape_of answer.input_facts in
  let sb = List.map shape_of answer.output_facts in
  let over = excess sa sb and under = excess sb sa in
  if not (is_empty over && is_empty under) then { over; under; rewritten = [] }
  else
    let ca = List.map condition_of answer.input_facts
    and cb = List.map condition_of answer.output_facts in
    let full = excess ca cb @ excess cb ca in
    { over = []; under = []; rewritten = List.sort_uniq String.compare full }

(* The rule a declaration fact belongs to. A fact carries at most one unescaped
   `#`, and it introduces the property. *)
let rule_path fact =
  match String.index_opt fact '#' with
  | Some i -> String.sub fact 0 i
  | None -> fact

let is_declaration fact = Option.is_some (String.index_opt fact '#')

(* The declarations the browser threw away from a rule it still parsed.

   Losing a whole rule is not evidence of a parse error: truncating a sheet at a
   rule boundary loses the rules after it and is well formed. Losing a
   declaration out of a rule the browser still built is, because the text is
   still there and the browser refused it - CSS Syntax 3 (ED) sec. 5.5.6 returns
   nothing for a declaration that does not parse, and the browser is where that
   shows.

   [still_written] is what holds that premise up. A mutation inside a property
   name renames the declaration, and one that eats a [;] merges it into its
   neighbour: the browser loses the fact either way, and there is no refused
   text to refuse. cascade writes an unknown property name back on purpose, so
   the loss on its own says nothing. *)
let internal_losses ~control ~mutant ~still_written =
  let ck = List.map kind_of control and mk = List.map kind_of mutant in
  let rules l = counts (List.filter (fun f -> not (is_declaration f)) l) in
  let cr = rules ck and mr = rules mk in
  List.filter
    (fun f ->
      is_declaration f && still_written f
      &&
      let p = rule_path f in
      Option.value ~default:0 (Hashtbl.find_opt mr p)
      >= Option.value ~default:0 (Hashtbl.find_opt cr p))
    (excess ck mk)

(* What makes two findings the same finding: the kinds and property names that
   moved, not the selectors they moved under. A reduction has to keep this exact
   signature, or it is a repro of something else. *)
let signature v =
  let part tag l =
    String.concat ""
      [
        tag;
        String.concat "," (List.sort_uniq String.compare (List.map kind_of l));
      ]
  in
  String.concat ";"
    [
      part "over=" v.over; part "under=" v.under; part "rewritten=" v.rewritten;
    ]

(* ===== Reduction ===== *)

(* Candidate reductions of [s]: delete one run, halving the run length down to a
   single byte. Deleting halves converges in far fewer rounds than deleting
   bytes, and the byte-sized runs are what finishes the job. *)
let reductions s =
  let n = String.length s in
  let acc = ref [] in
  let size = ref (max 1 (n / 2)) in
  let stop = ref false in
  while not !stop do
    let i = ref 0 in
    while !i < n do
      let len = min !size (n - !i) in
      if len > 0 && len < n then acc := delete_range s !i len :: !acc;
      i := !i + !size
    done;
    if !size = 1 then stop := true else size := !size / 2
  done;
  List.sort_uniq String.compare !acc

type finding = { job : job; seen : verdict; small : string; alike : int }

let minimise ~node ~chrome ~script ~work ~rounds findings =
  let current = Array.of_list findings in
  let improved = ref true in
  let round = ref 0 in
  while !improved && !round < rounds do
    improved := false;
    incr round;
    let jobs = ref [] in
    Array.iteri
      (fun k f ->
        List.iteri
          (fun j candidate ->
            if not (String.equal candidate "") then
              jobs :=
                ( String.concat ""
                    [ "m/"; string_of_int k; "/"; string_of_int j ],
                  candidate,
                  (cascade candidate).output )
                :: !jobs)
          (reductions f.small))
      current;
    let jobs = List.rev !jobs in
    if not (is_empty jobs) then begin
      let answers = ask ~node ~chrome ~script ~work jobs in
      List.iter
        (fun (id, candidate, _) ->
          match String.split_on_char '/' id with
          | [ "m"; k; _ ] -> (
              let k = int_of_string k in
              let f = current.(k) in
              if String.length candidate < String.length f.small then
                match Hashtbl.find_opt answers id with
                | None -> ()
                | Some answer ->
                    let v = verdict answer in
                    if
                      (not (agreed v))
                      && String.equal (signature v) (signature f.seen)
                    then begin
                      current.(k) <- { f with small = candidate };
                      improved := true
                    end)
          | _ -> ())
        jobs
    end
  done;
  (Array.to_list current, !round)

(* ===== Reporting ===== *)

let regenerate css =
  String.concat ""
    [ "  dune exec test/render/parse_recovery.exe -- --css "; shell_quote css ]

let show_facts label l =
  List.iteri (fun i f -> if i < 8 then Fmt.pr "      %s %s@." label f) l;
  if List.length l > 8 then
    Fmt.pr "      %s ... %d more@." label (List.length l - 8)

let usage () =
  print_endline
    (String.concat "\n"
       [
         "parse_recovery [--full] [--seed N] [--css TEXT] [--only ID] [--count]";
         "";
         "  --full     more insertion positions per seed";
         "  --seed N   the generator's seed (default 1)";
         "  --css T    report on one input and stop";
         "  --only ID  run the corpus jobs whose id starts with ID";
         "  --count    print the corpus size and stop, without a browser";
       ])

(* ===== Entry point ===== *)

let () =
  let full = ref false in
  let only = ref None in
  let corpus_seed = ref !rand_state in
  let literal = ref None in
  let count_only = ref false in
  let rec parse = function
    | [] -> ()
    | "--full" :: rest ->
        full := true;
        parse rest
    | "--count" :: rest ->
        count_only := true;
        parse rest
    | "--seed" :: n :: rest ->
        rand_state := int_of_string n;
        corpus_seed := !rand_state;
        parse rest
    | "--css" :: t :: rest ->
        literal := Some t;
        parse rest
    | "--only" :: id :: rest ->
        only := Some id;
        parse rest
    | ("--help" | "-help") :: _ ->
        usage ();
        exit 0
    | _ :: rest -> parse rest
  in
  parse (List.tl (Array.to_list Sys.argv));

  let jobs =
    match !literal with
    | Some css ->
        [ { id = "literal"; family = "literal"; css; control = None } ]
    | None -> (
        let all = corpus ~sample:(if !full then 12 else 4) in
        match !only with
        | None -> all
        | Some prefix ->
            (* The control a job is measured against has to come with it, or the
               strict check has nothing to compare. *)
            let starts_with s = String.starts_with ~prefix s in
            let picked = List.filter (fun j -> starts_with j.id) all in
            let wanted = Hashtbl.create 16 in
            List.iter
              (fun j ->
                Hashtbl.replace wanted j.id ();
                Option.iter (fun c -> Hashtbl.replace wanted c ()) j.control)
              picked;
            List.filter (fun j -> Hashtbl.mem wanted j.id) all)
  in
  if !count_only then begin
    Fmt.pr "%s: %d input(s)@." harness (List.length jobs);
    exit 0
  end;
  if is_empty jobs then fail "no input matched";
  (* A run that exercised almost nothing is not a green run. *)
  if Option.is_none !literal && Option.is_none !only && List.length jobs < 2000
  then
    fail
      (String.concat ""
         [
           "the corpus collapsed to ";
           string_of_int (List.length jobs);
           " input(s); it should hold thousands";
         ]);

  Browser.suppressed harness;
  let node =
    match Browser.node_binary () with
    | Some n -> n
    | None -> Browser.skip harness "no node"
  in
  let chrome =
    match Browser.chrome_binary () with
    | Some c -> c
    | None -> Browser.skip harness "no headless browser"
  in
  let script = Filename.dirname Sys.executable_name // "parse_recovery.js" in
  let work = Filename.get_temp_dir_name () // "cascade-parse-recovery" in
  (try Unix.mkdir work 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());

  let parses = Hashtbl.create (List.length jobs) in
  List.iter (fun j -> Hashtbl.replace parses j.id (cascade j.css)) jobs;
  let started = Unix.gettimeofday () in
  let answers =
    ask ~node ~chrome ~script ~work
      (List.map
         (fun j -> (j.id, j.css, (Hashtbl.find parses j.id).output))
         jobs)
  in
  let elapsed = Unix.gettimeofday () -. started in

  (* --- one input, reported in full --- *)
  (match !literal with
  | Some css ->
      let answer = Hashtbl.find answers "literal" in
      let parse = Hashtbl.find parses "literal" in
      let v = verdict answer in
      Fmt.pr "input:   %s@." (one_line ~limit:400 css);
      Fmt.pr "cascade: %s@." (one_line ~limit:400 parse.output);
      Fmt.pr "strict:  %s@." (if parse.strict_ok then "Ok" else "Error");
      List.iter (fun f -> Fmt.pr "browser  %s@." f) answer.input_facts;
      List.iter (fun f -> Fmt.pr "cascade  %s@." f) answer.output_facts;
      show_facts "browser kept, cascade dropped:" v.over;
      show_facts "cascade kept, browser dropped:" v.under;
      show_facts "same rules, other prelude:    " v.rewritten;
      exit (if agreed v then 0 else 1)
  | None -> ());

  (* --- the sweep --- *)
  let facts_seen = ref 0 in
  List.iter
    (fun j ->
      facts_seen :=
        !facts_seen + List.length (Hashtbl.find answers j.id).input_facts)
    jobs;

  let findings = ref [] and strict_gaps = ref [] in
  let over_n = ref 0 and under_n = ref 0 and rewritten_n = ref 0 in
  List.iter
    (fun j ->
      let answer = Hashtbl.find answers j.id in
      let v = verdict answer in
      if not (agreed v) then begin
        if not (is_empty v.over) then incr over_n;
        if not (is_empty v.under) then incr under_n;
        if not (is_empty v.rewritten) then incr rewritten_n;
        findings := { job = j; seen = v; small = j.css; alike = 1 } :: !findings
      end;
      (* Strict has to reject whatever the browser itself threw away: a
         declaration the browser refused out of a rule it still built, or
         anything the lenient parse already disagrees with the browser about. *)
      if (Hashtbl.find parses j.id).strict_ok then
        let lost =
          match j.control with
          | None -> []
          | Some control_id -> (
              match Hashtbl.find_opt answers control_id with
              | None -> []
              | Some control ->
                  let props id =
                    match Hashtbl.find_opt parses id with
                    | Some p -> p.properties
                    | None -> []
                  in
                  let before = props control_id and after = props j.id in
                  let times name l =
                    List.length (List.filter (String.equal name) l)
                  in
                  let still_written f =
                    match String.index_opt f '#' with
                    | None -> true
                    | Some i ->
                        let name =
                          String.sub f (i + 1) (String.length f - i - 1)
                        in
                        times name after >= times name before
                  in
                  internal_losses ~control:control.input_facts
                    ~mutant:answer.input_facts ~still_written)
        in
        let reason =
          if not (is_empty lost) then lost
          else if not (agreed v) then v.over @ v.under @ v.rewritten
          else []
        in
        if not (is_empty reason) then strict_gaps := (j, reason) :: !strict_gaps)
    jobs;
  let findings = List.rev !findings and strict_gaps = List.rev !strict_gaps in

  Fmt.pr "%s: %d input(s), %d browser fact(s), seed %d, %.1fs@." harness
    (List.length jobs) !facts_seen !corpus_seed elapsed;
  (* A browser that answered with nothing would make every comparison pass. *)
  if !facts_seen < 2 * List.length jobs then
    fail
      (String.concat ""
         [
           "only ";
           string_of_int !facts_seen;
           " fact(s) over ";
           string_of_int (List.length jobs);
           " input(s); the oracle read nothing";
         ]);

  if is_empty findings && is_empty strict_gaps then begin
    Fmt.pr "%s: cascade kept exactly what the browser kept@." harness;
    exit 0
  end;

  (* One representative per symptom, reduced. *)
  let by_signature = Hashtbl.create 64 in
  List.iter
    (fun f ->
      let s = signature f.seen in
      match Hashtbl.find_opt by_signature s with
      | None -> Hashtbl.replace by_signature s f
      | Some best ->
          let keep =
            if String.length f.job.css < String.length best.job.css then f
            else best
          in
          Hashtbl.replace by_signature s { keep with alike = best.alike + 1 })
    findings;
  let reps =
    Hashtbl.fold (fun _ f acc -> f :: acc) by_signature []
    |> List.sort (fun a b ->
        match Int.compare b.alike a.alike with
        | 0 -> String.compare a.job.id b.job.id
        | c -> c)
  in
  let reduced, rounds = minimise ~node ~chrome ~script ~work ~rounds:30 reps in

  Fmt.pr
    "@.%s: %d disagreement(s) over %d input(s): %d over-discard, %d \
     under-discard, %d prelude rewrite; %d distinct symptom(s), reduced in %d \
     round(s)@."
    harness (List.length findings) (List.length jobs) !over_n !under_n
    !rewritten_n (List.length reduced) rounds;

  let label v =
    if not (is_empty v.over || is_empty v.under) then "OVER+UNDER-DISCARD"
    else if not (is_empty v.over) then "OVER-DISCARD"
    else if not (is_empty v.under) then "UNDER-DISCARD"
    else "PRELUDE-REWRITE"
  in
  let show section keep =
    let listed = List.filter (fun f -> keep f.seen) reduced in
    if not (is_empty listed) then begin
      Fmt.pr "@.=== %s (%d) ===@." section (List.length listed);
      List.iter
        (fun f ->
          Fmt.pr "@.%s [%s] %s, %d input(s) in the corpus@." (label f.seen)
            f.job.family f.job.id f.alike;
          Fmt.pr "  minimised: %s@." (one_line f.small);
          if not (String.equal f.small f.job.css) then
            Fmt.pr "  from:      %s@." (one_line f.job.css);
          Fmt.pr "  cascade:   %s@." (one_line (cascade f.small).output);
          show_facts "browser kept, cascade dropped:" f.seen.over;
          show_facts "cascade kept, browser dropped:" f.seen.under;
          show_facts "same rules, other prelude:    " f.seen.rewritten;
          Fmt.pr "%s@." (regenerate f.small))
        listed
    end
  in
  show "OVER-DISCARDING: working CSS the browser kept and cascade deleted"
    (fun v -> not (is_empty v.over));
  show "UNDER-DISCARDING: CSS the browser threw away and cascade kept" (fun v ->
      (not (is_empty v.under)) && is_empty v.over);
  show "PRELUDE REWRITES: the same rules under a different condition" (fun v ->
      is_empty v.over && is_empty v.under);

  if not (is_empty strict_gaps) then begin
    Fmt.pr "@.=== STRICT ACCEPTED WHAT THE BROWSER REJECTED (%d) ===@."
      (List.length strict_gaps);
    List.iteri
      (fun i (j, lost) ->
        if i < 25 then begin
          Fmt.pr "@.%s [%s]@." j.id j.family;
          Fmt.pr "  input:   %s@." (one_line j.css);
          List.iteri
            (fun k f -> if k < 4 then Fmt.pr "      the browser refused %s@." f)
            lost;
          Fmt.pr "  dune exec test/render/parse_recovery.exe -- --only %s@."
            j.id
        end)
      strict_gaps;
    if List.length strict_gaps > 25 then
      Fmt.pr "@.  ... %d more@." (List.length strict_gaps - 25)
  end;
  exit 1
