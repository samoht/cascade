(* Differential custom-property substitution: a transform that resolves [var()]
   must not change what the browser paints.

   CSS Variables 1 (ED) sec. 2 makes a custom property hold an arbitrary token
   stream, and sec. 3 substitutes that stream into a [var()] textually, at
   computed-value time. Two consequences are normative and easy to get wrong. A
   declaration containing a syntactically valid [var()] is valid at parse time
   whatever the rest of it says (sec. 3), so it wins its cascade slot and only
   then, if substitution leaves something the property's grammar rejects,
   becomes invalid at computed-value time: it takes the unset value rather than
   letting the declaration under it win. And a custom property whose value is a
   CSS-wide keyword never holds that keyword as a stream: sec. 2 says they "are
   not preserved as the custom property's value, and thus are not substituted in
   by the corresponding variable", so [--x: initial] leaves [--x] with the
   guaranteed-invalid value and [var(--x)] is then invalid at computed-value
   time. An empty value is not that: sec. 2.2 makes [--x:] "a valid (empty)
   value, not the guaranteed-invalid value", so a [var()] reading it substitutes
   nothing rather than taking its fallback.

   The oracle is [Browser_compare.run]: the page is rendered under the input
   text as the author wrote it and under each transform's output, and
   [Browser_compare.identical] is the verdict. A green run says the transform
   preserved the page, not that two transforms agree with each other, and
   nothing here decides that two spellings are one: the browser paints them
   alike or it does not. A custom property is not itself painted, so what the
   run checks is every binding a rendered property reads through a [var()];
   every consumer below paints what it reads, and a binding nothing reads is
   outside what a transform has to keep.

   A pair costs a browser launch, a second or two, so the default run renders
   the curated sheets and a seeded sample of sixteen from the value x consumer x
   shape product, about 110 pairs in under three minutes. [--full] renders the
   whole product, thousands of pairs over hours; [--sample N] sizes the slice,
   and [--only ID] and [--css TEXT] narrow the run to one case.

   Skips cleanly, with status 0, when node or a headless Chromium is missing.
   CASCADE_NO_BROWSER fails instead: see [Browser.suppressed]. *)

open Cascade

let harness = "custom_property"
let is_empty = function [] -> true | _ :: _ -> false

let fail message =
  prerr_endline (String.concat "" [ harness; ": "; message ]);
  exit 1

(* ===== Strings ===== *)

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
let one_line ?(limit = 170) s =
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

(* ===== The document ===== *)

(* Three nested elements, so inheritance has somewhere to travel and a shadowing
   definition has somewhere to sit, and a word in the innermost so a colour or a
   font paints. Its inline style makes every consumer below visible without
   writing any property a consumer writes and without reading any custom
   property, which the closed-world transforms are entitled to delete: a fixed
   height, an opaque background and an inset shadow draw the box, so a width or
   a margin moves pixels, and an outline width and style let [outline-color]
   paint. A positioned sibling overlapping the box paints above it unless
   [z-index] lifts the box; [#i] is positioned in a rule rather than inline,
   where a consumer's [z-index] could not lose to it. *)
let document =
  "<div id=\"o\" class=\"o\"><div id=\"m\" class=\"m\"><div id=\"i\" \
   class=\"i\" style=\"height:20px;background-color:#ddd;box-shadow:inset 0 0 \
   0 3px #888;outline-width:2px;outline-style:solid\">ab</div><div id=\"j\" \
   style=\"position:relative;z-index:3;margin-top:-12px;height:14px;width:60px;background-color:#0af\"></div></div></div>"

let base_rules = "#i{position:relative}"

(* ===== The corpus ===== *)

type job = {
  id : string;
  family : string;
  css : string;
  (* The custom properties a context may be built from: every one the sheet
     declares exactly once, unconditionally, at the top level. [None] when the
     sheet declares one under a condition, in a layer, or twice, because a flat
     context would then be a claim the sheet does not make. *)
  root_customs : (string * string) list option;
}

type consumer = {
  ctag : string;
  csel : string; (* the rule the reader sits in *)
  cdecl : string; (* reads var(--x) *)
  cprev : string; (* a literal declaration of the same property *)
}

(* One consumer per shape a substitution can land in: a whole value, a value
   with a fallback, a component of a list, an argument of a function, a
   shorthand, a nested fallback, and a custom property. [cprev] is the same
   property spelled literally, so a shape can put a live declaration under the
   var() one and turn "both sides end up invalid" into an observable difference.
   [content] paints on a pseudo-element only, so that consumer reads on
   [::before]; a custom property paints nothing, so that one is read on by a
   property that does. *)
let consumer ?(csel = "#i") ctag cdecl cprev = { ctag; csel; cdecl; cprev }

let consumers =
  [
    consumer "width" "width:var(--x)" "width:37px";
    consumer "widthfb" "width:var(--x,9px)" "width:37px";
    consumer "glue" "width:var(--x)px" "width:37px";
    consumer "colour" "color:var(--x)" "color:rgb(1,2,3)";
    consumer "colourfb" "color:var(--x,lime)" "color:rgb(1,2,3)";
    consumer "margin" "margin:var(--x) 4px" "margin:11px";
    consumer "calc" "width:calc(var(--x)*2)" "width:37px";
    consumer "transform" "transform:var(--x)" "transform:translateY(5px)";
    consumer "border" "border:var(--x) solid red" "border:9px dotted blue";
    consumer "font" "font-family:var(--x)" "font-family:Courier";
    consumer ~csel:"#i::before" "content" "content:var(--x)" "content:\"zz\"";
    consumer "nestfb" "width:var(--nope,var(--x,8px))" "width:37px";
    consumer "image" "background-image:var(--x)"
      "background-image:linear-gradient(blue,blue)";
    consumer "zindex" "z-index:var(--x)" "z-index:7";
    consumer "custom" "--y:var(--x);outline-color:var(--y)" "--y:zz";
    consumer "outline" "outline-color:var(--x)" "outline-color:rgb(4,5,6)";
  ]

(* One custom-property value per kind of token stream a substitution has to
   carry: lengths and colours that resolve, a stream that does not parse as the
   consumer's grammar, the empty stream, the CSS-wide keywords sec. 2 takes away
   from the stream, a stream that is itself a reference, and the bracket shapes
   CSS Syntax 3 (ED) sec. 7.2 rules on. *)
let values =
  [
    ("len", "1px");
    ("len2", "3px");
    ("pct", "50%");
    ("num", "10");
    ("badunit", "1p");
    ("colour", "red");
    ("hex", "#0f0");
    ("empty", "");
    ("space", " ");
    ("initial", "initial");
    ("inherit", "inherit");
    ("unset", "unset");
    ("revert", "revert");
    ("twoident", "a  b");
    ("comma", "1px, 2px");
    ("calc", "calc(1px + 1px)");
    ("calcvar", "calc(var(--n)*2)");
    ("chain", "var(--n)");
    ("chainfb", "var(--nope,7px)");
    ("selfref", "var(--x)");
    ("func", "translateX(3px)");
    ("grad", "linear-gradient(red,red)");
    ("string", "\"hi\"");
    ("bang", "!");
    ("keyword", "solid");
  ]

(* Where the definition sits relative to the reader. [root] and [same] put a
   single unconditional definition in reach of a flat context; the rest do not,
   and say so by carrying no context. *)
let shapes =
  [
    "root";
    "same";
    "prev";
    "shadow";
    "layer";
    "important";
    "media";
    "inherit-chain";
  ]

let build_shape shape value consumer =
  let defs = String.concat "" [ "--n:5px;--x:"; value ] in
  let read =
    String.concat "" [ base_rules; consumer.csel; "{"; consumer.cdecl; "}" ]
  in
  let read_prev =
    String.concat ""
      [
        base_rules; consumer.csel; "{"; consumer.cprev; ";"; consumer.cdecl; "}";
      ]
  in
  let root_only = Some [ ("--n", "5px"); ("--x", value) ] in
  match shape with
  | "root" -> (String.concat "" [ ":root{"; defs; "}"; read ], root_only)
  | "same" ->
      ( String.concat ""
          [ base_rules; consumer.csel; "{"; defs; ";"; consumer.cdecl; "}" ],
        root_only )
  | "prev" -> (String.concat "" [ ":root{"; defs; "}"; read_prev ], root_only)
  | "shadow" -> (String.concat "" [ ":root{"; defs; "}#m{--x:2px}"; read ], None)
  | "layer" ->
      ( String.concat ""
          [
            "@layer a,b;@layer a{:root{--n:5px;--x:2px}}@layer b{:root{--x:";
            value;
            "}}";
            read;
          ],
        None )
  | "important" ->
      ( String.concat "" [ ":root{"; defs; "!important}:root{--x:2px}"; read ],
        None )
  | "media" ->
      ( String.concat ""
          [
            ":root{--n:5px}@media (min-width:1px){:root{--x:"; value; "}}"; read;
          ],
        None )
  | "inherit-chain" ->
      ( String.concat ""
          [
            "#o{";
            defs;
            "}#m{--z:var(--x)}";
            base_rules;
            consumer.csel;
            "{";
            consumer.cdecl;
            "}";
          ],
        None )
  | other -> fail (String.concat "" [ "unknown shape "; other ])

(* Sheets written out whole, for the cases a product of a value and a consumer
   cannot express: a cycle needs two definitions that refer to each other, a
   registration needs an at-rule, and a [var()] in a position that is not a
   value needs a different grammar entirely. *)
let curated =
  [
    ( "cycle-two",
      "#o{color:rgb(1,2,3)}:root{--a:var(--b);--b:var(--a)}#i{color:var(--a)}"
    );
    ("cycle-self", "#o{color:rgb(1,2,3)}:root{--a:var(--a)}#i{color:var(--a)}");
    ( "cycle-three",
      "#o{color:rgb(1,2,3)}:root{--a:var(--b);--b:var(--c);--c:var(--a)}#i{color:var(--a)}"
    );
    ( "cycle-fallback",
      "#o{color:rgb(1,2,3)}:root{--a:var(--b);--b:var(--a)}#i{color:var(--a,lime)}"
    );
    ( "cycle-one-live",
      "#o{color:rgb(1,2,3)}:root{--a:var(--b);--b:var(--a);--c:teal}#i{color:var(--c)}"
    );
    ("invalid-none", "#o{color:rgb(1,2,3)}#i{color:var(--nothing)}");
    ( "invalid-under",
      "#o{color:rgb(1,2,3)}#i{color:rgb(9,9,9);color:var(--nothing)}" );
    ( "invalid-under-width",
      "#i{position:relative;width:37px;width:var(--nothing)}" );
    ( "registered-match",
      "@property \
       --p{syntax:\"<length>\";inherits:true;initial-value:4px}:root{--p:9px}#i{width:var(--p)}"
    );
    ( "registered-mismatch",
      "@property \
       --p{syntax:\"<length>\";inherits:true;initial-value:4px}:root{--p:red}#i{width:var(--p)}"
    );
    ( "registered-noninherit",
      "@property \
       --p{syntax:\"<length>\";inherits:false;initial-value:4px}:root{--p:9px}#i{width:var(--p)}"
    );
    ( "registered-universal",
      "@property \
       --p{syntax:\"*\";inherits:true}:root{--p:9px}#i{width:var(--p)}" );
    ( "registered-no-initial",
      "@property \
       --p{syntax:\"<length>\";inherits:true}:root{--p:9px}#i{width:var(--p)}"
    );
    ( "registered-computed",
      "@property \
       --p{syntax:\"<length>\";inherits:true;initial-value:0px}#o{font-size:20px;--p:2em}#i{width:var(--p)}"
    );
    ("fallback-comma", "#i{font-family:var(--nope,Times,serif)}");
    ("fallback-empty", "#i{position:relative;width:var(--nope,)}");
    ( "fallback-invalid",
      "#i{position:relative;width:37px;width:var(--nope,notalength)}" );
    ( "fallback-deep",
      "#i{position:relative;width:var(--a,var(--b,var(--c,6px)))}" );
    ( "fallback-live-var",
      ":root{--b:6px}#i{position:relative;width:var(--a,var(--b))}" );
    ( "important-consumer",
      ":root{--x:5px}#i{position:relative;width:37px;width:var(--x)!important}"
    );
    ( "important-definition",
      ":root{--x:5px!important}:root{--x:2px}#i{position:relative;width:var(--x)}"
    );
    ( "revert-layer",
      "@layer a,b;@layer a{:root{--x:2px}}@layer \
       b{:root{--x:revert-layer}}#i{position:relative;width:var(--x)}" );
    ("name-in-var", ":root{--p:width}#i{position:relative;var(--p):3px}");
    ("var-in-selector", ":root{--s:.i}#i{position:relative}var(--s){color:red}");
    ("var-in-media", ":root{--w:1px}@media (min-width:var(--w)){#i{color:red}}");
    ( "var-in-supports",
      ":root{--w:1px}@supports (width:var(--w)){#i{color:red}}" );
    ("stream-unmatched-bracket", "#i{position:relative;--x:a]b;width:var(--x)}");
    ("stream-semicolon-in-block", "#i{position:relative;--x:(a;b);color:green}");
    ("stream-comment", "#i{position:relative;--x:/* c */1px;width:var(--x)}");
    ("stream-escape", "#i{position:relative;--x:\\31 px;width:var(--x)}");
    ("stream-trailing-space", "#i{position:relative;--x:  1px  ;width:var(--x)}");
    ("stream-number", "#i{position:relative;--x:10.0px;width:var(--x)}");
    ("stream-comma-space", "#i{--x:1px, 2px;font-family:var(--x)}");
    ("stream-bang", ":root{--x:!}#i{position:relative;width:var(--x,5px)}");
    ("glue-empty", "#i{position:relative;width:37px;--x: ;width:var(--x)10px}");
    ( "two-refs-one-missing",
      "#i{position:relative;margin:11px;--a:1px;margin:var(--a) var(--z)}" );
    ( "shorthand-whole",
      "#i{position:relative;--b:1px solid red;border:var(--b)}" );
    ("unit-em", "#o{font-size:20px}#i{position:relative;--x:2em;width:var(--x)}");
    ( "unit-rem",
      ":root{font-size:16px}#i{position:relative;--x:2rem;width:var(--x)}" );
    ( "currentcolor",
      "#i{position:relative;color:rgb(7,8,9);--x:currentColor;outline-color:var(--x)}"
    );
    ( "animation-name",
      "@keyframes \
       k{from{opacity:0}to{opacity:1}}:root{--k:k}#i{position:relative;animation-name:var(--k);animation-duration:1s}"
    );
    ("all-shorthand", ":root{--x:inherit}#o{color:rgb(1,2,3)}#i{all:var(--x)}");
  ]

let product =
  List.concat_map
    (fun (vtag, value) ->
      List.concat_map
        (fun consumer ->
          List.map (fun shape -> (shape, vtag, value, consumer)) shapes)
        consumers)
    values

(* A seeded slice of the product, so a default run is a sample of it and
   [--full] is the whole of it. *)
let pick_product sample =
  let all = Array.of_list product in
  let n = Array.length all in
  if sample >= n then product
  else
    let chosen = ref [] in
    while List.length !chosen < sample do
      let k = next_rand n in
      if not (List.mem k !chosen) then chosen := k :: !chosen
    done;
    List.map (fun k -> all.(k)) (List.rev !chosen)

let corpus ~sample =
  let generated =
    List.map
      (fun (shape, vtag, value, consumer) ->
        let css, root_customs = build_shape shape value consumer in
        {
          id = String.concat "/" [ shape; vtag; consumer.ctag ];
          family = shape;
          css;
          root_customs;
        })
      (pick_product sample)
  in
  generated
  @ List.map
      (fun (name, css) ->
        {
          id = String.concat "" [ "curated/"; name ];
          family = "curated";
          css;
          root_customs = None;
        })
      curated

(* ===== Cascade's side ===== *)

type leg = { lname : string; lcss : string }

let parse css =
  match Css.of_string ~strict:false css with
  | Ok { Css.stylesheet; _ } -> Some stylesheet
  | Error _ -> None

let print sheet = Css.to_string ~minify:true sheet

let context_of customs =
  let decls =
    List.filter_map
      (fun (name, value) -> Css.Declaration.parse_custom_property name value)
      customs
  in
  Css.Context.v ~custom_properties:decls ()

let transforms = [ "minify"; "inline-vars"; "optimize"; "eval" ]

(* Every transform in the library that can rewrite a [var()], plus the plain
   round trip that says whether the reader and the printer alone already moved
   the sheet. A leg that raises contributes nothing rather than aborting the
   run: the other legs of the job are still answers. *)
let legs job =
  match parse job.css with
  | None -> []
  | Some sheet ->
      let attempt name f =
        match f sheet with
        | text -> Some { lname = name; lcss = text }
        | exception _ -> None
      in
      List.filter_map Fun.id
        [
          attempt "minify" print;
          attempt "inline-vars" (fun s -> print (Css.inline_vars s));
          attempt "optimize" (fun s -> print (Css.optimize s));
          (match job.root_customs with
          | None -> None
          | Some customs ->
              let ctx = context_of customs in
              attempt "eval" (fun s -> print (Css.eval_stylesheet ctx s)));
        ]

(* The legs worth a browser launch: one per distinct text that is not the
   input's, since a transform that did nothing preserved the page by
   construction, and two that print alike are one render. *)
let distinct_legs ~css legs =
  let seen = ref [ css ] in
  List.filter
    (fun l ->
      if List.mem l.lcss !seen then false
      else (
        seen := l.lcss :: !seen;
        true))
    legs

(* ===== The browser's answer ===== *)

type answer = Same | Differs of Browser_compare.t

(* The page under the input and under one leg; a run the browser could not
   complete is a failure of the harness, never an answer. *)
let ask ~css leg =
  match
    Browser_compare.run ~html:document [ ("input", css); (leg.lname, leg.lcss) ]
  with
  | Error e -> fail (String.concat "" [ "the browser comparison failed: "; e ])
  | Ok report ->
      if Browser_compare.identical report then Same else Differs report

let show_report (report : Browser_compare.t) =
  List.iter
    (fun (r : Browser_compare.render) ->
      Fmt.pr "      %s %s: %dx%d pixels differ at (%d,%d)@." r.viewport r.state
        r.width r.height r.x r.y)
    report.renders;
  List.iteri
    (fun i (d : Browser_compare.difference) ->
      if i < 6 then
        Fmt.pr "      %s%s %s: %s -> %s@." d.element d.pseudo d.property
          (one_line ~limit:60 d.first)
          (one_line ~limit:60 d.second))
    report.differences;
  let n = List.length report.differences in
  if n > 6 then Fmt.pr "      ... %d computed value(s) differ in all@." n

(* ===== Calibration ===== *)

(* Legs the harness writes itself, so the check is exercised by something that
   does not move when cascade does. A wrong substitution has to be caught and a
   spelling that paints the same has to pass; a harness that fails either way
   answers nothing about the transforms it is pointed at. *)
type expectation = Finding | Clean

type control = {
  kname : string;
  kcss : string;
  kleg : string;
  kwant : expectation;
}

let controls =
  [
    (* Reported: the substituted value is not the one the definition holds. *)
    {
      kname = "wrong-length";
      kcss = ":root{--x:1px}#i{position:relative;width:var(--x)}";
      kleg = ":root{--x:1px}#i{position:relative;width:2px}";
      kwant = Finding;
    };
    {
      kname = "wrong-colour";
      kcss = ":root{--x:red}#i{color:var(--x)}";
      kleg = ":root{--x:red}#i{color:blue}";
      kwant = Finding;
    };
    (* Reported: the definition is deleted while a reader still needs it, which
       is the one deletion [--inline-vars] does not licence. *)
    {
      kname = "definition-still-read";
      kcss = ":root{--x:red}#m{color:var(--x)}";
      kleg = "#m{color:var(--x)}";
      kwant = Finding;
    };
    (* Reported: the custom property a rendered property reads is corrupted. *)
    {
      kname = "custom-chain";
      kcss = "#i{--y:red;outline-color:var(--y)}";
      kleg = "#i{--y:blue;outline-color:var(--y)}";
      kwant = Finding;
    };
    (* Reported: an invalid-at-computed-value-time declaration is dropped
       instead, so the declaration under it wins. *)
    {
      kname = "iacvt-vs-dropped";
      kcss = "#i{position:relative;width:37px;width:var(--nothing)}";
      kleg = "#i{position:relative;width:37px}";
      kwant = Finding;
    };
    (* Passed: a different spelling of the same paint. *)
    {
      kname = "same-colour";
      kcss = ":root{--x:#f00}#i{color:var(--x)}";
      kleg = ":root{--x:#f00}#i{color:rgb(255,0,0)}";
      kwant = Clean;
    };
    {
      kname = "same-length";
      kcss = ":root{--x:1px}#i{position:relative;width:var(--x)}";
      kleg = ":root{--x:1px}#i{position:relative;width:1px}";
      kwant = Clean;
    };
    (* Passed: both sides are invalid at computed-value time, by different
       routes, and the property takes the unset value either way. *)
    {
      kname = "both-invalid";
      kcss = "#i{position:relative;--x:1p;width:var(--x)}";
      kleg = "#i{position:relative;--x:1p;width:var(--nothing)}";
      kwant = Clean;
    };
    (* Passed: the substitution happened and the binding went with it, which is
       what [--inline-vars] does, and a binding nothing reads is not painted. *)
    {
      kname = "definition-removed";
      kcss = ":root{--x:1px}#i{position:relative;width:var(--x)}";
      kleg = "#i{position:relative;width:1px}";
      kwant = Clean;
    };
    {
      kname = "binding-nothing-reads";
      kcss = "#i{--x:1px}";
      kleg = "#i{--x:2px}";
      kwant = Clean;
    };
  ]

let describe = function
  | Finding -> "a render that differs"
  | Clean -> "the same render"

let calibrate () =
  let bad = ref 0 in
  Fmt.pr "%s: calibration over %d control(s)@." harness (List.length controls);
  List.iter
    (fun k ->
      let answer = ask ~css:k.kcss { lname = "control"; lcss = k.kleg } in
      match (answer, k.kwant) with
      | Same, Clean | Differs _, Finding ->
          Fmt.pr "  ok       %s: %s@." k.kname (describe k.kwant)
      | Same, Finding ->
          incr bad;
          Fmt.pr "  BLIND    %s: expected %s, got %s@." k.kname
            (describe Finding) (describe Clean)
      | Differs report, Clean ->
          incr bad;
          Fmt.pr "  BLIND    %s: expected %s, got %s@." k.kname (describe Clean)
            (describe Finding);
          show_report report)
    controls;
  if !bad > 0 then begin
    Fmt.pr "@.%s: %d control(s) did not behave; the check is not trustworthy@."
      harness !bad;
    exit 1
  end;
  Fmt.pr "%s: every control behaved@." harness

(* ===== Reporting ===== *)

let regenerate css =
  String.concat ""
    [ "  dune exec test/render/custom_property.exe -- --css "; shell_quote css ]

type finding = { job : job; leg : leg; report : Browser_compare.t }

let show_finding f =
  Fmt.pr "@.[%s] %s: %s renders differently from the input@." f.job.family
    f.job.id f.leg.lname;
  Fmt.pr "  input:     %s@." (one_line f.job.css);
  Fmt.pr "  %-11s%s@." f.leg.lname (one_line f.leg.lcss);
  show_report f.report;
  Fmt.pr "%s@." (regenerate f.job.css)

let usage () =
  print_endline
    (String.concat "\n"
       [
         "custom_property [--full] [--sample N] [--seed N] [--css TEXT] \
          [--only ID] [--calibrate] [--count]";
         "";
         "  --full       every shape of every value and consumer";
         "  --sample N   how many of the product to render (default 16)";
         "  --seed N     the generator's seed (default 1)";
         "  --css T      report on one stylesheet and stop";
         "  --only ID    run the corpus jobs whose id starts with ID";
         "  --calibrate  run the controls and stop";
         "  --count      print the corpus size and stop, without a browser";
       ])

type options = {
  mutable full : bool;
  mutable sample : int;
  mutable only : string option;
  mutable literal : string option;
  mutable count_only : bool;
  mutable calibrate_only : bool;
}

let parse_args () =
  let o =
    {
      full = false;
      sample = 16;
      only = None;
      literal = None;
      count_only = false;
      calibrate_only = false;
    }
  in
  let rec args = function
    | [] -> ()
    | "--full" :: rest ->
        o.full <- true;
        args rest
    | "--sample" :: n :: rest ->
        o.sample <- int_of_string n;
        args rest
    | "--count" :: rest ->
        o.count_only <- true;
        args rest
    | "--calibrate" :: rest ->
        o.calibrate_only <- true;
        args rest
    | "--seed" :: n :: rest ->
        rand_state := int_of_string n;
        args rest
    | "--css" :: t :: rest ->
        o.literal <- Some t;
        args rest
    | "--only" :: id :: rest ->
        o.only <- Some id;
        args rest
    | ("--help" | "-help") :: _ ->
        usage ();
        exit 0
    | _ :: rest -> args rest
  in
  args (List.tl (Array.to_list Sys.argv));
  o

(* --- one sheet, reported in full --- *)
let report_literal css =
  let job = { id = "literal"; family = "literal"; css; root_customs = None } in
  let ls = legs job in
  Fmt.pr "input:   %s@." (one_line ~limit:400 css);
  List.iter
    (fun l -> Fmt.pr "%-12s %s@." l.lname (one_line ~limit:400 l.lcss))
    ls;
  let findings =
    List.filter_map
      (fun leg ->
        match ask ~css leg with
        | Same -> None
        | Differs report -> Some { job; leg; report })
      (distinct_legs ~css ls)
  in
  List.iter show_finding findings;
  if is_empty findings then
    Fmt.pr "every transform painted what the input painted@.";
  exit (if is_empty findings then 0 else 1)

(* ===== Entry point ===== *)

let jobs_of o =
  let seed = !rand_state in
  let all = corpus ~sample:(if o.full then List.length product else o.sample) in
  let jobs =
    match o.only with
    | None -> all
    | Some prefix -> List.filter (fun j -> String.starts_with ~prefix j.id) all
  in
  (jobs, seed)

(* How often each transform moved the sheet away from what the author wrote,
   which is the reference every leg is compared against. A leg that never moved
   is a leg this corpus does not check, whatever its finding count. *)
let check_moved ~all_legs asked =
  let moved = Hashtbl.create 8 in
  List.iter
    (fun j ->
      List.iter
        (fun l ->
          if not (String.equal l.lcss j.css) then
            Hashtbl.replace moved l.lname
              (1 + Option.value ~default:0 (Hashtbl.find_opt moved l.lname)))
        (Hashtbl.find all_legs j.id))
    asked;
  Fmt.pr "%s: sheets each transform rewrote:" harness;
  List.iter
    (fun name ->
      Fmt.pr " %s=%d" name
        (Option.value ~default:0 (Hashtbl.find_opt moved name)))
    transforms;
  Fmt.pr "@.";
  List.iter
    (fun name ->
      if Option.value ~default:0 (Hashtbl.find_opt moved name) = 0 then
        fail
          (String.concat ""
             [
               "the ";
               name;
               " transform rewrote no sheet in this corpus, so nothing checked \
                it";
             ]))
    transforms

(* Every job's distinct legs through the browser, with what differed. *)
let render ~all_legs asked =
  let pairs = ref 0 in
  let findings =
    List.concat_map
      (fun job ->
        List.filter_map
          (fun leg ->
            incr pairs;
            match ask ~css:job.css leg with
            | Same -> None
            | Differs report -> Some { job; leg; report })
          (distinct_legs ~css:job.css (Hashtbl.find all_legs job.id)))
      asked
  in
  (findings, !pairs)

let report_findings ~pairs findings =
  Fmt.pr "@.%s: %d pair(s) render differently out of %d@." harness
    (List.length findings) pairs;
  Fmt.pr "%s: differences per transform:" harness;
  List.iter
    (fun name ->
      Fmt.pr " %s=%d" name
        (List.length
           (List.filter (fun f -> String.equal f.leg.lname name) findings)))
    transforms;
  Fmt.pr "@.";
  List.iter show_finding findings

let sweep o jobs ~seed =
  if is_empty jobs then fail "no job matched";
  (* A run that exercised almost nothing is not a green run. *)
  if Option.is_none o.only && (not o.calibrate_only) && List.length jobs < 40
  then
    fail
      (String.concat ""
         [
           "the corpus collapsed to ";
           string_of_int (List.length jobs);
           " job(s); it should hold dozens";
         ]);
  calibrate ();
  if o.calibrate_only then exit 0;
  let all_legs = Hashtbl.create (List.length jobs) in
  List.iter (fun j -> Hashtbl.replace all_legs j.id (legs j)) jobs;
  let unreadable =
    List.filter (fun j -> is_empty (Hashtbl.find all_legs j.id)) jobs
  in
  let asked =
    List.filter (fun j -> not (is_empty (Hashtbl.find all_legs j.id))) jobs
  in
  let started = Unix.gettimeofday () in
  let findings, pairs = render ~all_legs asked in
  let elapsed = Unix.gettimeofday () -. started in
  Fmt.pr "%s: %d job(s), %d unreadable, %d pair(s), seed %d, %.1fs@." harness
    (List.length asked) (List.length unreadable) pairs seed elapsed;
  check_moved ~all_legs asked;
  if is_empty findings then
    Fmt.pr "%s: every transform painted what the input painted@." harness
  else (
    report_findings ~pairs findings;
    exit 1)

let () =
  let o = parse_args () in
  let jobs, seed = jobs_of o in
  if o.count_only then begin
    Fmt.pr "%s: %d job(s)@." harness (List.length jobs);
    exit 0
  end;
  Browser.suppressed harness;
  (match Browser.node_binary () with
  | Some _ -> ()
  | None -> Browser.skip harness "no node");
  (match Browser.chrome_binary () with
  | Some _ -> ()
  | None -> Browser.skip harness "no headless browser");
  match o.literal with
  | Some css -> report_literal css
  | None -> sweep o jobs ~seed
