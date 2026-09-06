(* Differential custom-property substitution: a transform that resolves [var()]
   must not change what the browser computes.

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

   Sec. 4.1 forbids the other half of this outright. A custom property's value
   is serialised "exactly as specified by the author", and "simplifications that
   might occur in other properties, such as dropping comments, normalizing
   whitespace, reserializing numeric tokens from their value, etc., must not
   occur".

   The oracle is the browser. Every variant of a job is computed in the same
   frame, in the same document, so a difference is the transform's and not a
   difference of spelling: a value the browser normalises it normalises on both
   sides. The reference variant is the input text as the author wrote it, so a
   green run says the transform preserved the page, not that two transforms
   agree with each other.

   This harness samples custom properties by name. [render_diff] and
   [resolve_diff] both drop every [--] property from their comparison, on the
   ground that a custom property reaches the render only through a [var()] in a
   real property; that holds only where a real property does read it, and it is
   exactly the value a substitution bug corrupts.

   Skips cleanly, with status 0, when node or a headless Chromium is missing.
   CASCADE_NO_BROWSER fails instead: see [Browser.suppressed]. *)

open Cascade

let ( // ) = Filename.concat
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

(* Three nested elements under body, so inheritance has somewhere to travel and
   a shadowing definition has somewhere to sit. [position] is declared here
   rather than in a job, so [z-index] computes to something other than [auto] in
   every variant alike. *)
let document =
  "<div id=\"o\" class=\"o\"><div id=\"m\" class=\"m\"><div id=\"i\" \
   class=\"i\"></div></div></div>"

let base_rules = "#i{position:relative}"

(* Selectors the frame has to resolve, and the count each one must return. A
   NODE that does not report ids, or a body the harness built wrong, answers
   every comparison out of a document nobody wrote. *)
let adapter_probes =
  [ ("#o", 1); ("#m", 1); ("#i", 1); (".i", 1); ("#o #i", 1); ("#m>#i", 1) ]

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
  cdecl : string; (* reads var(--x) *)
  cprev : string; (* a literal declaration of the same property *)
}

(* One consumer per shape a substitution can land in: a whole value, a value
   with a fallback, a component of a list, an argument of a function, a
   shorthand, a nested fallback, and a custom property. [cprev] is the same
   property spelled literally, so a shape can put a live declaration under the
   var() one and turn "both sides end up invalid" into an observable
   difference. *)
let consumers =
  [
    { ctag = "width"; cdecl = "width:var(--x)"; cprev = "width:37px" };
    { ctag = "widthfb"; cdecl = "width:var(--x,9px)"; cprev = "width:37px" };
    { ctag = "glue"; cdecl = "width:var(--x)px"; cprev = "width:37px" };
    { ctag = "colour"; cdecl = "color:var(--x)"; cprev = "color:rgb(1,2,3)" };
    {
      ctag = "colourfb";
      cdecl = "color:var(--x,lime)";
      cprev = "color:rgb(1,2,3)";
    };
    { ctag = "margin"; cdecl = "margin:var(--x) 4px"; cprev = "margin:11px" };
    { ctag = "calc"; cdecl = "width:calc(var(--x)*2)"; cprev = "width:37px" };
    {
      ctag = "transform";
      cdecl = "transform:var(--x)";
      cprev = "transform:translateY(5px)";
    };
    {
      ctag = "border";
      cdecl = "border:var(--x) solid red";
      cprev = "border:9px dotted blue";
    };
    {
      ctag = "font";
      cdecl = "font-family:var(--x)";
      cprev = "font-family:Courier";
    };
    { ctag = "content"; cdecl = "content:var(--x)"; cprev = "content:\"zz\"" };
    {
      ctag = "nestfb";
      cdecl = "width:var(--nope,var(--x,8px))";
      cprev = "width:37px";
    };
    {
      ctag = "image";
      cdecl = "background-image:var(--x)";
      cprev = "background-image:linear-gradient(blue,blue)";
    };
    { ctag = "zindex"; cdecl = "z-index:var(--x)"; cprev = "z-index:7" };
    { ctag = "custom"; cdecl = "--y:var(--x)"; cprev = "--y:zz" };
    {
      ctag = "outline";
      cdecl = "outline-color:var(--x)";
      cprev = "outline-color:rgb(4,5,6)";
    };
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
  let read = String.concat "" [ base_rules; "#i{"; consumer.cdecl; "}" ] in
  let read_prev =
    String.concat ""
      [ base_rules; "#i{"; consumer.cprev; ";"; consumer.cdecl; "}" ]
  in
  let root_only = Some [ ("--n", "5px"); ("--x", value) ] in
  match shape with
  | "root" -> (String.concat "" [ ":root{"; defs; "}"; read ], root_only)
  | "same" ->
      ( String.concat "" [ base_rules; "#i{"; defs; ";"; consumer.cdecl; "}" ],
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
            "#i{";
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
       k{from{opacity:0}to{opacity:1}}:root{--k:k}#i{position:relative;animation-name:var(--k)}"
    );
    ("all-shorthand", ":root{--x:inherit}#o{color:rgb(1,2,3)}#i{all:var(--x)}");
  ]

let corpus ~sample =
  let jobs = ref [] in
  let add id family css root_customs =
    jobs := { id; family; css; root_customs } :: !jobs
  in
  List.iter
    (fun (vtag, value) ->
      List.iter
        (fun consumer ->
          (* A seeded subset of the shapes, so a default run is a slice of the
             product and [--full] is the whole of it. *)
          let picked =
            if sample >= List.length shapes then shapes
            else
              let chosen = ref [] in
              while List.length !chosen < sample do
                let s = List.nth shapes (next_rand (List.length shapes)) in
                if not (List.exists (String.equal s) !chosen) then
                  chosen := s :: !chosen
              done;
              List.rev !chosen
          in
          List.iter
            (fun shape ->
              let css, root_customs = build_shape shape value consumer in
              add
                (String.concat "/" [ shape; vtag; consumer.ctag ])
                shape css root_customs)
            picked)
        consumers)
    values;
  List.iter
    (fun (name, css) ->
      add (String.concat "" [ "curated/"; name ]) "curated" css None)
    curated;
  List.rev !jobs

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

(* The custom-property names a comparison has to ask for by name. Read off the
   text rather than out of the parser: a harness that asked cascade which names
   matter would share an error with the code it is checking, and a name the
   reader lost is exactly the kind of thing to catch. *)
let is_name_byte c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || Char.equal c '-' || Char.equal c '_'

let custom_names css =
  let n = String.length css in
  let acc = ref [] in
  let i = ref 0 in
  while !i < n - 1 do
    if Char.equal css.[!i] '-' && Char.equal css.[!i + 1] '-' then begin
      let stop = ref (!i + 2) in
      while !stop < n && is_name_byte css.[!stop] do
        incr stop
      done;
      if !stop > !i + 2 then acc := String.sub css !i (!stop - !i) :: !acc;
      i := !stop
    end
    else incr i
  done;
  List.sort_uniq String.compare !acc

(* ===== The browser's answers ===== *)

let read_lines ic =
  let rec loop acc =
    match input_line ic with
    | line -> loop (line :: acc)
    | exception End_of_file -> List.rev acc
  in
  loop []

let run_driver ~node ~chrome ~script ~jobs ~work =
  let errors = work // "custom-property.err" in
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

type diff = {
  leg : string;
  element : string;
  property : string;
  reference : string;
  variant : string;
}

(* [--inline-vars] is documented to "drop the now-unused custom property
   definitions" once it has substituted them (bin/cmd_fmt.ml, and
   [Inline.vars]), so a binding that is gone from the computed style of a leg
   that advertises removing it is that contract, not a disagreement. Anything
   else is: a binding that changed to another value, a binding that appeared, a
   binding removed by a transform that never claimed to, and every difference on
   a property the browser actually renders. The distinction is what the value
   became, not which property it was, so a substitution bug that empties a
   binding still has to corrupt nothing else to land here. *)
let removes_definitions leg =
  String.equal leg "inline-vars" || String.equal leg "optimize"

let is_custom d = String.starts_with ~prefix:"--" d.property

let dropped_definition d =
  is_custom d && removes_definitions d.leg
  && String.equal (String.trim d.variant) ""
  && not (String.equal (String.trim d.reference) "")

(* CSS Syntax 3 (ED) sec. 10 asks a serialization to round-trip with parsing
   "except for consecutive <whitespace-token>s, which may be collapsed into a
   single token", since "CSS grammars always interpret any amount of whitespace
   as identical to a single space". A custom property's computed value is the
   token stream it was written with, so two readings that differ only in
   whitespace no grammar can see substitute alike wherever the binding is used.
   cascade's own custom-value printer drops a space only where the token
   sequence is unchanged, so running both readings through it is what decides:
   [a b] and [a b] agree, [a b] and [ab] do not. *)
let same_token_stream a b =
  let canonical v =
    match Css.Declaration.custom_property "--x" v with
    | d -> Some (Css.Declaration.to_string ~minify:true d)
    | exception Failure _ -> None
  in
  match (canonical a, canonical b) with
  | Some x, Some y -> String.equal x y
  | Some _, None | None, Some _ | None, None -> false

let same_but_for_whitespace d =
  is_custom d && same_token_stream d.reference d.variant

let disagreement d =
  (not (dropped_definition d)) && not (same_but_for_whitespace d)

type answer = {
  diffs : diff list;
  elements : int;
  properties : int;
  variants : int;
  adapter : (string * int) list;
}

let empty_answer =
  { diffs = []; elements = 0; properties = 0; variants = 0; adapter = [] }

(* One driver round: every job's document, names and variants, read back into a
   table. Two legs that print the same text are one variant, so a transform that
   did nothing costs the browser nothing; a difference the two share is then
   filed under the earlier name, which understates the later one's count without
   losing the finding. *)
let ask ~node ~chrome ~script ~work jobs =
  let file = work // "jobs.json" in
  let payload =
    Json.Arr
      (List.map
         (fun (id, css, legs) ->
           let seen = Hashtbl.create 8 in
           let distinct =
             List.filter
               (fun l ->
                 if Hashtbl.mem seen l.lcss then false
                 else begin
                   Hashtbl.replace seen l.lcss ();
                   true
                 end)
               legs
           in
           Json.Obj
             [
               ("id", Json.Str id);
               ("doc", Json.Str document);
               ( "names",
                 Json.Arr
                   (List.map
                      (fun n -> Json.Str n)
                      (custom_names
                         (String.concat " "
                            (css :: List.map (fun l -> l.lcss) legs)))) );
               ( "adapter",
                 Json.Arr (List.map (fun (s, _) -> Json.Str s) adapter_probes)
               );
               ( "sheets",
                 Json.Arr
                   (Json.Obj
                      [ ("name", Json.Str "input"); ("css", Json.Str css) ]
                   :: List.map
                        (fun l ->
                          Json.Obj
                            [
                              ("name", Json.Str l.lname);
                              ("css", Json.Str l.lcss);
                            ])
                        distinct) );
             ])
         jobs)
  in
  let oc = open_out file in
  output_string oc (Json.to_string payload);
  close_out oc;
  match run_driver ~node ~chrome ~script ~jobs:file ~work with
  | Error err -> fail (String.concat "" [ "the browser driver failed:\n"; err ])
  | Ok lines ->
      let table = Hashtbl.create (List.length jobs) in
      let get id =
        match Hashtbl.find_opt table id with
        | Some a -> a
        | None ->
            Hashtbl.replace table id empty_answer;
            empty_answer
      in
      let broke = ref [] in
      List.iter
        (fun line ->
          match String.split_on_char '\t' line with
          | [ "d"; id; leg; element; property; reference; variant ] ->
              let a = get id in
              Hashtbl.replace table id
                {
                  a with
                  diffs =
                    { leg; element; property; reference; variant } :: a.diffs;
                }
          | [ "n"; id; els; props; variants; _ ] ->
              let a = get id in
              Hashtbl.replace table id
                {
                  a with
                  elements = int_of_string els;
                  properties = int_of_string props;
                  variants = int_of_string variants;
                }
          | [ "a"; id; selector; count ] ->
              let a = get id in
              Hashtbl.replace table id
                {
                  a with
                  adapter = (selector, int_of_string count) :: a.adapter;
                }
          | "x" :: id :: rest ->
              broke := (id, String.concat "\t" rest) :: !broke
          | _ -> ())
        lines;
      List.iter
        (fun (id, message) ->
          prerr_endline
            (String.concat ""
               [ harness; ": the page failed on "; id; ": "; message ]))
        !broke;
      if not (is_empty !broke) then exit 1;
      Hashtbl.iter
        (fun id a ->
          Hashtbl.replace table id { a with diffs = List.rev a.diffs })
        (Hashtbl.copy table);
      List.iter
        (fun (id, _, _) ->
          if not (Hashtbl.mem table id) then
            fail (String.concat "" [ "the browser said nothing for "; id ]))
        jobs;
      table

(* ===== The verdict for one job ===== *)

(* What makes two findings the same finding: which transform moved which
   properties, not the element it moved them on or the values involved. A
   reduction has to keep this exact signature, or it is a repro of something
   else. *)
let signature diffs =
  String.concat ";"
    (List.sort_uniq String.compare
       (List.map (fun d -> String.concat "|" [ d.leg; d.property ]) diffs))

(* ===== Reduction ===== *)

(* Candidate reductions of a sheet: drop one declaration, or drop one rule. A
   job is a handful of declarations to start with, so the two moves that matter
   are the two the corpus is built out of. *)
let split_on_top_level css =
  let n = String.length css in
  let out = ref [] in
  let depth = ref 0 in
  let start = ref 0 in
  String.iteri
    (fun i c ->
      if Char.equal c '{' then incr depth
      else if Char.equal c '}' then begin
        decr depth;
        if !depth = 0 then begin
          out := String.sub css !start (i - !start + 1) :: !out;
          start := i + 1
        end
      end
      else if Char.equal c ';' && !depth = 0 then begin
        out := String.sub css !start (i - !start + 1) :: !out;
        start := i + 1
      end)
    css;
  if !start < n then out := String.sub css !start (n - !start) :: !out;
  List.rev !out

let drop_nth l k = List.filteri (fun i _ -> not (Int.equal i k)) l

let declaration_drops chunk =
  match (String.index_opt chunk '{', String.rindex_opt chunk '}') with
  | Some o, Some c when c > o ->
      let head = String.sub chunk 0 (o + 1) in
      let body = String.sub chunk (o + 1) (c - o - 1) in
      let tail = String.sub chunk c (String.length chunk - c) in
      let decls = String.split_on_char ';' body in
      List.filteri (fun i _ -> i < List.length decls) decls
      |> List.mapi (fun i _ ->
          String.concat "" [ head; String.concat ";" (drop_nth decls i); tail ])
  | _, _ -> []

let reductions css =
  let chunks = split_on_top_level css in
  let rule_drops =
    List.mapi (fun i _ -> String.concat "" (drop_nth chunks i)) chunks
  in
  let decl_drops =
    List.concat
      (List.mapi
         (fun i chunk ->
           List.map
             (fun replacement ->
               String.concat ""
                 (List.mapi
                    (fun j c -> if Int.equal i j then replacement else c)
                    chunks))
             (declaration_drops chunk))
         chunks)
  in
  List.sort_uniq String.compare
    (List.filter
       (fun c ->
         (not (String.equal c "")) && String.length c < String.length css)
       (rule_drops @ decl_drops))

type finding = { job : job; seen : diff list; small : string; alike : int }

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
            let probe = { f.job with css = candidate } in
            let ls = legs probe in
            if not (is_empty ls) then
              jobs :=
                ( String.concat ""
                    [ "m/"; string_of_int k; "/"; string_of_int j ],
                  candidate,
                  ls )
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
                | Some a ->
                    let reported = List.filter disagreement a.diffs in
                    if
                      (not (is_empty reported))
                      && String.equal (signature reported) (signature f.seen)
                    then begin
                      current.(k) <- { f with small = candidate };
                      improved := true
                    end)
          | _ -> ())
        jobs
    end
  done;
  (Array.to_list current, !round)

(* ===== Calibration ===== *)

(* Legs the harness writes itself, so the check is exercised by something that
   does not move when cascade does. A wrong substitution has to be caught and a
   spelling that means the same has to pass; a harness that fails either way
   answers nothing about the transforms it is pointed at. *)
type expectation = Finding | Clean | Removed

let equal_expectation a b =
  match (a, b) with
  | Finding, Finding | Clean, Clean | Removed, Removed -> true
  | (Finding | Clean | Removed), _ -> false

type control = {
  kname : string;
  kcss : string;
  kleg : string;
  kleg_name : string;
  kwant : expectation;
}

let controls =
  [
    (* Reported: the substituted value is not the one the definition holds. *)
    {
      kname = "wrong-length";
      kcss = ":root{--x:1px}#i{position:relative;width:var(--x)}";
      kleg = ":root{--x:1px}#i{position:relative;width:2px}";
      kleg_name = "inline-vars";
      kwant = Finding;
    };
    {
      kname = "wrong-colour";
      kcss = ":root{--x:red}#i{color:var(--x)}";
      kleg = ":root{--x:red}#i{color:blue}";
      kleg_name = "inline-vars";
      kwant = Finding;
    };
    (* Reported: the definition is deleted while a reader still needs it, which
       is the one deletion [--inline-vars] does not licence. *)
    {
      kname = "definition-still-read";
      kcss = ":root{--x:red}#m{color:var(--x)}";
      kleg = "#m{color:var(--x)}";
      kleg_name = "inline-vars";
      kwant = Finding;
    };
    (* Reported: the custom property itself is corrupted, and no real property
       reads it. This is the case the other harnesses here filter away. *)
    {
      kname = "custom-only";
      kcss = "#i{--x:1px}";
      kleg = "#i{--x:2px}";
      kleg_name = "inline-vars";
      kwant = Finding;
    };
    (* Reported: an invalid-at-computed-value-time declaration is dropped
       instead, so the declaration under it wins. *)
    {
      kname = "iacvt-vs-dropped";
      kcss = "#i{position:relative;width:37px;width:var(--nothing)}";
      kleg = "#i{position:relative;width:37px}";
      kleg_name = "inline-vars";
      kwant = Finding;
    };
    (* Reported: a transform that never claimed to drop a binding dropped one.
       Only the two closed-world legs are licensed to. *)
    {
      kname = "definition-dropped-by-printer";
      kcss = ":root{--unused:1px}#i{position:relative;width:2px}";
      kleg = "#i{position:relative;width:2px}";
      kleg_name = "minify";
      kwant = Finding;
    };
    (* Passed: a different spelling of the same computed value. *)
    {
      kname = "same-colour";
      kcss = ":root{--x:#f00}#i{color:var(--x)}";
      kleg = ":root{--x:#f00}#i{color:rgb(255,0,0)}";
      kleg_name = "inline-vars";
      kwant = Clean;
    };
    {
      kname = "same-length";
      kcss = ":root{--x:1px}#i{position:relative;width:var(--x)}";
      kleg = ":root{--x:1px}#i{position:relative;width:1px}";
      kleg_name = "inline-vars";
      kwant = Clean;
    };
    (* Passed: both sides are invalid at computed-value time, by different
       routes, and the property takes the unset value either way. *)
    {
      kname = "both-invalid";
      kcss = "#i{position:relative;--x:1p;width:var(--x)}";
      kleg = "#i{position:relative;--x:1p;width:var(--nothing)}";
      kleg_name = "inline-vars";
      kwant = Clean;
    };
    (* Removed: the substitution happened and the binding went with it, which is
       what [--inline-vars] says it does. *)
    {
      kname = "definition-removed";
      kcss = ":root{--x:1px}#i{position:relative;width:var(--x)}";
      kleg = "#i{position:relative;width:1px}";
      kleg_name = "inline-vars";
      kwant = Removed;
    };
  ]

let describe = function
  | Finding -> "a disagreement"
  | Clean -> "no difference at all"
  | Removed -> "a removed binding and nothing else"

let observed a =
  let reported = List.filter disagreement a.diffs in
  if not (is_empty reported) then Finding
  else if is_empty a.diffs then Clean
  else Removed

let adapter_ok a =
  List.for_all
    (fun (sel, want) ->
      match List.assoc_opt sel a.adapter with
      | Some got -> Int.equal got want
      | None -> false)
    adapter_probes

let show_adapter a =
  List.iter
    (fun (sel, want) ->
      let got = Option.value ~default:(-1) (List.assoc_opt sel a.adapter) in
      if not (Int.equal got want) then
        Fmt.pr "      %s matched %d element(s), expected %d@." sel got want)
    adapter_probes

let calibrate ~node ~chrome ~script ~work =
  let jobs =
    List.map
      (fun k ->
        ( String.concat "" [ "k/"; k.kname ],
          k.kcss,
          [ { lname = k.kleg_name; lcss = k.kleg } ] ))
      controls
  in
  let answers = ask ~node ~chrome ~script ~work jobs in
  let bad = ref 0 in
  Fmt.pr "%s: calibration over %d control(s)@." harness (List.length controls);
  List.iter
    (fun k ->
      let a =
        match Hashtbl.find_opt answers (String.concat "" [ "k/"; k.kname ]) with
        | Some a -> a
        | None -> fail (String.concat "" [ "no answer for "; k.kname ])
      in
      if not (adapter_ok a) then begin
        incr bad;
        Fmt.pr "  ADAPTER  %s: the frame did not resolve the document@." k.kname;
        show_adapter a
      end;
      let got = observed a in
      if equal_expectation got k.kwant then
        Fmt.pr "  ok       %s: %s, over %d differing value(s)@." k.kname
          (describe k.kwant) (List.length a.diffs)
      else begin
        incr bad;
        Fmt.pr "  BLIND    %s: expected %s, got %s@." k.kname (describe k.kwant)
          (describe got);
        List.iteri
          (fun i d ->
            if i < 4 then
              Fmt.pr "      %s %s: %s vs %s@." d.element d.property d.reference
                d.variant)
          a.diffs
      end)
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

let show_diffs diffs =
  List.iteri
    (fun i d ->
      if i < 6 then
        Fmt.pr "      [%s] %s %s: browser %s, cascade %s@." d.leg d.element
          d.property
          (one_line ~limit:60 d.reference)
          (one_line ~limit:60 d.variant))
    diffs;
  if List.length diffs > 6 then
    Fmt.pr "      ... %d more@." (List.length diffs - 6)

let usage () =
  print_endline
    (String.concat "\n"
       [
         "custom_property [--full] [--seed N] [--css TEXT] [--only ID] \
          [--calibrate] [--count]";
         "";
         "  --full       every shape of every value and consumer";
         "  --seed N     the generator's seed (default 1)";
         "  --css T      report on one stylesheet and stop";
         "  --only ID    run the corpus jobs whose id starts with ID";
         "  --calibrate  run the controls and stop";
         "  --count      print the corpus size and stop, without a browser";
       ])

(* ===== Entry point ===== *)

let () =
  let full = ref false in
  let only = ref None in
  let corpus_seed = ref !rand_state in
  let literal = ref None in
  let count_only = ref false in
  let calibrate_only = ref false in
  let rec args = function
    | [] -> ()
    | "--full" :: rest ->
        full := true;
        args rest
    | "--count" :: rest ->
        count_only := true;
        args rest
    | "--calibrate" :: rest ->
        calibrate_only := true;
        args rest
    | "--seed" :: n :: rest ->
        rand_state := int_of_string n;
        corpus_seed := !rand_state;
        args rest
    | "--css" :: t :: rest ->
        literal := Some t;
        args rest
    | "--only" :: id :: rest ->
        only := Some id;
        args rest
    | ("--help" | "-help") :: _ ->
        usage ();
        exit 0
    | _ :: rest -> args rest
  in
  args (List.tl (Array.to_list Sys.argv));

  let jobs =
    match !literal with
    | Some css ->
        [ { id = "literal"; family = "literal"; css; root_customs = None } ]
    | None -> (
        let all = corpus ~sample:(if !full then List.length shapes else 2) in
        match !only with
        | None -> all
        | Some prefix ->
            List.filter (fun j -> String.starts_with ~prefix j.id) all)
  in
  if !count_only then begin
    Fmt.pr "%s: %d job(s)@." harness (List.length jobs);
    exit 0
  end;
  if is_empty jobs then fail "no job matched";
  (* A run that exercised almost nothing is not a green run. *)
  if
    Option.is_none !literal && Option.is_none !only && (not !calibrate_only)
    && List.length jobs < 500
  then
    fail
      (String.concat ""
         [
           "the corpus collapsed to ";
           string_of_int (List.length jobs);
           " job(s); it should hold hundreds";
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
  let script = Filename.dirname Sys.executable_name // "custom_property.js" in
  let work = Filename.get_temp_dir_name () // "cascade-custom-property" in
  (try Unix.mkdir work 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());

  calibrate ~node ~chrome ~script ~work;
  if !calibrate_only then exit 0;

  let all_legs = Hashtbl.create (List.length jobs) in
  List.iter (fun j -> Hashtbl.replace all_legs j.id (legs j)) jobs;
  let unreadable =
    List.filter (fun j -> is_empty (Hashtbl.find all_legs j.id)) jobs
  in
  let asked =
    List.filter (fun j -> not (is_empty (Hashtbl.find all_legs j.id))) jobs
  in
  let started = Unix.gettimeofday () in
  let answers =
    ask ~node ~chrome ~script ~work
      (List.map (fun j -> (j.id, j.css, Hashtbl.find all_legs j.id)) asked)
  in
  let elapsed = Unix.gettimeofday () -. started in

  (* --- one sheet, reported in full --- *)
  (match !literal with
  | Some css ->
      let a = Hashtbl.find answers "literal" in
      Fmt.pr "input:   %s@." (one_line ~limit:400 css);
      List.iter
        (fun l -> Fmt.pr "%-12s %s@." l.lname (one_line ~limit:400 l.lcss))
        (Hashtbl.find all_legs "literal");
      Fmt.pr "sampled: %d element(s), %d propert(y|ies), %d variant(s)@."
        a.elements a.properties a.variants;
      let reported = List.filter disagreement a.diffs in
      let gone = List.filter dropped_definition a.diffs in
      if not (is_empty gone) then
        Fmt.pr "bindings a closed-world leg removed: %d@." (List.length gone);
      show_diffs reported;
      exit (if is_empty reported then 0 else 1)
  | None -> ());

  (* --- the sweep --- *)
  (* A browser that answered out of a document nobody wrote, or that read
     nothing, would make every comparison pass. *)
  let adapter_broken =
    List.filter (fun j -> not (adapter_ok (Hashtbl.find answers j.id))) asked
  in
  if not (is_empty adapter_broken) then
    fail
      (String.concat ""
         [
           string_of_int (List.length adapter_broken);
           " job(s) ran against a document the frame did not build";
         ]);
  let comparisons =
    List.fold_left
      (fun acc j ->
        let a = Hashtbl.find answers j.id in
        acc + (a.elements * a.properties * a.variants))
      0 asked
  in
  if comparisons < 1000 then
    fail
      (String.concat ""
         [
           "only ";
           string_of_int comparisons;
           " comparison(s); the oracle read nothing";
         ]);

  (* How often each transform moved the sheet away from what the author wrote,
     which is the reference every leg is compared against. A leg that never
     moved is a leg this corpus does not check, whatever its finding count. *)
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

  let findings = ref [] in
  let removed = ref 0 in
  let by_leg = Hashtbl.create 8 in
  List.iter
    (fun j ->
      let a = Hashtbl.find answers j.id in
      let reported = List.filter disagreement a.diffs in
      if List.exists dropped_definition a.diffs then incr removed;
      if not (is_empty reported) then begin
        List.iter
          (fun d ->
            Hashtbl.replace by_leg d.leg
              (1 + Option.value ~default:0 (Hashtbl.find_opt by_leg d.leg)))
          reported;
        findings :=
          { job = j; seen = reported; small = j.css; alike = 1 } :: !findings
      end)
    asked;
  let findings = List.rev !findings in

  Fmt.pr "%s: %d job(s), %d unreadable, %d comparison(s), seed %d, %.1fs@."
    harness (List.length asked) (List.length unreadable) comparisons
    !corpus_seed elapsed;
  Fmt.pr "%s: sheets each transform rewrote:" harness;
  List.iter
    (fun name ->
      Fmt.pr " %s=%d" name
        (Option.value ~default:0 (Hashtbl.find_opt moved name)))
    [ "minify"; "inline-vars"; "optimize"; "eval" ];
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
    [ "minify"; "inline-vars"; "optimize"; "eval" ];

  Fmt.pr
    "%s: %d job(s) where a closed-world leg removed a binding it had \
     substituted@."
    harness !removed;

  if is_empty findings then begin
    Fmt.pr "%s: every transform computed what the browser computed@." harness;
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
  let reduced, rounds = minimise ~node ~chrome ~script ~work ~rounds:12 reps in

  Fmt.pr
    "@.%s: %d job(s) disagree out of %d; %d distinct symptom(s), reduced in %d \
     round(s)@."
    harness (List.length findings) (List.length asked) (List.length reduced)
    rounds;
  Fmt.pr "%s: differences per transform:" harness;
  List.iter
    (fun name ->
      Fmt.pr " %s=%d" name
        (Option.value ~default:0 (Hashtbl.find_opt by_leg name)))
    [ "minify"; "inline-vars"; "optimize"; "eval" ];
  Fmt.pr "@.";

  let show section keep =
    let listed = List.filter (fun f -> List.exists keep f.seen) reduced in
    if not (is_empty listed) then begin
      Fmt.pr "@.=== %s (%d) ===@." section (List.length listed);
      List.iter
        (fun f ->
          Fmt.pr "@.[%s] %s, %d job(s) in the corpus@." f.job.family f.job.id
            f.alike;
          Fmt.pr "  minimised: %s@." (one_line f.small);
          if not (String.equal f.small f.job.css) then
            Fmt.pr "  from:      %s@." (one_line f.job.css);
          List.iter
            (fun l -> Fmt.pr "  %-11s%s@." l.lname (one_line l.lcss))
            (legs { f.job with css = f.small });
          show_diffs f.seen;
          Fmt.pr "%s@." (regenerate f.small))
        listed
    end
  in
  show "SUBSTITUTION: inline-vars computed something else" (fun d ->
      String.equal d.leg "inline-vars");
  show "EVALUATION: eval computed something else" (fun d ->
      String.equal d.leg "eval");
  show "OPTIMIZER: optimize computed something else" (fun d ->
      String.equal d.leg "optimize");
  show "ROUND TRIP: the reader and the printer alone moved the sheet" (fun d ->
      String.equal d.leg "minify");
  exit 1
