(** Cascade reading back what it writes.

    Two properties, over every input the repo already ships:

    - {b readable}: re-reading an emission raises no diagnostic the input did
      not already raise. A diagnostic on our own bytes is a construct browsers
      drop too, so the emission is a rendering change, not a shorter spelling.
    - {b stable}: emitting the re-read gives the same bytes. One outside pass is
      the contract, because the optimizer already runs its own fixpoint loop; a
      second pass that moves is a first pass that stopped early.

    The two are independent. A rejected emission usually also moves, because the
    reader drops what it rejects, but an emission can read back as a different
    construct and then sit still: [@scope to (...)] prints as [@scopeto (...)],
    an unknown at-rule that round-trips opaquely for ever.

    Inputs come from corpora other harnesses own: the keithamus minifier
    fixtures (both the sources and the upstream minified answers), the
    Lightning-derived trace inputs, the shipped example sheets, and the
    spec-derived property grammar vectors. Nothing here is generated, and
    nothing here needs regenerating.

    Every output mode the CLI offers is checked, so a defect one mode alone
    reaches is not hidden behind the mode that happens to be clean.

    A third property holds for the optimizing modes: {b serialised}, the
    selectors the optimizer builds are the ones reading its print gives back
    (docs/invariants.md, "pp is a serializer"). Text stability alone misses a
    selector that prints like the reader's shape but is another node: every
    later rewrite that compares selectors reads the two apart, and the pass that
    would have fired only fires once the text is read back. Selectors are
    compared structurally, bar an unknown functional pseudo, which keeps the
    tokens it was read from, and every block reads back with as many statements
    as it was built with. Declarations are not compared: a custom property's
    tokens keep the spelling they were written with, which is part of the value,
    and the minified printer spells some of them shorter.

    A fourth property holds without the optimizer: every typed declaration a
    corpus holds, printed minified, reads back as the declaration it was. The
    printer may pick the spelling, never the node.

    Failures are reduced before they are reported. A candidate is always a
    concatenation of substrings of the original input, never a re-print of its
    parse: re-printing would hide exactly the emission bugs this looks for. A
    reduction is kept only when it still fails the same way, judged by the
    reader diagnostics it introduces and by whether it moves on the second pass.
    Reduction runs under a step budget, so a large input can end up partly
    reduced rather than minimal. *)

open Cascade

(* {1 Corpora} *)

type input = { name : string; css : string }

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

(* Dune runs the test from its own build directory; [dune exec] runs it from the
   project root. Both reach the corpora from one repo-relative path. *)
let locate rel =
  let from_build = Filename.concat ".." (Filename.concat ".." rel) in
  if Sys.file_exists from_build then Some from_build
  else if Sys.file_exists rel then Some rel
  else None

let subdirs path =
  Sys.readdir path |> Array.to_list
  |> List.filter (fun name -> Sys.is_directory (Filename.concat path name))
  |> List.sort String.compare

let minify_tests_inputs () =
  match locate "test/interop/css-minify-tests/traces/tests" with
  | None -> []
  | Some root ->
      subdirs root
      |> List.concat_map (fun category ->
          let cat_dir = Filename.concat root category in
          subdirs cat_dir
          |> List.concat_map (fun id ->
              let dir = Filename.concat cat_dir id in
              [ "source.css"; "expected.css" ]
              |> List.filter_map (fun file ->
                  let path = Filename.concat dir file in
                  if Sys.file_exists path then
                    let name = String.concat "/" [ category; id; file ] in
                    Some { name; css = read_file path }
                  else None)))

let lightning_inputs () =
  match locate "test/interop/lightning/traces/minify.pairs" with
  | None -> []
  | Some path ->
      Trace_pairs.read path
      |> List.mapi (fun i record ->
          {
            name = String.concat "" [ "record "; string_of_int i ];
            css = record.Trace_pairs.input;
          })

let example_inputs () =
  match locate "test/examples" with
  | None -> []
  | Some root ->
      Sys.readdir root |> Array.to_list |> List.sort String.compare
      |> List.filter_map (fun name ->
          if Filename.check_suffix name ".css" then
            Some { name; css = read_file (Filename.concat root name) }
          else None)

(* The manifest vectors are bare property values; each becomes the one
   declaration of a rule. Negatives are inputs too: whatever the reader keeps of
   one still has to survive its own printing. *)
let grammar_inputs () =
  let declaration property value =
    String.concat "" [ "a{"; property; ":"; value; "}" ]
  in
  let vectors kind property values =
    List.mapi
      (fun i value ->
        {
          name = String.concat "" [ property; " "; kind; string_of_int i ];
          css = declaration property value;
        })
      values
  in
  Cascade_spec_inventory.Property_grammar.rows
  |> List.concat_map (fun (row : Cascade_spec_inventory.Property_grammar.row) ->
      vectors "+" row.property row.positives
      @ vectors "-" row.property row.negatives)

(* Two spellings of one value in two rules. The optimizer holds normal forms, so
   the first pass merges the rules; a normal form that is not the node its print
   reads back as leaves them apart, and the second pass merges what the first
   printed alike. *)
let normal_form_inputs () =
  List.mapi
    (fun i css ->
      { name = String.concat "" [ "spellings "; string_of_int i ]; css })
    [
      {|a{background:url("i.png")}b{background:url(i.png)}|};
      {|a{background:url("a b.png")}b{background:url(a\ b.png)}|};
      {|a{list-style:url("x.png")}b{list-style:url(x.png)}|};
      {|a{content:url("x.png")}b{content:url(x.png)}|};
      {|a{mask:url("m.svg")}b{mask:url(m.svg)}|};
      {|a{position-area:center center}b{position-area:center}|};
      {|a{place-items:center center}b{place-items:center}|};
      {|a{place-content:center center}b{place-content:center}|};
      {|a{flex:1 1 0%}b{flex:1}|};
      {|a{flex:2 3 0%}b{flex:2 3}|};
      {|a{rotate:1 0 0 10deg}b{rotate:x 10deg}|};
      {|a{grid-row:span 1 a}b{grid-row:span a}|};
      {|a{list-style-type:symbols(symbolic "*")}b{list-style-type:symbols("*")}|};
      {|a{clip-path:polygon(nonzero,0 0,1px 0,0 1px)}b{clip-path:polygon(0 0,1px 0,0 1px)}|};
      {|a{animation-range:normal normal}b{animation-range:normal}|};
      {|a{animation-timeline:scroll(block nearest)}b{animation-timeline:scroll()}|};
      {|a{color:color(xyz-d65 .1 .2 .3)}b{color:color(xyz .1 .2 .3)}|};
      {|a{color:color(display-p3 50% 0 0)}b{color:color(display-p3 .5 0 0)}|};
      {|a{width:calc(50px - 50px)}b{width:0}|};
      {|a{transform:scale(calc(.1 + .2))}b{transform:scale(.3)}|};
      {|a{foo:a , b}b{foo:a,b}|};
      {|a{width:33.33333333333333%}b{width:33.33333333%}|};
      {|a{max-height:54.599999999999994px}b{max-height:54.6px}|};
      {|a{animation-delay:.7250000000000001s}b{animation-delay:.725s}|};
      {|a{color:rgba(0,0,0,0.09999999999999998)}b{color:rgba(0,0,0,.1)}|};
    ]

(* An empty or shrunken corpus and a clean corpus look the same from outside, so
   each one carries the size the repo ships. *)
type corpus = { corpus_name : string; floor : int; load : unit -> input list }

let corpora =
  [
    {
      corpus_name = "css-minify-tests";
      floor = 700;
      load = minify_tests_inputs;
    };
    { corpus_name = "lightning"; floor = 2500; load = lightning_inputs };
    { corpus_name = "examples"; floor = 2; load = example_inputs };
    { corpus_name = "property-grammar"; floor = 2000; load = grammar_inputs };
    { corpus_name = "spellings"; floor = 25; load = normal_form_inputs };
  ]

(* {1 Modes} *)

type mode = {
  label : string;
  optimize : bool;
  scope : Optimize.scope;
  lossless : bool;
  enforce_spec : bool;
  minify : bool;
}

let base =
  {
    label = "cascade fmt";
    optimize = false;
    scope = `Fragment;
    lossless = false;
    enforce_spec = false;
    minify = false;
  }

let minified =
  { base with label = "cascade fmt --minify"; optimize = true; minify = true }

let modes =
  [
    base;
    minified;
    {
      minified with
      label = "cascade fmt --minify --scope=stylesheet";
      scope = `Stylesheet;
    };
    {
      minified with
      label = "cascade fmt --minify --enforce-spec";
      enforce_spec = true;
    };
    { minified with label = "cascade fmt --minify --lossless"; lossless = true };
  ]

let read mode css =
  Css.of_string ~strict:false ~enforce_spec:mode.enforce_spec css

let emit mode stylesheet =
  let stylesheet =
    if mode.optimize then
      Css.optimize ~scope:mode.scope ~lossless:mode.lossless
        ~enforce_spec:mode.enforce_spec stylesheet
    else stylesheet
  in
  Css.to_string ~minify:mode.minify ~lossless:mode.lossless
    ~enforce_spec:mode.enforce_spec stylesheet

(* {1 The check} *)

(* Diagnostics are compared without their spans: the same complaint about the
   same construct lands at a different offset once the text is minified. *)
let shape (e : Error.t) =
  String.concat ""
    [ String.concat "/" e.path; ": "; Pp.to_string Error.pp_kind e.kind ]

let rec take_out x = function
  | [] -> None
  | y :: tl when String.equal x y -> Some tl
  | y :: tl -> Option.map (fun rest -> y :: rest) (take_out x tl)

let introduced ~before ~after =
  let keep (pool, extra) s =
    match take_out s pool with
    | Some pool -> (pool, extra)
    | None -> (pool, s :: extra)
  in
  List.fold_left keep (before, []) after |> snd |> List.rev

type finding = {
  emitted : string;
  again : string option;  (** second pass, when it differs from the first *)
  diagnostics : string list;  (** what re-reading the emission newly raised *)
  fatal : string option;  (** the reader or the printer gave up *)
}

(* [read] and [emit] are arguments so that the calibration cases can drive the
   same oracle with an emitter that is wrong on purpose. *)
let attempt ~(read : string -> (Css.parse, Error.t) result)
    ~(emit : Css.t -> string) css =
  match read css with
  | Error _ -> None (* Cascade never emitted this, so it owes it nothing. *)
  | Ok parsed -> (
      let emitted = emit parsed.stylesheet in
      let before = List.map shape parsed.warnings in
      match read emitted with
      | Error e ->
          Some
            {
              emitted;
              again = None;
              diagnostics = [];
              fatal = Some (Error.to_string e);
            }
      | Ok back ->
          let diagnostics =
            introduced ~before ~after:(List.map shape back.warnings)
          in
          let second = emit back.stylesheet in
          let again =
            if String.equal second emitted then None else Some second
          in
          if Option.is_none again && List.is_empty diagnostics then None
          else Some { emitted; again; diagnostics; fatal = None })

(* The sweep runs before Alcotest sees a single case, so one input that makes a
   printer raise would take the whole report down with it. *)
let check ~read ~emit css =
  try attempt ~read ~emit css
  with Invalid_argument msg | Failure msg ->
    Some
      {
        emitted = "";
        again = None;
        diagnostics = [];
        fatal = Some (String.concat "" [ "raised: "; msg ]);
      }

(* What makes two failures the same failure, for the reducer. *)
let signature f =
  String.concat "|"
    ((match f.fatal with Some _ -> "fatal" | None -> "read")
    :: (match f.again with Some _ -> "moves" | None -> "stands")
    :: f.diagnostics)

(* {1 Reduction} *)

(* Index just past the construct starting at [i], so that a delimiter inside a
   comment or a string is not mistaken for structure. *)
let skip text ~stop i =
  let rec closing quote j =
    if j >= stop then stop
    else if Char.equal text.[j] '\\' then closing quote (j + 2)
    else if Char.equal text.[j] quote then j + 1
    else closing quote (j + 1)
  in
  let rec comment j =
    if j + 1 >= stop then stop
    else if Char.equal text.[j] '*' && Char.equal text.[j + 1] '/' then j + 2
    else comment (j + 1)
  in
  match text.[i] with
  | '/' when i + 1 < stop && Char.equal text.[i + 1] '*' -> comment (i + 2)
  | ('"' | '\'') as quote -> closing quote (i + 1)
  | '\\' -> min stop (i + 2)
  | _ -> i + 1

(* Index just past the '}' closing the block whose body starts at [i]. *)
let rec skip_block text ~stop i =
  if i >= stop then stop
  else
    match text.[i] with
    | '}' -> i + 1
    | '{' -> skip_block text ~stop (skip_block text ~stop (i + 1))
    | _ -> skip_block text ~stop (skip text ~stop i)

(* Every statement span at every nesting level, as (offset, length) pairs. *)
let rec spans text ~start ~stop acc =
  let rec walk i piece acc =
    if i >= stop then if i > piece then (piece, i - piece) :: acc else acc
    else
      match text.[i] with
      | ';' -> walk (i + 1) (i + 1) ((piece, i + 1 - piece) :: acc)
      | '{' ->
          let after = skip_block text ~stop (i + 1) in
          let body_end =
            if after > i + 1 && Char.equal text.[after - 1] '}' then after - 1
            else after
          in
          let acc = spans text ~start:(i + 1) ~stop:body_end acc in
          walk after after ((piece, after - piece) :: acc)
      | _ -> walk (skip text ~stop i) piece acc
  in
  walk start start acc

let without text (start, len) =
  String.concat ""
    [
      String.sub text 0 start;
      String.sub text (start + len) (String.length text - start - len);
    ]

let longest_first (_, a) (_, b) = Int.compare b a

(* Delete one span at a time, largest first, keeping a deletion only when the
   result still fails the same way. *)
let reduce ~reproduces text =
  let rec loop text budget =
    let spent = ref 0 in
    let smaller (start, len) =
      if !spent >= budget || len = 0 || len >= String.length text then false
      else (
        incr spent;
        reproduces (without text (start, len)))
    in
    let candidates =
      spans text ~start:0 ~stop:(String.length text) []
      |> List.sort longest_first
    in
    if budget <= 0 then text
    else
      match List.find_opt smaller candidates with
      | None -> text
      | Some span -> loop (without text span) (budget - !spent)
  in
  loop text 4000

let minimise ~read ~emit css finding =
  let target = signature finding in
  let reproduces candidate =
    match check ~read ~emit candidate with
    | None -> false
    | Some f -> String.equal (signature f) target
  in
  reduce ~reproduces css

(* {1 Reporting} *)

(* One entry per input that failed, carrying the reduced text rather than the
   fixture it came from. *)
type failure = { input : input; reduced : string; finding : finding }

type result = {
  mode : mode;
  corpus : corpus;
  exercised : int;
  failures : failure list;
}

let sweep mode (corpus, inputs) =
  let read = read mode and emit = emit mode in
  let failures =
    List.filter_map
      (fun input ->
        Option.map
          (fun finding ->
            { input; reduced = minimise ~read ~emit input.css finding; finding })
          (check ~read ~emit input.css))
      inputs
  in
  { mode; corpus; exercised = List.length inputs; failures }

let add_line buf label value =
  Buffer.add_string buf "    ";
  Buffer.add_string buf label;
  Buffer.add_string buf " : ";
  Buffer.add_string buf (String.escaped value);
  Buffer.add_char buf '\n'

let add_failure buf failure =
  Buffer.add_string buf "  ";
  Buffer.add_string buf failure.input.name;
  Buffer.add_char buf '\n';
  add_line buf "input " failure.reduced;
  add_line buf "pass 1" failure.finding.emitted;
  Option.iter (add_line buf "pass 2") failure.finding.again;
  Option.iter (add_line buf "reader") failure.finding.fatal;
  List.iter (add_line buf "reader") failure.finding.diagnostics

let headline result =
  String.concat ""
    [
      string_of_int (List.length result.failures);
      " of ";
      string_of_int result.exercised;
      " ";
      result.corpus.corpus_name;
      " inputs are not a fixed point of `";
      result.mode.label;
      "`";
    ]

(* Alcotest shows the message of the first failing case only, so the whole list
   goes to the console before the run and each case keeps its own count. *)
(* A corpus with no failures says nothing in the list below, so its size is
   stated here: a silent corpus is a clean one only if it was read at all. *)
let add_exercised buf loaded =
  Buffer.add_string buf "exercised";
  List.iter
    (fun (corpus, inputs) ->
      Buffer.add_char buf ' ';
      Buffer.add_string buf corpus.corpus_name;
      Buffer.add_char buf '=';
      Buffer.add_string buf (string_of_int (List.length inputs)))
    loaded;
  Buffer.add_string buf ", in every mode\n"

let print_report loaded results =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf "cascade does not read back what it writes\n";
  add_exercised buf loaded;
  List.iter
    (fun result ->
      match result.failures with
      | [] -> ()
      | failures ->
          Buffer.add_char buf '\n';
          Buffer.add_string buf (headline result);
          Buffer.add_char buf '\n';
          List.iter (add_failure buf) failures)
    results;
  Buffer.add_char buf '\n';
  print_string (Buffer.contents buf)

(* {1 Serialised} *)

let selector_text sel = Selector.to_string ~minify:true sel

(* The tree the reader builds, down to the simple selectors and the selector
   lists of the pseudo-classes that take one. An unknown functional pseudo is a
   leaf compared by spelling: it holds the tokens it was read from, positions
   included. *)
let rec same_selector (a : Selector.t) (b : Selector.t) =
  match (a, b) with
  | Compound xs, Compound ys
  | List xs, List ys
  | Is xs, Is ys
  | Where xs, Where ys
  | Not xs, Not ys
  | Has xs, Has ys ->
      List.equal same_selector xs ys
  | Combined (l, c, r), Combined (l', c', r') ->
      Selector.equal_combinator c c' && same_selector l l' && same_selector r r'
  | Relative (c, r), Relative (c', r') ->
      Selector.equal_combinator c c' && same_selector r r'
  | ( ( Compound _ | List _ | Is _ | Where _ | Not _ | Has _ | Combined _
      | Relative _ ),
      _ )
  | ( _,
      ( Compound _ | List _ | Is _ | Where _ | Not _ | Has _ | Combined _
      | Relative _ ) ) ->
      false
  | (Unknown_pseudo_class_call _ | Unknown_pseudo_element_call _), _ ->
      String.equal (selector_text a) (selector_text b)
  | a, b -> Selector.equal a b

(* Every selector a sheet holds, in document order. Only selectors are compared,
   so a statement the printer writes as nothing (an empty [@font-face]) does not
   shift the ones after it. *)
let rec selectors acc (stmts : Stylesheet.statement list) =
  List.fold_left
    (fun acc (stmt : Stylesheet.statement) ->
      let acc =
        match stmt with
        | Rule r -> r.selector :: acc
        | Scope (start, stop, _) ->
            List.rev_append (List.filter_map Fun.id [ start; stop ]) acc
        | _ -> acc
      in
      selectors acc (Stylesheet.statement_children stmt))
    acc stmts

let rec first_unread built read =
  match (built, read) with
  | [], [] -> None
  | [], _ :: _ | _ :: _, [] -> Some "the selectors read back differ in number"
  | b :: built, r :: read ->
      if same_selector b r then first_unread built read
      else
        Some
          (String.concat ""
             [ "built "; selector_text b; ", read back "; selector_text r ])

(* A statement the optimizer keeps is one the printer writes: one it writes as
   nothing reads back as nothing, and the block holding it comes back one
   statement short. *)
let rec first_dropped built read =
  let built_n = List.length built and read_n = List.length read in
  if built_n <> read_n then
    Some
      (String.concat ""
         [
           string_of_int built_n;
           " statements built, ";
           string_of_int read_n;
           " read back";
         ])
  else
    List.fold_left2
      (fun found b r ->
        match found with
        | Some _ -> found
        | None ->
            first_dropped
              (Stylesheet.statement_children b)
              (Stylesheet.statement_children r))
      None built read

let unread_selector mode css =
  match read mode css with
  | Error _ -> None
  | Ok parsed -> (
      let built =
        Css.optimize ~scope:mode.scope ~lossless:mode.lossless
          ~enforce_spec:mode.enforce_spec parsed.stylesheet
      in
      let printed =
        Css.to_string ~minify:mode.minify ~lossless:mode.lossless
          ~enforce_spec:mode.enforce_spec built
      in
      match read mode printed with
      | Error e -> Some (Error.to_string e)
      | Ok back -> (
          match first_dropped built back.stylesheet with
          | Some _ as dropped -> dropped
          | None ->
              first_unread
                (List.rev (selectors [] built))
                (List.rev (selectors [] back.stylesheet))))

type unread = { unread_input : input; unread_reduced : string; why : string }

let serialised_sweep mode (corpus, inputs) =
  let check css = try unread_selector mode css with Failure _ -> None in
  let unread =
    List.filter_map
      (fun input ->
        Option.map
          (fun why ->
            let reproduces candidate = Option.is_some (check candidate) in
            {
              unread_input = input;
              unread_reduced = reduce ~reproduces input.css;
              why;
            })
          (check input.css))
      inputs
  in
  (corpus, List.length inputs, unread)

let unread_headline (corpus, exercised, unread) mode =
  String.concat ""
    [
      string_of_int (List.length unread);
      " of ";
      string_of_int exercised;
      " ";
      corpus.corpus_name;
      " inputs build a selector `";
      mode.label;
      "` does not read back";
    ]

let print_unread mode ((_, _, unread) as result) =
  match unread with
  | [] -> ()
  | unread ->
      let buf = Buffer.create 1024 in
      Buffer.add_string buf (unread_headline result mode);
      Buffer.add_char buf '\n';
      List.iter
        (fun u ->
          Buffer.add_string buf "  ";
          Buffer.add_string buf u.unread_input.name;
          Buffer.add_char buf '\n';
          add_line buf "input " u.unread_reduced;
          add_line buf "reader" u.why)
        unread;
      print_string (Buffer.contents buf)

(* {1 Values}

   The minified printer prints the node it is given (docs/invariants.md, "pp is
   a serializer"): a declaration read from a corpus and printed minified,
   without the optimizer, reads back as the same declaration. Custom properties
   are left out, for the reason the serialised check gives.

   [pending] names each property for which that does not hold yet, with what
   still breaks it: the minified printer changing the node, or the reader
   keeping a spelling the node carries ([#FFF], [0.5em]). The check fails on a
   property missing from the list and on one listed that no longer fails, so the
   list can only shrink. *)

let pending =
  [
    (* an authored hex keeps its digits *)
    "color";
    "border";
    "outline";
    (* a string keeps its authored quote and escapes *)
    "content";
    (* [ms] prints as [s] *)
    "animation-delay";
    "animation-duration";
    "transition-delay";
    "transition-duration";
    (* the reader keeps the case of [none] *)
    "animation";
    (* a [var()] fallback keeps its source spelling *)
    "transition";
    (* a [calc()] around a number goes, as does a zero [hue-rotate()] angle *)
    "filter";
    "flex";
    "opacity";
    (* the value is invalid or unknown, and its tokens print without
       separators *)
    "clip-path";
    "font-family";
    "foo";
    "rotate";
    (* the reader keeps the authored number ([1.0000px], [0.5em]) *)
    "font-size";
    "margin";
    "margin-block";
    "max-height";
    "vertical-align";
    "width";
  ]

let is_pending name = List.exists (String.equal name) pending
let declaration_text d = Declaration.to_string ~minify:true d

let unread_value ?(print = declaration_text) d =
  let printed = print d in
  match Declaration.of_string printed with
  | Error e -> Some (String.concat "" [ printed; ": "; Error.to_string e ])
  | Ok back ->
      if Declaration.equal_declaration d back then None
      else
        Some
          (String.concat ""
             [
               Declaration.to_string d;
               " prints as ";
               printed;
               ", which reads back as ";
               Declaration.to_string back;
             ])

let corpus_declarations inputs =
  List.concat_map
    (fun input ->
      match read base input.css with
      | Error _ -> []
      | Ok parsed ->
          Stylesheet.fold_declarations
            (fun acc ds -> List.rev_append ds acc)
            [] parsed.stylesheet
          |> List.rev)
    inputs

let typed d =
  not (String.starts_with ~prefix:"--" (Declaration.property_name d))

type value_report = {
  checked : int;
  unread : (string * string) list;  (** property, why *)
  settled : string list;  (** pending properties that read back *)
}

let values_sweep loaded =
  let decls =
    List.concat_map (fun (_, inputs) -> corpus_declarations inputs) loaded
    |> List.filter typed
  in
  let failing = Hashtbl.create 64 in
  let unread =
    List.filter_map
      (fun d ->
        let name = Declaration.property_name d in
        match
          try unread_value d
          with Failure msg | Invalid_argument msg -> Some msg
        with
        | None -> None
        | Some why ->
            Hashtbl.replace failing name ();
            if is_pending name then None else Some (name, why))
      decls
  in
  let settled =
    List.filter (fun name -> not (Hashtbl.mem failing name)) pending
  in
  { checked = List.length decls; unread; settled }

let print_values report =
  let buf = Buffer.create 1024 in
  List.iter
    (fun (name, why) ->
      Buffer.add_string buf "  ";
      Buffer.add_string buf name;
      Buffer.add_string buf " : ";
      Buffer.add_string buf (String.escaped why);
      Buffer.add_char buf '\n')
    report.unread;
  List.iter
    (fun name ->
      Buffer.add_string buf "  ";
      Buffer.add_string buf name;
      Buffer.add_string buf " : reads back everywhere, drop it from pending\n")
    report.settled;
  print_string (Buffer.contents buf)

let values_case report () =
  match (report.unread, report.settled) with
  | [], [] -> ()
  | unread, settled ->
      Alcotest.failf
        "of %d declarations, %d print as a text that reads back as another \
         node, and %d pending properties now read back"
        report.checked (List.length unread) (List.length settled)

(* {1 Normal form}

   Normalize reaches the node the reader gives its own print
   (docs/invariants.md, "pp is a serializer"). For every typed declaration a
   corpus holds and [n] its normal form, in both the lossy and the lossless
   mode:

   - normalising [n] again gives [n]; - reading [n]'s print back, minified or
   pretty, and normalising gives [n]; - two normal forms with one minified print
   are one node, so they share a {!Declaration.hash}, which is what factoring
   buckets on.

   A normal form that breaks the second or third is a merge one [--minify]
   misses and a second makes, since the second reads the print and the first
   held the node. Custom properties are left out, as in the values check, and so
   is a declaration the reader marks invalid, which the optimizer drops before
   anything reads its node ({!Declaration.is_invalid}). *)

let normal ~lossless d = Declaration.normalize ~lossless d
let print ~lossless ~minify d = Pp.to_string ~minify ~lossless Declaration.pp d

let off_normal ?(normalize = normal) ?(print = print) ~lossless d =
  let n = normalize ~lossless d in
  let again = normalize ~lossless n in
  let text = print ~lossless ~minify:true in
  if not (Declaration.equal_declaration n again) then
    Some
      (String.concat ""
         [
           Declaration.to_string d;
           " normalises to ";
           text n;
           ", which normalises again to ";
           text again;
         ])
  else
    List.find_map
      (fun minify ->
        let printed = print ~lossless ~minify n in
        match Declaration.of_string printed with
        | Error e ->
            Some (String.concat "" [ printed; ": "; Error.to_string e ])
        | Ok back ->
            let back = normalize ~lossless back in
            if Declaration.equal_declaration n back then None
            else
              Some
                (String.concat ""
                   [
                     Declaration.to_string d;
                     " normalises to a node printing ";
                     printed;
                     ", whose reading normalises to ";
                     text back;
                   ]))
      [ true; false ]

type normal_report = {
  normalised : int;
  off : string list;
      (** normal forms that are not the reading of their print *)
  split : string list;  (** one minified print, two normal forms *)
}

let normal_sweep ?normalize ?print:(pr = print) decls =
  let off = ref [] and split = ref [] in
  let seen = Hashtbl.create 1024 in
  let normalize_with = Option.value normalize ~default:normal in
  List.iter
    (fun lossless ->
      List.iter
        (fun d ->
          (match
             try off_normal ?normalize ~print:pr ~lossless d
             with Failure msg | Invalid_argument msg -> Some msg
           with
          | Some why -> off := why :: !off
          | None -> ());
          let n = normalize_with ~lossless d in
          let key = (lossless, pr ~lossless ~minify:true n) in
          match Hashtbl.find_opt seen key with
          | None -> Hashtbl.add seen key n
          | Some m ->
              if not (Declaration.equal_declaration m n) then
                split :=
                  String.concat ""
                    [
                      snd key;
                      " is the print of two normal forms, read from ";
                      Declaration.to_string m;
                      " and ";
                      Declaration.to_string d;
                    ]
                  :: !split)
        decls)
    [ false; true ];
  {
    normalised = 2 * List.length decls;
    off = List.sort_uniq String.compare !off;
    split = List.sort_uniq String.compare !split;
  }

let print_normal report =
  let buf = Buffer.create 1024 in
  List.iter
    (fun why ->
      Buffer.add_string buf "  normal form : ";
      Buffer.add_string buf (String.escaped why);
      Buffer.add_char buf '\n')
    (report.off @ report.split);
  print_string (Buffer.contents buf)

let normal_case report () =
  match (report.off, report.split) with
  | [], [] -> ()
  | off, split ->
      Alcotest.failf
        "of %d normal forms, %d are not the reading of their own print and %d \
         prints are shared by two normal forms"
        report.normalised (List.length off) (List.length split)

(* {1 Calibration}

   A report of nothing is a clean run or a blind oracle, and the two look alike
   from outside. These drive the same oracle with emitters that are wrong on
   purpose, so they keep discriminating after the defects in the corpora are
   fixed. *)

let honest = emit minified
let reader = read minified

(* The emission class this check exists for: a CSS-wide keyword standing as one
   component of a value the reader only accepts it as the whole of. *)
let keyword_component _ = "a{margin:1px inherit}"

(* Readable but never settling: each pass measures the text the pass before it
   wrote. *)
let growing stylesheet =
  let text = honest stylesheet in
  String.concat ""
    [ text; "b{width:"; string_of_int (String.length text); "px}" ]

let calibration_input = "a{margin:1px 2px}"

let honest_emitter_passes () =
  match check ~read:reader ~emit:honest calibration_input with
  | None -> ()
  | Some finding ->
      Alcotest.failf "%s is not a fixed point: %s" calibration_input
        finding.emitted

let unreadable_emitter_caught () =
  match check ~read:reader ~emit:keyword_component calibration_input with
  | None -> Alcotest.fail "an emission the reader rejects went unreported"
  | Some finding ->
      if List.is_empty finding.diagnostics && Option.is_none finding.fatal then
        Alcotest.failf "no reader diagnostic reported for %s" finding.emitted

let unstable_emitter_caught () =
  match check ~read:reader ~emit:growing calibration_input with
  | None -> Alcotest.fail "an emission that never settles went unreported"
  | Some finding ->
      if Option.is_none finding.again then
        Alcotest.failf "no second pass reported for %s" finding.emitted;
      if not (List.is_empty finding.diagnostics) then
        Alcotest.failf "unexpected reader diagnostic for %s" finding.emitted

(* Reduction has to keep the failure, not just shrink the text. *)
let reduction_keeps_the_failure () =
  let padded =
    String.concat "" [ "b{color:red}"; calibration_input; "c{color:#00f}" ]
  in
  match check ~read:reader ~emit:keyword_component padded with
  | None -> Alcotest.fail "the saboteur stopped failing on a padded input"
  | Some finding ->
      let reduced =
        minimise ~read:reader ~emit:keyword_component padded finding
      in
      if String.length reduced >= String.length padded then
        Alcotest.failf "reduction did not shrink %s" padded;
      if Option.is_none (check ~read:reader ~emit:keyword_component reduced)
      then Alcotest.failf "reduced input %s no longer fails" reduced

(* The selector class the serialised check exists for: a compound spliced into a
   compound, which prints like the flat one the reader builds. *)
let nested_compound_caught () =
  let built =
    Selector.Compound
      [
        Selector.compound [ Selector.class_ "a"; Selector.Hover ];
        Selector.Focus;
      ]
  in
  let read = Selector.v (selector_text built) in
  if same_selector built read then
    Alcotest.failf "%s spliced into a compound went unreported"
      (selector_text built)

(* The value class the values check exists for: a printer that writes another
   node than the one it was given. *)
let unread_value_caught () =
  let blue =
    match Declaration.of_string "color:blue" with
    | Ok d -> d
    | Error e -> Alcotest.failf "color:blue: %s" (Error.to_string e)
  in
  match unread_value ~print:(fun _ -> "color:red") blue with
  | Some _ -> ()
  | None -> Alcotest.fail "a value printed as another went unreported"

(* The class the normal-form check exists for: a printer that drops the quotes
   of a [url()] the normaliser kept, and one that gives two nodes one text. *)
let off_normal_caught () =
  let read text =
    match Declaration.of_string text with
    | Ok d -> d
    | Error e -> Alcotest.failf "%s: %s" text (Error.to_string e)
  in
  let as_is ~lossless:_ d = d in
  let unquoted ~lossless:_ ~minify:_ _ = "background-image:url(i.png)" in
  let quoted = read {|background-image:url("i.png")|} in
  let report = normal_sweep ~normalize:as_is ~print:unquoted [ quoted ] in
  if List.is_empty report.off then
    Alcotest.fail "a print reading back as another normal form went unreported";
  let red ~lossless:_ ~minify:_ _ = "color:red" in
  let report =
    normal_sweep ~normalize:as_is ~print:red
      [ read "color:red"; read "color:blue" ]
  in
  if List.is_empty report.split then
    Alcotest.fail "two normal forms printing alike went unreported"

let calibration =
  [
    Alcotest.test_case "unread value caught" `Quick unread_value_caught;
    Alcotest.test_case "off normal form caught" `Quick off_normal_caught;
    Alcotest.test_case "unread selector caught" `Quick nested_compound_caught;
    Alcotest.test_case "fixed point accepted" `Quick honest_emitter_passes;
    Alcotest.test_case "unreadable emission caught" `Quick
      unreadable_emitter_caught;
    Alcotest.test_case "unsettled emission caught" `Quick
      unstable_emitter_caught;
    Alcotest.test_case "reduction keeps the failure" `Quick
      reduction_keeps_the_failure;
  ]

(* {1 Entry point} *)

let size_case corpus inputs () =
  let loaded = List.length inputs in
  if loaded < corpus.floor then
    Alcotest.failf
      "%s: loaded %d inputs, expected at least %d - the corpus moved, shrank, \
       or was not copied into the build directory"
      corpus.corpus_name loaded corpus.floor

let sweep_case result () =
  match result.failures with [] -> () | _ -> Alcotest.fail (headline result)

let () =
  let loaded = List.map (fun corpus -> (corpus, corpus.load ())) corpora in
  let sizes =
    List.map
      (fun (corpus, inputs) ->
        Alcotest.test_case corpus.corpus_name `Quick (size_case corpus inputs))
      loaded
  in
  let group mode =
    let results = List.map (sweep mode) loaded in
    let cases =
      List.map
        (fun result ->
          Alcotest.test_case result.corpus.corpus_name `Quick
            (sweep_case result))
        results
    in
    (results, (mode.label, cases))
  in
  let groups = List.map group modes in
  if
    List.exists
      (fun (results, _) ->
        List.exists (fun r -> not (List.is_empty r.failures)) results)
      groups
  then print_report loaded (List.concat_map fst groups);
  let serialised =
    List.filter_map
      (fun mode ->
        if not mode.optimize then None
        else
          let cases =
            List.map
              (fun loaded ->
                let ((corpus, _, unread) as result) =
                  serialised_sweep mode loaded
                in
                print_unread mode result;
                Alcotest.test_case corpus.corpus_name `Quick (fun () ->
                    match unread with
                    | [] -> ()
                    | _ -> Alcotest.fail (unread_headline result mode)))
              loaded
          in
          Some (String.concat "" [ mode.label; " (serialised)" ], cases))
      modes
  in
  let values = values_sweep loaded in
  print_values values;
  let normal =
    List.concat_map (fun (_, inputs) -> corpus_declarations inputs) loaded
    |> List.filter (fun d -> typed d && not (Declaration.is_invalid d))
    |> normal_sweep
  in
  print_normal normal;
  Alcotest.run "fixpoint"
    (("calibration", calibration)
    :: ("corpora", sizes)
    :: (List.map snd groups @ serialised
       @ [
           ( "cascade fmt --minify, no optimizer (values)",
             [ Alcotest.test_case "every corpus" `Quick (values_case values) ]
           );
           ( "normalize, no optimizer (normal form)",
             [ Alcotest.test_case "every corpus" `Quick (normal_case normal) ]
           );
         ]))
