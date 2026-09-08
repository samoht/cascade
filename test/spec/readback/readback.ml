(* Does cascade read back everything it writes, over GENERATED input?

   test/fixpoint asks the same question over the corpora the repo ships, and
   says so in its own header: "Nothing here is generated, and nothing here needs
   regenerating". This asks it over the sheets
   {!Cascade_spec_inventory.Sheet_gen} draws from the generator the accept-set
   harness uses, so the population moves with the seed instead of with the
   repository.

   Every sheet the generator draws, not a sample of them: the sweep costs a few
   seconds over the whole population, and a prefix of a sorted list is a sample
   of the sheets whose text sorts first rather than a sample of the sheets.

   No browser is involved. The question is entirely about cascade: CSS it
   ACCEPTED, printed, and then could not read, or read as something else, is a
   defect whatever any browser thinks of it. Input it rejected is not this
   harness's business - the accept-set oracle owns that direction - so a sheet
   that does not parse, or parses with a warning, is skipped.

   Three failures are reported apart: an emission that does not parse, one that
   parses and moves when it is printed again, and one the comparator does not
   call the sheet it was given.

   Every emission mode the CLI offers is swept, because each runs a different
   set of passes and only the plain formatter runs none of them. Idempotence is
   the optimizer's own contract: it runs a fixed-point loop inside, so an
   emission a second pass moves is a first pass that stopped early.

   The population reaches past one declaration. A pass that decides about a
   declaration by looking at another one - shorthand contraction, duplicate
   elimination, rule merging, the canonical projection - does nothing at all to
   a sheet holding a single declaration, which is every sheet a value generator
   can write. Sheet_gen writes the rest, and carries the parts each sheet is
   built from: a part that is already a finding explains the composite, so the
   composite is not reported a second time.

   Reproducing one finding:

   dune exec test/spec/readback/readback.exe -- --css 'a{color:red}' *)

let seed = ref 0
let one_css = ref None
let population_only = ref false

let usage () =
  prerr_endline
    "readback [--seed N] [--css 'a{color:red}'] [--population]\n\
    \  --seed N     the generator seed (default 0)\n\
    \  --css S      run one sheet and print what each step did\n\
    \  --population print the population size and exit";
  exit 2

let rec parse_args i =
  if i < Array.length Sys.argv then
    match Sys.argv.(i) with
    | "--seed" when i + 1 < Array.length Sys.argv ->
        seed := int_of_string Sys.argv.(i + 1);
        parse_args (i + 2)
    | "--css" when i + 1 < Array.length Sys.argv ->
        one_css := Some Sys.argv.(i + 1);
        parse_args (i + 2)
    | "--population" ->
        population_only := true;
        parse_args (i + 1)
    | _ -> usage ()

(* ===== Emission modes ===== *)

(* The modes test/fixpoint sweeps, for the same reason: none of them is a fixed
   point over every input the repo ships, so checking only the default would be
   picking the mode that makes the shortest list. *)
type mode = {
  label : string;
  optimize : bool;
  scope : Cascade.Optimize.scope;
  lossless : bool;
  enforce_spec : bool;
  minify : bool;
}

let pretty =
  {
    label = "fmt";
    optimize = false;
    scope = `Fragment;
    lossless = false;
    enforce_spec = false;
    minify = false;
  }

let minified =
  { pretty with label = "fmt --minify"; optimize = true; minify = true }

let modes =
  [
    pretty;
    { pretty with label = "fmt --minify (print only)"; minify = true };
    minified;
    { minified with label = "--minify --scope=stylesheet"; scope = `Stylesheet };
    { minified with label = "--minify --enforce-spec"; enforce_spec = true };
    { minified with label = "--minify --lossless"; lossless = true };
  ]

(* A sheet cascade accepted without complaint. Anything else is another oracle's
   question. *)
let accepted mode css =
  match Cascade.Css.of_string ~enforce_spec:mode.enforce_spec css with
  | Error _ -> None
  | Ok { warnings = _ :: _; _ } -> None
  | Ok { stylesheet; warnings = []; _ } -> Some stylesheet

let emit mode stylesheet =
  let stylesheet =
    if mode.optimize then
      Cascade.Css.optimize ~scope:mode.scope ~lossless:mode.lossless
        ~enforce_spec:mode.enforce_spec stylesheet
    else stylesheet
  in
  Cascade.Css.to_string ~minify:mode.minify ~lossless:mode.lossless
    ~enforce_spec:mode.enforce_spec stylesheet

(* ===== Findings ===== *)

type kind = Unreadable | Unstable | Unequal
type finding = { css : string; mode : string; kind : kind; got : string }

let findings : finding list ref = ref []
let dirty = Hashtbl.create 4096

let record ~css ~mode ~kind ~got =
  Hashtbl.replace dirty css ();
  findings := { css; mode; kind; got } :: !findings

(* ===== The checks ===== *)

(* The comparator is asked in one mode. It has no lossless or enforce-spec
   setting, so a divergence under those would be a question about the arguments
   rather than about the emission, and [--minify] is the mode whose passes
   rewrite the most.

   It shares its normalisation with the emitter, so what it answers is whether
   the two sides of the library agree, not whether the emission preserved the
   sheet: a pass both of them run cannot be arbitrated by either. The browser
   oracles own that question. What this catches is the emitter rewriting
   something the comparator does not model, which is a difference a diff run
   over the same sheet would report as real. *)
let compares mode = String.equal mode.label minified.label

let check_mode ~css mode =
  match accepted mode css with
  | None -> ()
  | Some sheet -> (
      let once = emit mode sheet in
      match accepted mode once with
      | None -> record ~css ~mode:mode.label ~kind:Unreadable ~got:once
      | Some again ->
          let twice = emit mode again in
          if not (String.equal once twice) then
            record ~css ~mode:mode.label ~kind:Unstable
              ~got:(String.concat "" [ once; "  ->  "; twice ])
          else if
            compares mode
            && not (Cascade_diff.Css_compare.equal ~mode:`Canonical css once)
          then record ~css ~mode:mode.label ~kind:Unequal ~got:once)

(* Pretty and minified are two spellings of one AST, so reading either back and
   minifying has to give the same text. *)
let check_across ~css =
  match (accepted pretty css, accepted minified css) with
  | Some readable, Some sheet -> (
      let compact = emit minified sheet in
      match accepted minified (emit pretty readable) with
      | None -> ()
      | Some back ->
          let again = emit minified back in
          if not (String.equal again compact) then
            record ~css ~mode:"fmt then --minify" ~kind:Unstable
              ~got:(String.concat "" [ again; "  <>  "; compact ]))
  | Some _, None | None, Some _ | None, None -> ()

(* Every mode over one declaration, two over a sheet holding more. The mode axis
   is where a pass is configured, and the modes differ from each other only in
   what they do to a value; a sheet holding several declarations multiplies the
   population instead, and pays for the modes that would ask it the same
   question six times. *)
let check ~composite css =
  List.iter (check_mode ~css)
    (if composite then [ pretty; minified ] else modes);
  check_across ~css

(* ===== The population ===== *)

(* The manifest names every property the spec inventory tracks. The reader
   answers for more than that (measured by scripts/check_properties.ml: 455 of
   548), so the names it does not carry are outside this sweep. *)
let properties =
  List.sort_uniq String.compare
    Cascade_spec_inventory.Property_grammar.property_names

(* A generator that stopped generating would otherwise turn this sweep into a
   green run that asks nothing. *)
let minimum_properties = 400
let minimum_sheets = 200_000

(* De-duplicated across properties: a sheet two properties both draw is one
   sheet, and one asked twice is one finding reported twice. A sheet reached
   both with parts and without keeps the part-less reading, since it is in the
   population on its own account. *)
let sheets () =
  let table = Hashtbl.create 65536 in
  let order = ref [] in
  List.iter
    (fun property ->
      List.iter
        (fun (s : Cascade_spec_inventory.Sheet_gen.sheet) ->
          match Hashtbl.find_opt table s.css with
          | Some [] -> ()
          | Some (_ :: _) -> if s.parts = [] then Hashtbl.replace table s.css []
          | None ->
              Hashtbl.replace table s.css s.parts;
              order := s.css :: !order)
        (Cascade_spec_inventory.Sheet_gen.sheets_for ~seed:!seed property))
    properties;
  List.rev_map
    (fun css ->
      { Cascade_spec_inventory.Sheet_gen.css; parts = Hashtbl.find table css })
    !order

(* ===== Calibration ===== *)

(* A run that reports nothing has to be a run that could have. Each check is put
   through its own oracle with an input it must catch, so a green sweep means
   the population was examined rather than that a check went quiet. *)
let calibrate () =
  let fail what = failwith (String.concat "" [ "calibration: "; what ]) in
  let sheet =
    match accepted minified "a{color:red}" with
    | Some s -> s
    | None -> fail "cascade cannot read a{color:red}"
  in
  let compact = emit minified sheet in
  (match accepted minified (String.concat "" [ compact; "@@{" ]) with
  | None -> ()
  | Some _ -> fail "an unreadable sheet read, so the unreadable check is blind");
  (match accepted minified "a{color:blue}" with
  | Some other when not (String.equal (emit minified other) compact) -> ()
  | Some _ | None -> fail "two different sheets compared equal");
  (* The comparator has to call two different sheets different, or the unequal
     check reports nothing whatever the optimizer does to a sheet. *)
  if
    Cascade_diff.Css_compare.equal ~mode:`Canonical "a{color:red}"
      "a{color:blue}"
  then fail "the comparator called two different sheets equal";
  if
    not
      (Cascade_diff.Css_compare.equal ~mode:`Canonical "a{color:red}"
         "a{color:#f00}")
  then fail "the comparator called one sheet two";
  (* The optimizer has to run in the modes that ask for it: a mode that emitted
     its input would pass every check here without exercising a pass. *)
  match accepted minified "a{margin-top:1px;margin:2px}" with
  | None -> fail "cascade cannot read a two-declaration rule"
  | Some s ->
      if String.equal (emit minified s) (emit pretty s) then
        fail "the optimizer left a rule the elimination pass owns"

(* ===== Reporting ===== *)

let kind_name = function
  | Unreadable -> "UNREADABLE: cascade cannot read what it printed"
  | Unstable -> "UNSTABLE: printing the emission again moves it"
  | Unequal -> "UNEQUAL: the emission is not the sheet it was given"

(* A sheet with every property name written [P] and every value [V]. One loose
   pass shows up as one shape under a hundred property names and a thousand
   values, so the shape is the cause and neither of those is: what a reader of
   this report wants to know is that a second pass merged two rules the first
   left apart, not that it did so for [margin-left] as well as for [top]. *)
let holds text needle =
  let n = String.length needle and h = String.length text in
  let rec at i =
    i + n <= h && (String.equal (String.sub text i n) needle || at (i + 1))
  in
  at 0

let property_name text =
  (not (String.equal text ""))
  && String.for_all
       (fun c ->
         match c with
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' -> true
         | _ -> false)
       text

let shape css =
  let out = Buffer.create (String.length css) in
  let held = Buffer.create 32 in
  let inside = ref false in
  let close () =
    if !inside then (
      Buffer.add_string out
        (if holds (Buffer.contents held) "!important" then "V!important"
         else "V");
      inside := false)
    else Buffer.add_string out (Buffer.contents held);
    Buffer.clear held
  in
  String.iter
    (fun c ->
      match c with
      | ':'
        when (not !inside) && property_name (String.trim (Buffer.contents held))
        ->
          Buffer.clear held;
          Buffer.add_string out "P:";
          inside := true
      | ';' | '{' | '}' ->
          close ();
          Buffer.add_char out c
      | _ -> Buffer.add_char held c)
    css;
  close ();
  Buffer.contents out

(* Grouped by the properties written, not by the mode: one loose pass answers
   the same way in every mode that runs it, and a cause split six ways reads as
   six defects. *)
let grouped findings =
  let table = Hashtbl.create 256 in
  List.iter
    (fun f ->
      let key = String.concat "   ->   " [ shape f.css; shape f.got ] in
      let previous = try Hashtbl.find table key with Not_found -> [] in
      Hashtbl.replace table key (f :: previous))
    findings;
  Hashtbl.fold (fun key members acc -> (key, List.rev members) :: acc) table []
  |> List.sort (fun (a, xs) (b, ys) ->
      match compare (List.length ys) (List.length xs) with
      | 0 -> String.compare a b
      | n -> n)

(* Enough vectors of a cause to see whether it is one cause. *)
let witnesses = 3

let report () =
  List.iter
    (fun kind ->
      let mine = List.filter (fun f -> f.kind = kind) !findings in
      match mine with
      | [] -> ()
      | _ ->
          let groups = grouped mine in
          Fmt.pr "@.=== %s: %d over %d cause(s) ===@." (kind_name kind)
            (List.length mine) (List.length groups);
          List.iter
            (fun (key, members) ->
              let modes =
                String.concat ", "
                  (List.sort_uniq String.compare
                     (List.map (fun f -> f.mode) members))
              in
              Fmt.pr "@.  %s@.  %d vector(s), in %s@." key (List.length members)
                modes;
              List.iteri
                (fun i f ->
                  if i < witnesses then Fmt.pr "    %s@.      %s@." f.css f.got)
                members)
            groups)
    [ Unreadable; Unstable; Unequal ]

(* ===== One sheet ===== *)

let explain css =
  List.iter
    (fun mode ->
      match accepted mode css with
      | None -> Fmt.pr "%-28s did not accept it@." mode.label
      | Some sheet -> (
          let once = emit mode sheet in
          let flat = String.concat " " (String.split_on_char '\n' once) in
          match accepted mode once with
          | None -> Fmt.pr "%-28s %s  [UNREADABLE]@." mode.label flat
          | Some again ->
              let twice = emit mode again in
              let verdict =
                if not (String.equal once twice) then
                  String.concat "" [ "  [UNSTABLE -> "; twice; "]" ]
                else if
                  compares mode
                  && not
                       (Cascade_diff.Css_compare.equal ~mode:`Canonical css once)
                then "  [UNEQUAL]"
                else ""
              in
              Fmt.pr "%-28s %s%s@." mode.label flat verdict))
    modes

(* ===== Main ===== *)

let fail line =
  Fmt.pr "FAIL %s@." line;
  exit 1

let () =
  parse_args 1;
  calibrate ();
  match !one_css with
  | Some css ->
      explain css;
      exit 0
  | None ->
      let population = sheets () in
      let total = List.length population in
      if !population_only then (
        Fmt.pr "readback: %d sheet(s) over %d propert(ies), seed %d@." total
          (List.length properties) !seed;
        exit 0);
      if List.length properties < minimum_properties then
        fail
          (String.concat ""
             [
               "the inventory yielded ";
               string_of_int (List.length properties);
               " properties, fewer than the ";
               string_of_int minimum_properties;
               " expected";
             ]);
      if total < minimum_sheets then
        fail
          (String.concat ""
             [
               "the generator yielded ";
               string_of_int total;
               " sheets, fewer than the ";
               string_of_int minimum_sheets;
               " expected";
             ]);
      let started = Unix.gettimeofday () in
      (* The parts first, all of them, before any composite is judged. A pass
         that is wrong about one declaration is wrong about every sheet holding
         it, so the composite is that finding rather than a second one. *)
      List.iter
        (fun (s : Cascade_spec_inventory.Sheet_gen.sheet) ->
          match s.parts with [] -> check ~composite:false s.css | _ :: _ -> ())
        population;
      let parts_found = List.length !findings in
      List.iter
        (fun (s : Cascade_spec_inventory.Sheet_gen.sheet) ->
          match s.parts with
          | [] -> ()
          | _ :: _ ->
              if not (List.exists (Hashtbl.mem dirty) s.parts) then
                check ~composite:true s.css)
        population;
      let elapsed = Unix.gettimeofday () -. started in
      Fmt.pr "readback: %d sheet(s) over %d propert(ies), seed %d, %.1fs@."
        total (List.length properties) !seed elapsed;
      Fmt.pr
        "readback: %d finding(s) on a single declaration, %d on a sheet \
         holding more@."
        parts_found
        (List.length !findings - parts_found);
      report ();
      exit (if !findings = [] then 0 else 1)
