(* Whether a difference [cascade diff] REPORTS is accurate.

   [test/render/canonical_agree.ml] asks the other half of the certification
   claim: when the verdict is "no difference", do the two sheets really render
   alike. Nothing asked whether a report is true of the files it describes, and
   README makes that a claim - "added, removed, modified, and reordered rules
   are detected structurally, and property value changes are reported in terms
   of CSS values", with exit 1 for a difference and exit 0 for identity - so a
   report that names a rule neither file changed, or misses one both files
   changed, is a defect on the same footing as a wrong verdict.

   Three oracles, none of them cascade's own diff:

   1. Constructed ground truth. A seeded model of a stylesheet is rendered to
   text, one addressed mutation is applied to the model, and both sides are
   rendered again. The mutation's site is known before the diff runs, so the set
   of selectors the report names must be exactly the set the mutation touched: a
   name outside it is an over-report, a name missing from it is a miss. The
   values a value change reports are read back from the two parsed sheets, never
   from the diff. Weakness: only the mutations written here.

   2. Claim verification. Every entry a report emits asserts something about the
   two files - this selector is in the actual side, that declaration is in the
   expected side, this property went from X to Y. Each such assertion is checked
   against the two parsed stylesheets. This needs no ground truth, so it runs
   over corpus pairs too. Weakness: it can only falsify what the report says,
   never notice what it failed to say.

   3. Self-consistency, which needs no oracle. The summary line counts what the
   body shows; a shortened report's elision count closes the arithmetic; the
   exit status matches the verdict printed above it; the same pair reports the
   same bytes twice.

   No browser. The browser is the arbiter for "is this reported difference real"
   under [--diff=canonical], and that question is worth asking, but the pairs
   worth asking it about are the ones a transform README calls cascade-neutral
   produced, and for those the neutrality is already established: README names
   them, and [canonical_agree] renders the split shape against its source on
   every run. Constructing them here and asserting the verdict is [No_diff]
   tests the same claim without a browser launch. What that leaves uncovered is
   an over-report on a pair nobody predicted to be neutral, which is the
   browser's to find and is not asked here.

   Every disagreement is reported, never the first only, and a run that
   exercises no pair fails rather than passing quietly. *)

open Cascade
module C = Cascade_diff.Css_compare
module D = Cascade_diff.Tree_diff

let contains haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec at i =
    i + n <= h && (String.equal (String.sub haystack i n) needle || at (i + 1))
  in
  n = 0 || at 0

(* ===== Reading a stylesheet the way a report names it ===== *)

(* The report spells a value with [Css.declaration_value ~minify:false] and the
   [!important] flag, and a selector with [Css.Selector.to_string]. An index
   built the same way answers whether a claim about a sheet is true of it. Both
   are serialisers, not the code under test. *)
let shown_value decl =
  let v = Css.declaration_value ~minify:false decl in
  if Css.declaration_is_important decl then
    String.concat "" [ v; " !important" ]
  else v

type rule_entry = { selector : string; props : (string * string) list }

(* A statement no selector names is named by its text up to the block, as the
   report names it. *)
let head stmt =
  let text = String.trim (Css.to_string ~minify:true (Css.v [ stmt ])) in
  match String.index_opt text '{' with
  | Some i -> String.trim (String.sub text 0 i)
  | None -> text

let block_of stmt =
  match Css.as_rule stmt with
  | Some (_, _, nested) -> nested
  | None -> (
      match Css.as_layer stmt with
      | Some (_, block) -> block
      | None -> (
          match Css.as_media stmt with
          | Some (_, block) -> block
          | None -> (
              match Css.as_container stmt with
              | Some (_, _, block) -> block
              | None -> (
                  match Css.as_supports stmt with
                  | Some (_, block) -> block
                  | None -> (
                      match Css.as_origin stmt with
                      | Some (_, block) -> block
                      | None -> [])))))

let rec heads_of stmts =
  List.concat_map (fun stmt -> head stmt :: heads_of (block_of stmt)) stmts

type sheet_index = {
  rules : rule_entry list;
  heads : string list;
  source : string;  (** the file as written, for a head the printer drops *)
}

(* [Css.map] reaches every rule at any depth, depth-first, so the index is the
   sheet's rules in document order however they are nested. *)
let rule_index sheet =
  let acc = ref [] in
  ignore
    (Css.map
       (fun selector decls ->
         acc :=
           {
             selector = Css.Selector.to_string selector;
             props =
               List.map (fun d -> (Css.declaration_name d, shown_value d)) decls;
           }
           :: !acc;
         Css.rule ~selector decls)
       (Css.statements sheet));
  List.rev !acc

(* A frame of [@keyframes] is not a rule, and the report names it by its
   keyframe selector, so the index carries one entry per frame. *)
let frames_of sheet =
  let acc = ref [] in
  Css.Stylesheet.iter_statements
    (fun stmt ->
      match Css.as_keyframes stmt with
      | None -> ()
      | Some (_, frames) ->
          List.iter
            (fun (f : Css.keyframe) ->
              acc :=
                {
                  selector = Css.Keyframe.to_string f.selector;
                  props =
                    List.map
                      (fun d -> (Css.declaration_name d, shown_value d))
                      f.declarations;
                }
                :: !acc)
            frames)
    (Css.statements sheet);
  List.rev !acc

(* Every statement the sheet holds, whatever at-rule nests it, named the way a
   report with no selector to show names it. *)
let all_heads sheet =
  let acc = ref [] in
  Css.Stylesheet.iter_statements
    (fun stmt -> acc := head stmt :: !acc)
    (Css.statements sheet);
  List.rev !acc

let index ~source sheet =
  {
    rules = rule_index sheet @ frames_of sheet;
    heads = heads_of (Css.statements sheet) @ all_heads sheet;
    source;
  }

let parse text =
  match Css.of_string ~strict:false text with
  | Ok { stylesheet; _ } -> Some stylesheet
  | Error _ -> None
  | exception (Reader.Parse_error _ | Failure _ | Invalid_argument _) -> None

let index_of_text text = Option.map (index ~source:text) (parse text)

(* Compared without whitespace: a spelling difference in the layout is not a
   difference in what is named. *)
let tight s =
  String.concat ""
    (List.filter_map
       (fun c ->
         if Char.equal c ' ' || Char.equal c '\n' || Char.equal c '\t' then None
         else Some (String.make 1 c))
       (List.init (String.length s) (String.get s)))

(* Mode [`Canonical] compares two generated canonical forms, and the tree it
   reports describes those, not the files as written: a value the author spelled
   [blue] is named [#00f]. A claim about that pair is checked against the file
   and its canonical form together, so the check still catches a name in neither
   and does not fail on the projection's own spelling. *)
let union a b =
  {
    rules = a.rules @ b.rules;
    heads = a.heads @ b.heads;
    source = String.concat "\n" [ a.source; b.source ];
  }

let index_for (mode : C.mode) text =
  match (mode, index_of_text text) with
  | `Canonical, Some idx -> (
      match parse text with
      | None -> Some idx
      | Some sheet -> (
          match Css.to_string ~minify:true (Css.optimize sheet) with
          | canonical -> (
              match index_of_text canonical with
              | None -> Some idx
              | Some c -> Some (union idx c))
          | exception (Reader.Parse_error _ | Failure _ | Invalid_argument _) ->
              Some idx))
  | (`Auto | `Tree | `String | `Canonical), idx -> idx

(* An at-rule with no selector is named by its own text. [@charset] is dropped
   by the printer, so the index cannot hold its head; the file it came from
   still does, and that is the claim being checked. *)
let has_pair (s, p) l =
  List.exists (fun (s', p') -> String.equal s s' && String.equal p p') l

let has_selector idx s =
  List.exists (fun e -> String.equal e.selector s) idx.rules
  || List.exists (String.equal s) idx.heads
  || String.length s > 0
     && Char.equal s.[0] '@'
     && contains (tight idx.source) (tight s)

let has_decl idx s p v =
  List.exists
    (fun e ->
      String.equal e.selector s
      && List.exists
           (fun (p', v') -> String.equal p p' && String.equal v v')
           e.props)
    idx.rules

(* ===== A seeded model of a stylesheet, so a mutation has an address ===== *)

type mdecl = { prop : string; value : string; important : bool }
type mrule = { sel : string; decls : mdecl list }
type mblock = Flat of mrule | Grouped of { cond : string; rules : mrule list }
type rng = { mutable state : int }

let rng seed = { state = seed * 2654435761 land max_int }

let next r =
  r.state <- ((r.state * 2862933555777941757) + 3037000493) land max_int;
  r.state

let int r n = if n <= 1 then 0 else next r mod n
let pick r l = List.nth l (int r (List.length l))

(* Selectors are drawn without replacement, so a mutation's site is one rule and
   naming it names nothing else. Sheets that repeat a selector reach the claim
   arm through the corpus instead. *)
let selector_pool =
  [
    ".a";
    ".b";
    ".card";
    "div";
    "p";
    "#lead";
    "span";
    "li";
    ".a .b";
    ".a > .b";
    "p:first-child";
    "ul li";
    ".x";
    ".y";
    ".z";
    "h1";
  ]

(* Values a boring reader accepts and a printer keeps. Whatever it does to the
   spelling, the truth is read back from the parsed sheet, so a normalisation
   here is harmless; a normalisation that makes the two sides equal is caught
   and the case is dropped. *)
let property_pool =
  [
    ("color", [ "red"; "blue"; "#123456" ]);
    ("background-color", [ "lime"; "black" ]);
    ("width", [ "1px"; "2px"; "50%" ]);
    ("height", [ "3px"; "4px" ]);
    ("margin-top", [ "1px"; "5px" ]);
    ("padding-left", [ "2px"; "6px" ]);
    ("display", [ "block"; "flex"; "inline-block" ]);
    ("z-index", [ "1"; "2" ]);
    ("font-weight", [ "700"; "400" ]);
    ("overflow-x", [ "hidden"; "auto" ]);
    ("border-top-width", [ "1px"; "3px" ]);
    ("opacity", [ "0.5"; "1" ]);
  ]

let assoc_values prop =
  Option.map snd
    (List.find_opt (fun (p, _) -> String.equal p prop) property_pool)

let conditions =
  [ "@media (min-width:400px)"; "@media print"; "@supports (display:flex)" ]

let take n l = List.filteri (fun i _ -> i < n) l

let shuffle r l =
  List.map (fun x -> (int r 1000000, x)) l
  |> List.sort (fun (a, _) (b, _) -> Int.compare a b)
  |> List.map snd

(* A shorthand written beside a longhand of its own family, in either order: the
   run the optimizer rewrites, and the one a report has to keep apart. *)
let family_pool =
  [
    [ ("margin", "1px 2px"); ("margin-top", "5px") ];
    [ ("overflow", "hidden"); ("overflow-x", "auto") ];
    [ ("padding", "3px"); ("padding-left", "6px") ];
    [ ("gap", "4px"); ("row-gap", "8px") ];
  ]

let model_decls r =
  let props = take (1 + int r 3) (shuffle r property_pool) in
  let plain =
    List.map
      (fun (prop, vs) -> { prop; value = pick r vs; important = int r 4 = 0 })
      props
  in
  (* A property written twice, the second write beside the first: a fallback
     chain, where occurrence n on one side answers occurrence n on the other.
     Nothing else in the sweep writes one. *)
  let plain =
    if int r 3 <> 0 then plain
    else
      List.concat_map
        (fun d ->
          match assoc_values d.prop with
          | Some vs -> (
              match List.filter (fun v -> not (String.equal v d.value)) vs with
              | alt :: _ -> [ d; { d with value = alt; important = false } ]
              | [] -> [ d ])
          | None -> [ d ])
        (take 1 plain)
      @ List.filteri (fun i _ -> i > 0) plain
  in
  let family =
    if int r 2 = 0 then
      let f = pick r family_pool in
      let f = if int r 2 = 0 then List.rev f else f in
      List.map (fun (prop, value) -> { prop; value; important = false }) f
    else []
  in
  family @ plain

let model ~seed =
  let r = rng seed in
  let n = 3 + int r 5 in
  let sels = take n (shuffle r selector_pool) in
  let rules = List.map (fun sel -> { sel; decls = model_decls r }) sels in
  (* One block groups a run of rules, so a mutation lands inside a container as
     well as beside one. *)
  let grouped = if int r 3 = 0 then 1 + int r 2 else 0 in
  let rec build i = function
    | [] -> []
    | rest when i = 1 && grouped > 0 && List.length rest > grouped ->
        let inner = take grouped rest in
        let after = List.filteri (fun k _ -> k >= grouped) rest in
        Grouped { cond = pick r conditions; rules = inner }
        :: build (i + 1) after
    | rule :: rest -> Flat rule :: build (i + 1) rest
  in
  build 0 rules

let render_rule buf rule =
  Buffer.add_string buf rule.sel;
  Buffer.add_char buf '{';
  List.iteri
    (fun i d ->
      if i > 0 then Buffer.add_char buf ';';
      Buffer.add_string buf d.prop;
      Buffer.add_char buf ':';
      Buffer.add_string buf d.value;
      if d.important then Buffer.add_string buf "!important")
    rule.decls;
  Buffer.add_string buf "}\n"

let render m =
  let buf = Buffer.create 512 in
  List.iter
    (function
      | Flat rule -> render_rule buf rule
      | Grouped { cond; rules } ->
          Buffer.add_string buf cond;
          Buffer.add_string buf "{\n";
          List.iter (render_rule buf) rules;
          Buffer.add_string buf "}\n")
    m;
  Buffer.contents buf

(* Document order over the model's rules, the order [index] walks the parsed
   sheet in. *)
let model_rules m =
  List.concat_map
    (function Flat rule -> [ rule ] | Grouped { rules; _ } -> rules)
    m

let map_model_rules f m =
  let i = ref (-1) in
  let one rule =
    incr i;
    f !i rule
  in
  List.map
    (function
      | Flat rule -> Flat (one rule)
      | Grouped g -> Grouped { g with rules = List.map one g.rules })
    m

let filter_model_rules keep m =
  let i = ref (-1) in
  let one rule =
    incr i;
    if keep !i then [ rule ] else []
  in
  List.filter_map
    (function
      | Flat rule -> ( match one rule with [] -> None | _ -> Some (Flat rule))
      | Grouped g -> (
          match List.concat_map one g.rules with
          | [] -> None
          | rules -> Some (Grouped { g with rules })))
    m

(* The condition of the block holding rule [i], when a block holds it. A report
   that reaches that rule names its container too, and that name is not an
   over-report. *)
let container_of m i =
  let k = ref (-1) in
  let found = ref None in
  List.iter
    (function
      | Flat _ -> incr k
      | Grouped { cond; rules } ->
          List.iter
            (fun _ ->
              incr k;
              if !k = i then found := Some cond)
            rules)
    m;
  !found

(* ===== Mutations, each with the site it touched ===== *)

type mutation =
  | Change_value of { rule : int; decl : int; value : string }
  | Drop_decl of { rule : int; decl : int }
  | Add_decl of { rule : int; prop : string; value : string }
  | Drop_rule of int
  | Add_rule of { sel : string; prop : string; value : string }
  | Swap_rules of int  (** top-level block [i] with block [i+1] *)
  | Rename_selector of { rule : int; sel : string }
  | Swap_decls of { rule : int; decl : int }
      (** two writes of one property swapped, so the cascade changes *)
  | Toggle_important of { rule : int; decl : int }
  | Drop_container of int  (** a whole conditional block *)

let apply m = function
  | Change_value { rule; decl; value } ->
      map_model_rules
        (fun i r ->
          if i <> rule then r
          else
            {
              r with
              decls =
                List.mapi
                  (fun k d -> if k = decl then { d with value } else d)
                  r.decls;
            })
        m
  | Drop_decl { rule; decl } ->
      map_model_rules
        (fun i r ->
          if i <> rule then r
          else { r with decls = List.filteri (fun k _ -> k <> decl) r.decls })
        m
  | Add_decl { rule; prop; value } ->
      map_model_rules
        (fun i r ->
          if i <> rule then r
          else
            { r with decls = r.decls @ [ { prop; value; important = false } ] })
        m
  | Drop_rule j -> filter_model_rules (fun i -> i <> j) m
  | Add_rule { sel; prop; value } ->
      m @ [ Flat { sel; decls = [ { prop; value; important = false } ] } ]
  | Swap_rules j ->
      List.mapi
        (fun i b ->
          if i = j then List.nth m (j + 1)
          else if i = j + 1 then List.nth m j
          else b)
        m
  | Rename_selector { rule; sel } ->
      map_model_rules (fun i r -> if i <> rule then r else { r with sel }) m
  | Swap_decls { rule; decl } ->
      map_model_rules
        (fun i r ->
          if i <> rule then r
          else
            let at k = List.nth r.decls k in
            {
              r with
              decls =
                List.mapi
                  (fun k d ->
                    if k = decl then at (decl + 1)
                    else if k = decl + 1 then at decl
                    else d)
                  r.decls;
            })
        m
  | Toggle_important { rule; decl } ->
      map_model_rules
        (fun i r ->
          if i <> rule then r
          else
            {
              r with
              decls =
                List.mapi
                  (fun k d ->
                    if k = decl then { d with important = not d.important }
                    else d)
                  r.decls;
            })
        m
  | Drop_container j -> List.filteri (fun i _ -> i <> j) m

let label = function
  | Change_value { rule; decl; _ } ->
      String.concat ""
        [ "change-value-"; string_of_int rule; "-"; string_of_int decl ]
  | Drop_decl { rule; decl } ->
      String.concat ""
        [ "drop-decl-"; string_of_int rule; "-"; string_of_int decl ]
  | Add_decl { rule; _ } -> String.concat "" [ "add-decl-"; string_of_int rule ]
  | Drop_rule j -> String.concat "" [ "drop-rule-"; string_of_int j ]
  | Add_rule _ -> "add-rule"
  | Swap_rules j -> String.concat "" [ "swap-rules-"; string_of_int j ]
  | Rename_selector { rule; _ } ->
      String.concat "" [ "rename-selector-"; string_of_int rule ]
  | Swap_decls { rule; decl } ->
      String.concat ""
        [ "swap-decls-"; string_of_int rule; "-"; string_of_int decl ]
  | Toggle_important { rule; decl } ->
      String.concat ""
        [ "toggle-important-"; string_of_int rule; "-"; string_of_int decl ]
  | Drop_container j -> String.concat "" [ "drop-container-"; string_of_int j ]

(* Every mutation the model admits, so the sweep is the model's own shape rather
   than a hand-picked list. *)
let mutations m =
  let rules = model_rules m in
  let nrules = List.length rules in
  let blocks = List.length m in
  let unused =
    List.filter
      (fun s -> not (List.exists (fun r -> String.equal r.sel s) rules))
      selector_pool
  in
  let values =
    List.concat
      (List.mapi
         (fun i r ->
           List.concat
             (List.mapi
                (fun k d ->
                  (* A value the declaration does not already hold, so the
                     mutation is one. *)
                  match
                    List.filter
                      (fun v -> not (String.equal v d.value))
                      (Option.value ~default:[] (assoc_values d.prop))
                  with
                  | [] -> []
                  | v :: _ -> [ Change_value { rule = i; decl = k; value = v } ])
                r.decls))
         rules)
  in
  let drops =
    List.concat
      (List.mapi
         (fun i r ->
           if List.length r.decls < 2 then []
           else List.mapi (fun k _ -> Drop_decl { rule = i; decl = k }) r.decls)
         rules)
  in
  let adds =
    List.filter_map
      (fun i ->
        let r = List.nth rules i in
        match
          List.filter
            (fun (p, _) ->
              not (List.exists (fun d -> String.equal d.prop p) r.decls))
            property_pool
        with
        | [] -> None
        | (prop, v :: _) :: _ -> Some (Add_decl { rule = i; prop; value = v })
        | (_, []) :: _ -> None)
      (List.init nrules (fun i -> i))
  in
  let rule_drops = List.init nrules (fun i -> Drop_rule i) in
  let renames =
    match unused with
    | [] -> []
    | sel :: _ -> List.init nrules (fun i -> Rename_selector { rule = i; sel })
  in
  let rule_adds =
    match unused with
    | [] -> []
    | sel :: _ -> [ Add_rule { sel; prop = "color"; value = "red" } ]
  in
  (* Only two adjacent flat rules: swapping a group with a rule moves a
     container, which the container arm covers and the rule sites do not. *)
  let swaps =
    List.filter_map
      (fun j ->
        match (List.nth m j, List.nth m (j + 1)) with
        | Flat a, Flat b when not (String.equal a.sel b.sel) ->
            Some (Swap_rules j)
        | _ -> None)
      (List.init (max 0 (blocks - 1)) (fun j -> j))
  in
  (* Only two writes of one property: swapping disjoint declarations changes no
     computed value, so a report that stays silent about it is right. *)
  let decl_swaps =
    List.concat
      (List.mapi
         (fun i r ->
           List.filteri
             (fun k _ -> k + 1 < List.length r.decls)
             (List.mapi (fun k _ -> k) r.decls)
           |> List.filter_map (fun k ->
               let a = List.nth r.decls k and b = List.nth r.decls (k + 1) in
               if
                 String.equal a.prop b.prop
                 && not (String.equal a.value b.value)
               then Some (Swap_decls { rule = i; decl = k })
               else None))
         rules)
  in
  let importants =
    List.concat
      (List.mapi
         (fun i r ->
           List.mapi
             (fun k _ -> Toggle_important { rule = i; decl = k })
             r.decls)
         rules)
  in
  let container_drops =
    List.filter_map
      (fun j ->
        match List.nth m j with
        | Grouped _ -> Some (Drop_container j)
        | Flat _ -> None)
      (List.init blocks (fun j -> j))
  in
  values @ drops @ adds @ rule_drops @ renames @ rule_adds @ swaps @ decl_swaps
  @ importants @ container_drops

(* ===== The site a mutation touched, read from the two parsed sheets ===== *)

type site = {
  selectors : string list;  (** every selector the change touched *)
  containers : string list;  (** the container conditions it reached *)
  value_change : (string * string * string * string) option;
      (** selector, property, expected value, actual value *)
  gone : (string * string) option;  (** selector, property removed *)
  came : (string * string) option;  (** selector, property added *)
}

let empty_site =
  {
    selectors = [];
    containers = [];
    value_change = None;
    gone = None;
    came = None;
  }

let nth_opt l i = List.nth_opt l i

(* The site, or [None] when the parsed sheets do not line up with the model - a
   declaration the reader refused, a value two spellings of one thing. Such a
   case is dropped from the ground-truth arm and counted, never assumed. *)
let site_of ~before ~after m mutation =
  let cont i = Option.to_list (container_of m i) in
  let sel_before i = Option.map (fun e -> e.selector) (nth_opt before i) in
  let sel_after i = Option.map (fun e -> e.selector) (nth_opt after i) in
  match mutation with
  | Change_value { rule; decl; _ } -> (
      match (nth_opt before rule, nth_opt after rule) with
      | Some e, Some a -> (
          match (nth_opt e.props decl, nth_opt a.props decl) with
          | Some (p1, v1), Some (p2, v2)
            when String.equal p1 p2
                 && (not (String.equal v1 v2))
                 && String.equal e.selector a.selector ->
              Some
                {
                  empty_site with
                  selectors = [ e.selector ];
                  containers = cont rule;
                  value_change = Some (e.selector, p1, v1, v2);
                }
          | _ -> None)
      | _ -> None)
  | Drop_decl { rule; decl } -> (
      match (nth_opt before rule, nth_opt after rule) with
      | Some e, Some a
        when String.equal e.selector a.selector
             && List.length a.props = List.length e.props - 1 -> (
          match nth_opt e.props decl with
          | Some (p, _) ->
              Some
                {
                  empty_site with
                  selectors = [ e.selector ];
                  containers = cont rule;
                  gone = Some (e.selector, p);
                }
          | None -> None)
      | _ -> None)
  | Add_decl { rule; prop; _ } -> (
      match (nth_opt before rule, nth_opt after rule) with
      | Some e, Some a
        when String.equal e.selector a.selector
             && List.length a.props = List.length e.props + 1 ->
          Some
            {
              empty_site with
              selectors = [ e.selector ];
              containers = cont rule;
              came = Some (e.selector, prop);
            }
      | _ -> None)
  | Drop_rule j -> (
      match sel_before j with
      | Some s when List.length after = List.length before - 1 ->
          Some { empty_site with selectors = [ s ]; containers = cont j }
      | _ -> None)
  | Add_rule { sel; _ } ->
      if List.length after = List.length before + 1 then
        Some { empty_site with selectors = [ sel ] }
      else None
  | Rename_selector { rule; sel } -> (
      match (sel_before rule, sel_after rule) with
      | Some old, Some now
        when String.equal now sel && not (String.equal old now) ->
          Some
            { empty_site with selectors = [ old; now ]; containers = cont rule }
      | _ -> None)
  | Swap_decls { rule; decl } -> (
      match (nth_opt before rule, nth_opt after rule) with
      | Some e, Some a
        when String.equal e.selector a.selector
             && List.length e.props = List.length a.props -> (
          match (nth_opt e.props decl, nth_opt a.props decl) with
          | Some (p1, v1), Some (p2, v2)
            when String.equal p1 p2 && not (String.equal v1 v2) ->
              Some
                {
                  empty_site with
                  selectors = [ e.selector ];
                  containers = cont rule;
                }
          | _ -> None)
      | _ -> None)
  | Toggle_important { rule; decl } -> (
      match (nth_opt before rule, nth_opt after rule) with
      | Some e, Some a
        when String.equal e.selector a.selector
             && List.length e.props = List.length a.props -> (
          match (nth_opt e.props decl, nth_opt a.props decl) with
          | Some (p1, v1), Some (p2, v2)
            when String.equal p1 p2 && not (String.equal v1 v2) ->
              Some
                {
                  empty_site with
                  selectors = [ e.selector ];
                  containers = cont rule;
                  value_change = Some (e.selector, p1, v1, v2);
                }
          | _ -> None)
      | _ -> None)
  | Drop_container j -> (
      match List.nth_opt m j with
      | Some (Grouped { cond; rules }) ->
          Some
            {
              empty_site with
              selectors =
                (* The selectors of the block, as the parsed sheet spells them:
                   the block's rules are contiguous in document order. *)
                (let start =
                   List.length (model_rules (List.filteri (fun i _ -> i < j) m))
                 in
                 List.filter_map
                   (fun k ->
                     Option.map
                       (fun e -> e.selector)
                       (nth_opt before (start + k)))
                   (List.init (List.length rules) (fun k -> k)));
              containers = [ cond ];
            }
      | Some (Flat _) | None -> None)
  | Swap_rules _ -> (
      (* The two swapped rules are the two indices whose selector moved. *)
      let moved =
        List.concat
          (List.mapi
             (fun i e ->
               match nth_opt after i with
               | Some a when not (String.equal a.selector e.selector) ->
                   [ e.selector; a.selector ]
               | _ -> [])
             before)
      in
      match moved with
      | [] -> None
      | _ ->
          Some
            { empty_site with selectors = List.sort_uniq String.compare moved })

(* ===== What a report names ===== *)

let rec container_names (c : D.container_diff) =
  match c with
  | Added i | Removed i -> [ i.condition ]
  | Reordered { info; _ } -> [ info.condition ]
  | Block_structure_changed { condition; _ } -> [ condition ]
  | Modified { info; container_changes; _ } ->
      info.condition :: List.concat_map container_names container_changes

let rule_names (r : D.rule_diff) =
  match r with
  | Added { selector; _ }
  | Removed { selector; _ }
  | Rearranged { selector; _ }
  | Reordered { selector; _ }
  | Content_changed { selector; _ } ->
      [ selector ]
  | Selector_changed { old_selector; new_selector; _ } ->
      [ old_selector; new_selector ]
  | Regrouped { from_selectors; to_selectors } -> from_selectors @ to_selectors

(* A container that came or went names every rule it carries, so those rules are
   named by the report even though no rule entry mentions them. *)
let carried stmts =
  List.concat_map
    (fun stmt -> List.map (fun e -> e.selector) (rule_index (Css.v [ stmt ])))
    stmts

let rec container_rule_names (c : D.container_diff) =
  match c with
  | Added info | Removed info | Reordered { info; _ } -> carried info.rules
  | Block_structure_changed { expected_blocks; actual_blocks; _ } ->
      List.concat_map (fun (_, stmts) -> carried stmts) expected_blocks
      @ List.concat_map (fun (_, stmts) -> carried stmts) actual_blocks
  | Modified { rule_changes; container_changes; _ } ->
      List.concat_map rule_names rule_changes
      @ List.concat_map container_rule_names container_changes

(* An entry that asserts nothing: a rule reported as modified whose before and
   after are the same declarations and whose three change lists are empty. It
   names a rule that did not change, it is counted by the summary and written
   into the JSON document, and the human report cannot print it. *)
let vacuous (r : D.rule_diff) =
  match r with
  | Content_changed
      {
        selector;
        old_declarations;
        new_declarations;
        property_changes = [];
        added_properties = [];
        removed_properties = [];
      }
    when List.length old_declarations = List.length new_declarations
         && List.for_all2
              (fun a b ->
                String.equal (Css.declaration_name a) (Css.declaration_name b)
                && String.equal (shown_value a) (shown_value b))
              old_declarations new_declarations ->
      Some selector
  | _ -> None

let rec container_vacuous (c : D.container_diff) =
  match c with
  | Added _ | Removed _ | Reordered _ | Block_structure_changed _ -> []
  | Modified { rule_changes; container_changes; _ } ->
      List.filter_map vacuous rule_changes
      @ List.concat_map container_vacuous container_changes

let vacuous_entries (d : D.t) =
  List.filter_map vacuous d.rules
  @ List.concat_map container_vacuous d.containers

(* Only the entries that assert something. An entry naming a rule and showing no
   change of it is a finding of its own, reported by [vacuous_entries]; counting
   it here would report the same defect twice, once under the wrong heading. *)
let named_selectors (d : D.t) =
  List.concat_map
    (fun r -> if Option.is_some (vacuous r) then [] else rule_names r)
    d.rules
  @ List.concat_map container_rule_names d.containers
  |> List.filter (fun s -> not (String.equal s ""))
  |> List.sort_uniq String.compare

let named_containers (d : D.t) =
  List.concat_map container_names d.containers |> List.sort_uniq String.compare

(* Every selector a swap also reports as reordered because a swap moves other
   rules past it: the report's own [swapped_with] field. *)
let swap_partners (d : D.t) =
  List.filter_map
    (fun (r : D.rule_diff) ->
      match r with Reordered { swapped_with; _ } -> swapped_with | _ -> None)
    d.rules

(* ===== Claim verification ===== *)

(* Each entry of a report asserts something about the two files. [claims] turns
   one entry into those assertions, each with the text a finding shows. *)

type claim = { text : string; ok : bool }

let claim text ok = { text; ok }
let quoted parts = String.concat "" parts

let decl_claims ~side ~sel ~idx ~what decls =
  List.map
    (fun d ->
      let p = Css.declaration_name d and v = shown_value d in
      claim
        (quoted
           [ what; " "; sel; " { "; p; ": "; v; " } in the "; side; " sheet" ])
        (has_decl idx sel p v))
    decls

let prop_claims ~side ~sel ~idx ~what props =
  List.map
    (fun (p, v) ->
      claim
        (quoted
           [ what; " "; sel; " { "; p; ": "; v; " } in the "; side; " sheet" ])
        (has_decl idx sel p v))
    props

let rule_claims ~expected ~actual (r : D.rule_diff) =
  match r with
  | Added { selector; declarations } ->
      claim
        (quoted [ "added rule "; selector; " is in the actual sheet" ])
        (has_selector actual selector)
      :: decl_claims ~side:"actual" ~sel:selector ~idx:actual ~what:"added"
           declarations
  | Removed { selector; declarations } ->
      claim
        (quoted [ "removed rule "; selector; " is in the expected sheet" ])
        (has_selector expected selector)
      :: decl_claims ~side:"expected" ~sel:selector ~idx:expected
           ~what:"removed" declarations
  | Content_changed
      { selector; property_changes; added_properties; removed_properties; _ }
    when not (String.equal selector "") ->
      claim
        (quoted [ "modified rule "; selector; " is in the expected sheet" ])
        (has_selector expected selector)
      :: claim
           (quoted [ "modified rule "; selector; " is in the actual sheet" ])
           (has_selector actual selector)
      :: List.concat_map
           (fun (c : D.declaration) ->
             [
               claim
                 (quoted
                    [
                      selector;
                      " { ";
                      c.property_name;
                      ": ";
                      c.expected_value;
                      " } in the expected sheet";
                    ])
                 (has_decl expected selector c.property_name c.expected_value);
               claim
                 (quoted
                    [
                      selector;
                      " { ";
                      c.property_name;
                      ": ";
                      c.actual_value;
                      " } in the actual sheet";
                    ])
                 (has_decl actual selector c.property_name c.actual_value);
               claim
                 (quoted
                    [
                      selector;
                      " ";
                      c.property_name;
                      ": the two values it reports differ";
                    ])
                 (not (String.equal c.expected_value c.actual_value));
             ])
           property_changes
      @ prop_claims ~side:"actual" ~sel:selector ~idx:actual ~what:"added"
          added_properties
      @ prop_claims ~side:"expected" ~sel:selector ~idx:expected ~what:"removed"
          removed_properties
  | Content_changed _ -> []
  | Selector_changed { old_selector; new_selector; _ } ->
      [
        claim
          (quoted
             [
               "selector changed from "; old_selector; ", in the expected sheet";
             ])
          (has_selector expected old_selector);
        claim
          (quoted
             [ "selector changed to "; new_selector; ", in the actual sheet" ])
          (has_selector actual new_selector);
      ]
  | Reordered { selector; _ } ->
      [
        claim
          (quoted [ "reordered rule "; selector; " is in the expected sheet" ])
          (has_selector expected selector);
        claim
          (quoted [ "reordered rule "; selector; " is in the actual sheet" ])
          (has_selector actual selector);
      ]
  | Rearranged { selector; declarations } ->
      claim
        (quoted [ "rearranged rule "; selector; " is in the expected sheet" ])
        (has_selector expected selector)
      :: claim
           (quoted [ "rearranged rule "; selector; " is in the actual sheet" ])
           (has_selector actual selector)
      :: decl_claims ~side:"expected" ~sel:selector ~idx:expected ~what:"kept"
           declarations
      @ decl_claims ~side:"actual" ~sel:selector ~idx:actual ~what:"kept"
          declarations
  | Regrouped { from_selectors; to_selectors } ->
      List.map
        (fun s ->
          claim
            (quoted [ "regrouped from "; s; ", in the expected sheet" ])
            (has_selector expected s))
        from_selectors
      @ List.map
          (fun s ->
            claim
              (quoted [ "regrouped to "; s; ", in the actual sheet" ])
              (has_selector actual s))
          to_selectors

(* A container names the rules it carries; those rules are in the sheet the
   container is claimed to be in. *)
let container_rules_claims ~side ~idx ~what stmts =
  List.concat_map
    (fun stmt ->
      List.concat_map
        (fun e ->
          claim
            (quoted
               [
                 what;
                 " container holds ";
                 e.selector;
                 " in the ";
                 side;
                 " sheet";
               ])
            (has_selector idx e.selector)
          :: List.map
               (fun (p, v) ->
                 claim
                   (quoted
                      [
                        what;
                        " container holds ";
                        e.selector;
                        " { ";
                        p;
                        ": ";
                        v;
                        " } in the ";
                        side;
                        " sheet";
                      ])
                   (has_decl idx e.selector p v))
               e.props)
        (rule_index (Css.v [ stmt ])))
    stmts

let rec container_claims ~expected ~actual (c : D.container_diff) =
  match c with
  | Added info ->
      container_rules_claims ~side:"actual" ~idx:actual ~what:"added" info.rules
  | Removed info ->
      container_rules_claims ~side:"expected" ~idx:expected ~what:"removed"
        info.rules
  | Reordered { info; _ } ->
      container_rules_claims ~side:"expected" ~idx:expected ~what:"reordered"
        info.rules
  | Block_structure_changed { expected_blocks; actual_blocks; _ } ->
      List.concat_map
        (fun (_, stmts) ->
          container_rules_claims ~side:"expected" ~idx:expected
            ~what:"restructured" stmts)
        expected_blocks
      @ List.concat_map
          (fun (_, stmts) ->
            container_rules_claims ~side:"actual" ~idx:actual
              ~what:"restructured" stmts)
          actual_blocks
  | Modified { rule_changes; container_changes; _ } ->
      List.concat_map (rule_claims ~expected ~actual) rule_changes
      @ List.concat_map (container_claims ~expected ~actual) container_changes

let claims ~expected ~actual (d : D.t) =
  List.concat_map (rule_claims ~expected ~actual) d.rules
  @ List.concat_map (container_claims ~expected ~actual) d.containers

(* ===== Self-consistency of the printed report ===== *)

let render_report ?entries result =
  let buf = Buffer.create 1024 in
  C.pp_diff ~expected:"expected" ~actual:"actual" ~color:false ?entries buf
    result;
  Buffer.contents buf

let render_summary ~expected_str ~actual_str result =
  let buf = Buffer.create 256 in
  C.pp_stats buf (C.stats ~expected_str ~actual_str result);
  Buffer.contents buf

let lines s = String.split_on_char '\n' s

let starts_with prefix s =
  String.length s >= String.length prefix
  && String.equal (String.sub s 0 (String.length prefix)) prefix

(* A top-level entry opens with the tree connector at column 0; a nested one is
   indented behind its parent's continuation. *)
let top_level_lines body =
  List.filter
    (fun l ->
      starts_with "\xe2\x94\x9c\xe2\x94\x80 " l
      || starts_with "\xe2\x94\x94\xe2\x94\x80 " l)
    (lines body)

(* [...N more differences] closes a shortened report. *)
let elided body =
  List.fold_left
    (fun acc l ->
      if contains l "more difference" then
        match String.index_opt l '.' with
        | None -> acc
        | Some i ->
            let rest = String.sub l i (String.length l - i) in
            let digits =
              String.concat ""
                (List.filter_map
                   (fun c ->
                     if c >= '0' && c <= '9' then Some (String.make 1 c)
                     else None)
                   (List.init (String.length rest) (String.get rest)))
            in
            Option.value ~default:acc (int_of_string_opt digits)
      else acc)
    0 (top_level_lines body)

let stats_total (s : C.stats) =
  s.added_rules + s.removed_rules + s.modified_rules + s.reordered_rules
  + s.rearranged_rules + s.regrouped_rules + s.container_changes

(* ===== Findings ===== *)

type category = Over | Wrong | Miss | Inconsistent | Control

let equal_category a b =
  match (a, b) with
  | Over, Over
  | Wrong, Wrong
  | Miss, Miss
  | Inconsistent, Inconsistent
  | Control, Control ->
      true
  | (Over | Wrong | Miss | Inconsistent | Control), _ -> false

let category_name = function
  | Over -> "over-report"
  | Wrong -> "wrong-name"
  | Miss -> "missed"
  | Inconsistent -> "inconsistent"
  | Control -> "control"

type finding = {
  category : category;
  pair : string;
  detail : string;
  repro : string;
}

let findings : finding list ref = ref []

let report category ~pair ~detail ~repro =
  findings := { category; pair; detail; repro } :: !findings

(* ===== Running the binary, for the exit status ===== *)

let bin = ref None

let write file contents =
  let oc = open_out file in
  output_string oc contents;
  close_out oc

let read_all ic =
  let buf = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_channel buf ic 1
     done
   with End_of_file -> ());
  Buffer.contents buf

let work () =
  let dir =
    Filename.concat (Filename.get_temp_dir_name ()) "cascade-diff-report"
  in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  dir

(* [cascade diff] on two files, as a caller runs it: the status and the
   bytes. *)
let run_cli ~exe ~mode ~extra a b =
  let dir = work () in
  let fa = Filename.concat dir "expected.css" in
  let fb = Filename.concat dir "actual.css" in
  write fa a;
  write fb b;
  let cmd =
    String.concat " "
      ([
         "NO_COLOR=1";
         Filename.quote exe;
         "diff";
         String.concat "" [ "--diff="; mode ];
       ]
      @ extra
      @ [ Filename.quote fa; Filename.quote fb; "2>&1" ])
  in
  let ic = Unix.open_process_in cmd in
  let out = read_all ic in
  let status =
    match Unix.close_process_in ic with
    | Unix.WEXITED n -> n
    | Unix.WSIGNALED n | Unix.WSTOPPED n -> 128 + n
  in
  (status, out)

let cli_repro ~mode ~extra =
  String.concat " "
    ([ "cascade diff"; String.concat "" [ "--diff="; mode ] ]
    @ extra
    @ [ "expected.css actual.css" ])

(* ===== One pair, every check that needs no ground truth ===== *)

let checked_pairs = ref 0
let checked_claims = ref 0
let cli_pairs = ref 0

let diff_of ~mode a b =
  match C.diff ~mode a b with
  | d -> Some d
  | exception (Reader.Parse_error _ | Failure _ | Invalid_argument _) -> None

let show css = if String.length css <= 200 then css else String.sub css 0 200

(* The claims a report makes, the shape of the printed report, and the status
   the binary exits with. [cli] bounds how many pairs pay for a process. *)
let check_pair ~pair ~mode ~cli ~expected_str ~actual_str result =
  incr checked_pairs;
  let mode_name =
    match mode with
    | `Auto -> "auto"
    | `Tree -> "tree"
    | `String -> "string"
    | `Canonical -> "canonical"
  in
  let repro = cli_repro ~mode:mode_name ~extra:[] in
  let bodies =
    String.concat "\n"
      [
        "  expected.css: ";
        show expected_str;
        "  actual.css:   ";
        show actual_str;
      ]
  in
  (* 1. Every claim the report makes is true of the two files. *)
  (match
     ( C.as_tree_diff result,
       index_for mode expected_str,
       index_for mode actual_str )
   with
  | Some d, Some expected, Some actual ->
      List.iter
        (fun c ->
          incr checked_claims;
          if not c.ok then
            report Wrong ~pair
              ~detail:
                (String.concat "\n"
                   [ String.concat "" [ "the report claims "; c.text ]; bodies ])
              ~repro)
        (claims ~expected ~actual d)
  | _ -> ());
  (* 2. The summary counts what the body shows. A layer-order entry prints its
     swaps at the same indentation as a top-level entry, so a report carrying
     one is left out of the arithmetic rather than counted wrong. *)
  (match C.as_tree_diff result with
  | Some d when Option.is_none d.layer_order ->
      let s = C.stats ~expected_str ~actual_str result in
      let body = render_report result in
      let hidden = elided body in
      let printed =
        List.length (top_level_lines body) - if hidden > 0 then 1 else 0
      in
      if printed + hidden <> stats_total s then
        report Inconsistent ~pair
          ~detail:
            (String.concat "\n"
               [
                 String.concat ""
                   [
                     "summary counts ";
                     string_of_int (stats_total s);
                     " top-level difference(s), the report shows ";
                     string_of_int printed;
                     " and elides ";
                     string_of_int hidden;
                   ];
                 String.concat ""
                   [
                     "  summary: ";
                     String.trim
                       (render_summary ~expected_str ~actual_str result);
                   ];
                 bodies;
               ])
          ~repro;
      (* 3. A shortened report closes its own arithmetic. *)
      let total = printed + hidden in
      if total > 1 then begin
        let short = render_report ~entries:1 result in
        let short_hidden = elided short in
        let short_printed =
          List.length (top_level_lines short)
          - if short_hidden > 0 then 1 else 0
        in
        if short_printed + short_hidden <> total then
          report Inconsistent ~pair
            ~detail:
              (String.concat ""
                 [
                   "--limit=1 shows ";
                   string_of_int short_printed;
                   " and elides ";
                   string_of_int short_hidden;
                   ", against ";
                   string_of_int total;
                   " in the full report";
                 ])
            ~repro:(cli_repro ~mode:mode_name ~extra:[ "--limit=1" ])
      end
  | _ -> ());
  (* 4. No entry asserts nothing. *)
  (match C.as_tree_diff result with
  | None -> ()
  | Some d ->
      List.iter
        (fun selector ->
          report Wrong ~pair
            ~detail:
              (String.concat "\n"
                 [
                   String.concat ""
                     [
                       "the report calls ";
                       selector;
                       " modified and shows nothing: the same declarations \
                        before and after, and no property change, added or \
                        removed property";
                     ];
                   bodies;
                 ])
            ~repro)
        (vacuous_entries d));
  (* 5. A difference is a report. *)
  (match result.C.result with
  | C.Tree_diff _ | C.String_diff _ ->
      let body = String.trim (render_report result) in
      if String.equal body "" then
        report Inconsistent ~pair
          ~detail:
            (String.concat "\n"
               [ "the comparison differs and the report is empty"; bodies ])
          ~repro
  | C.No_diff | C.Both_errors _ | C.Expected_error _ | C.Actual_error _ -> ());
  (* 6. The binary's status matches the verdict, and two runs agree. *)
  match (!bin, cli) with
  | Some exe, true ->
      incr cli_pairs;
      let status, out =
        run_cli ~exe ~mode:mode_name ~extra:[] expected_str actual_str
      in
      let status', out' =
        run_cli ~exe ~mode:mode_name ~extra:[] expected_str actual_str
      in
      if status <> status' || not (String.equal out out') then
        report Inconsistent ~pair ~detail:"two runs of the same pair disagree"
          ~repro;
      let differs =
        match result.C.result with
        | C.No_diff -> false
        | C.Tree_diff _ | C.String_diff _ | C.Both_errors _ | C.Expected_error _
        | C.Actual_error _ ->
            true
      in
      if differs && status <> 1 then
        report Inconsistent ~pair
          ~detail:
            (String.concat ""
               [
                 "the comparison reports a difference and the binary exits ";
                 string_of_int status;
                 " (README: differences exit 1)";
               ])
          ~repro
      else if (not differs) && status = 1 then
        report Inconsistent ~pair
          ~detail:"the comparison reports no difference and the binary exits 1"
          ~repro
      else if status > 2 then
        report Inconsistent ~pair
          ~detail:
            (String.concat ""
               [
                 "the binary exits ";
                 string_of_int status;
                 ", outside 0/1/2:\n";
                 out;
               ])
          ~repro
  | _, _ -> ()

(* ===== The ground-truth arm ===== *)

let ground_truth_cases = ref 0
let dropped_cases = ref 0

let check_ground_truth ~pair ~expected_str ~actual_str ~site result =
  match C.as_tree_diff result with
  | None ->
      (* The mutation is a difference, so the walk has to reach it. A string
         diff names no rule at all. *)
      report Miss ~pair
        ~detail:
          (String.concat ""
             [
               "the mutation changed ";
               String.concat ", " site.selectors;
               " and the comparison produced no structural report";
             ])
        ~repro:(cli_repro ~mode:"auto" ~extra:[])
  | Some d -> (
      incr ground_truth_cases;
      let named = named_selectors d in
      let partners = swap_partners d in
      let touched = List.sort_uniq String.compare site.selectors in
      let repro = cli_repro ~mode:"auto" ~extra:[] in
      let bodies =
        String.concat "\n"
          [
            String.concat "" [ "  expected.css: "; show expected_str ];
            String.concat "" [ "  actual.css:   "; show actual_str ];
            String.concat "" [ "  report names: "; String.concat ", " named ];
          ]
      in
      let extra =
        List.filter (fun s -> not (List.exists (String.equal s) touched)) named
      in
      (match extra with
      | [] -> ()
      | _ :: _ ->
          report Over ~pair
            ~detail:
              (String.concat "\n"
                 [
                   String.concat ""
                     [
                       "the mutation touched ";
                       String.concat ", " touched;
                       "; the report also names ";
                       String.concat ", " extra;
                     ];
                   bodies;
                 ])
            ~repro);
      let missing =
        List.filter (fun s -> not (List.exists (String.equal s) named)) touched
      in
      (* A swap names its partner in the entry rather than as an entry of its
         own, and that is naming it. *)
      let missing =
        List.filter
          (fun s -> not (List.exists (String.equal s) partners))
          missing
      in
      (match missing with
      | [] -> ()
      | _ :: _ ->
          report Miss ~pair
            ~detail:
              (String.concat "\n"
                 [
                   String.concat ""
                     [
                       "the mutation touched ";
                       String.concat ", " touched;
                       "; the report does not name ";
                       String.concat ", " missing;
                     ];
                   bodies;
                 ])
            ~repro);
      (* A condition is spelled by the report, not by the sheet: [(a:b)] in the
         source reads back as [(a: b)], and a [@supports] names its condition
         without the at-keyword. Compared without whitespace, and by containment
         so the at-keyword is not the difference. *)
      let site_containers = List.map tight site.containers in
      let containers = List.map tight (named_containers d) in
      let stray =
        List.filter
          (fun c ->
            not
              (List.exists
                 (fun s -> contains s c || contains c s)
                 site_containers))
          containers
      in
      (match (site.containers, stray) with
      | [], _ | _, [] -> ()
      | _ :: _, _ :: _ ->
          report Over ~pair
            ~detail:
              (String.concat "\n"
                 [
                   String.concat ""
                     [
                       "the mutation is inside ";
                       String.concat ", " site.containers;
                       "; the report names container(s) ";
                       String.concat ", " stray;
                     ];
                   bodies;
                 ])
            ~repro);
      (* The values a value change reports are the two the files hold. *)
      (match site.value_change with
      | None -> ()
      | Some (sel, prop, before, after) ->
          let changes =
            List.filter_map
              (fun (r : D.rule_diff) ->
                match r with
                | Content_changed { selector; property_changes; _ }
                  when String.equal selector sel ->
                    Some property_changes
                | _ -> None)
              d.rules
            @ List.concat_map
                (fun c ->
                  let rec go (c : D.container_diff) =
                    match c with
                    | Modified { rule_changes; container_changes; _ } ->
                        List.filter_map
                          (fun (r : D.rule_diff) ->
                            match r with
                            | Content_changed { selector; property_changes; _ }
                              when String.equal selector sel ->
                                Some property_changes
                            | _ -> None)
                          rule_changes
                        @ List.concat_map go container_changes
                    | Added _ | Removed _ | Reordered _
                    | Block_structure_changed _ ->
                        []
                  in
                  go c)
                d.containers
          in
          let all = List.concat changes in
          let matched =
            List.exists
              (fun (c : D.declaration) ->
                String.equal c.property_name prop
                && String.equal c.expected_value before
                && String.equal c.actual_value after)
              all
          in
          if not matched then
            report Wrong ~pair
              ~detail:
                (String.concat "\n"
                   [
                     String.concat ""
                       [
                         sel;
                         " { ";
                         prop;
                         " } went from ";
                         before;
                         " to ";
                         after;
                         "; the report says ";
                         (match all with
                         | [] -> "nothing about it"
                         | _ ->
                             String.concat "; "
                               (List.map
                                  (fun (c : D.declaration) ->
                                    String.concat ""
                                      [
                                        c.property_name;
                                        ": ";
                                        c.expected_value;
                                        " -> ";
                                        c.actual_value;
                                      ])
                                  all));
                       ];
                     bodies;
                   ])
              ~repro);
      (* A dropped or added declaration is named as one, under its selector. *)
      let named_removed =
        List.concat_map
          (fun (r : D.rule_diff) ->
            match r with
            | Content_changed { selector; removed_properties; _ } ->
                List.map (fun (p, _) -> (selector, p)) removed_properties
            | Removed { selector; declarations } ->
                List.map
                  (fun d -> (selector, Css.declaration_name d))
                  declarations
            | _ -> [])
          d.rules
      in
      let named_added =
        List.concat_map
          (fun (r : D.rule_diff) ->
            match r with
            | Content_changed { selector; added_properties; _ } ->
                List.map (fun (p, _) -> (selector, p)) added_properties
            | Added { selector; declarations } ->
                List.map
                  (fun d -> (selector, Css.declaration_name d))
                  declarations
            | _ -> [])
          d.rules
      in
      (match site.gone with
      | Some (sel, p)
        when (not (has_pair (sel, p) named_removed))
             && match site.containers with [] -> true | _ :: _ -> false ->
          report Miss ~pair
            ~detail:
              (String.concat "\n"
                 [
                   String.concat ""
                     [ sel; " lost { "; p; " } and the report does not say so" ];
                   bodies;
                 ])
            ~repro
      | _ -> ());
      match site.came with
      | Some (sel, p)
        when (not (has_pair (sel, p) named_added))
             && match site.containers with [] -> true | _ :: _ -> false ->
          report Miss ~pair
            ~detail:
              (String.concat "\n"
                 [
                   String.concat ""
                     [
                       sel; " gained { "; p; " } and the report does not say so";
                     ];
                   bodies;
                 ])
            ~repro
      | _ -> ())

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

(* ===== Calibration ===== *)

(* Both directions, on controls this harness owns, so a green run is not a blind
   one and a library fix cannot turn a control red. *)
let calibrate () =
  let fail what =
    report Control ~pair:"calibration" ~detail:what
      ~repro:"dune build @test/diff_report/runtest"
  in
  let idx text = Option.get (index_of_text text) in
  (* The claim checker must reject a claim that is false of the two files. *)
  let expected = idx ".a{color:red}" and actual = idx ".a{color:blue}" in
  let bogus : D.rule_diff =
    Removed { selector = ".nowhere"; declarations = [] }
  in
  (match rule_claims ~expected ~actual bogus with
  | [] -> fail "the claim checker says nothing about a Removed entry"
  | cs ->
      if List.for_all (fun c -> c.ok) cs then
        fail "the claim checker accepts a rule neither file holds");
  let wrong_value : D.rule_diff =
    Content_changed
      {
        selector = ".a";
        old_declarations = [];
        new_declarations = [];
        property_changes =
          [
            {
              property_name = "color";
              expected_value = "green";
              actual_value = "blue";
            };
          ];
        added_properties = [];
        removed_properties = [];
      }
  in
  if List.for_all (fun c -> c.ok) (rule_claims ~expected ~actual wrong_value)
  then fail "the claim checker accepts a value the expected file does not hold";
  (* And accept the claims a correct report would make about the same pair. *)
  let right : D.rule_diff =
    Content_changed
      {
        selector = ".a";
        old_declarations = [];
        new_declarations = [];
        property_changes =
          [
            {
              property_name = "color";
              expected_value = "red";
              actual_value = "blue";
            };
          ];
        added_properties = [];
        removed_properties = [];
      }
  in
  if not (List.for_all (fun c -> c.ok) (rule_claims ~expected ~actual right))
  then fail "the claim checker rejects a report that is true of the two files";
  (* The entry-count arithmetic must notice a summary that over-counts. A
     hand-built diff with one Content_changed that shows nothing is one the
     summary counts and the body cannot print. *)
  let silent : D.t =
    {
      rules =
        [
          Content_changed
            {
              selector = ".a";
              old_declarations = [];
              new_declarations = [];
              property_changes = [];
              added_properties = [];
              removed_properties = [];
            };
        ];
      containers = [];
      layer_order = None;
    }
  in
  let buf = Buffer.create 256 in
  D.pp ~color:false buf silent;
  let body = Buffer.contents buf in
  let printed = List.length (top_level_lines body) in
  if printed <> 0 then
    fail "the entry counter sees a top-level entry in a report that prints none";
  (* The counter must see the entries a real report does print. *)
  let two : D.t =
    {
      rules =
        [
          Added { selector = ".a"; declarations = [] };
          Removed { selector = ".b"; declarations = [] };
        ];
      containers = [];
      layer_order = None;
    }
  in
  let buf = Buffer.create 256 in
  D.pp ~color:false buf two;
  if List.length (top_level_lines (Buffer.contents buf)) <> 2 then
    fail "the entry counter miscounts a two-entry report";
  (* The elision reader must read the count a shortened report writes. *)
  let many : D.t =
    {
      rules =
        List.init 5 (fun i : D.rule_diff ->
            Added
              {
                selector = String.concat "" [ ".r"; string_of_int i ];
                declarations = [];
              });
      containers = [];
      layer_order = None;
    }
  in
  let buf = Buffer.create 256 in
  D.pp ~color:false ~entries:2 buf many;
  let body = Buffer.contents buf in
  if elided body <> 3 then
    fail
      (String.concat ""
         [ "the elision reader reads "; string_of_int (elided body); ", not 3" ]);
  if
    elided (render_report (C.diff ~mode:`Tree ".a{color:red}" ".a{color:blue}"))
    <> 0
  then fail "the elision reader invents an elision in a full report";
  (* A pair that differs must be reported, and an identical pair must not. *)
  (match (C.diff ~mode:`Tree ".a{color:red}" ".a{color:blue}").result with
  | C.Tree_diff _ -> ()
  | _ -> fail "a changed value is not a tree diff");
  (match (C.diff ~mode:`Tree ".a{color:red}" ".a{color:red}").result with
  | C.No_diff -> ()
  | _ -> fail "an identical pair is not No_diff");
  (* The ground-truth arm itself, both ways. A site the mutation did not touch
     must produce an over-report and a miss; the true site must produce neither.
     Findings the controls raise are taken back off the list. *)
  let expected_str = ".a{color:red}" and actual_str = ".a{color:blue}" in
  let result = C.diff ~mode:`Auto expected_str actual_str in
  let saved = !findings in
  let run site =
    findings := [];
    check_ground_truth ~pair:"calibration" ~expected_str ~actual_str ~site
      result;
    let got = List.map (fun f -> f.category) !findings in
    findings := saved;
    got
  in
  let truth =
    {
      empty_site with
      selectors = [ ".a" ];
      value_change = Some (".a", "color", "red", "blue");
    }
  in
  let wrong =
    {
      empty_site with
      selectors = [ ".nowhere" ];
      value_change = Some (".a", "color", "green", "blue");
    }
  in
  (match run truth with
  | [] -> ()
  | _ ->
      fail "the ground-truth arm rejects a report that names exactly the site");
  let got = run wrong in
  if not (List.exists (equal_category Over) got) then
    fail "the ground-truth arm misses a selector the mutation did not touch";
  if not (List.exists (equal_category Miss) got) then
    fail "the ground-truth arm misses a site the report does not name";
  if not (List.exists (equal_category Wrong) got) then
    fail "the ground-truth arm accepts a value change the files do not hold"

(* ===== Corpora ===== *)

let ( // ) = Filename.concat

let read_file file =
  let ic = open_in_bin file in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let entries dir =
  match Sys.readdir dir with
  | exception Sys_error _ -> []
  | e ->
      Array.sort String.compare e;
      Array.to_list e

let repo_relative rel =
  let rec up dir fuel =
    if fuel = 0 then None
    else if Sys.file_exists (dir // rel) then Some (dir // rel)
    else
      let parent = Filename.dirname dir in
      if parent = dir then None else up parent (fuel - 1)
  in
  up (Sys.getcwd ()) 12

let corpus_files () =
  match
    repo_relative
      ("test" // "interop" // "css-minify-tests" // "traces" // "tests")
  with
  | None -> []
  | Some root ->
      List.concat_map
        (fun category ->
          let dir = root // category in
          if not (try Sys.is_directory dir with Sys_error _ -> false) then []
          else
            List.filter_map
              (fun id ->
                let file = dir // id // "source.css" in
                if Sys.file_exists file then
                  Some
                    ( String.concat "" [ "corpus-"; category; "-"; id ],
                      read_file file )
                else None)
              (entries dir))
        (entries root)

(* Spread the sample over the whole list rather than taking a prefix, so every
   category is represented. *)
let sample n l =
  let total = List.length l in
  if n <= 0 || total <= n then l
  else
    let step = (total + n - 1) / n in
    List.filteri (fun i _ -> i mod step = 0) l

let example_files () =
  match repo_relative ("test" // "examples") with
  | None -> []
  | Some dir ->
      List.filter_map
        (fun file ->
          if Filename.check_suffix file ".css" then
            Some
              ( String.concat "" [ "example-"; Filename.remove_extension file ],
                read_file (dir // file) )
          else None)
        (entries dir)

(* ===== Arguments ===== *)

let seeds = ref 40
let corpus = ref 0
let cli_budget = ref 200

let usage () =
  Fmt.pr "usage: diff_report [--bin PATH] [--seeds N] [--corpus N] [--cli N]@.";
  exit 0

let parse_args () =
  let n = Array.length Sys.argv in
  let rec loop i =
    if i < n then begin
      let arg () = if i + 1 < n then Sys.argv.(i + 1) else "" in
      (match Sys.argv.(i) with
      | "--bin" -> bin := Some (arg ())
      | "--seeds" ->
          seeds := Option.value ~default:!seeds (int_of_string_opt (arg ()))
      | "--corpus" ->
          corpus := Option.value ~default:!corpus (int_of_string_opt (arg ()))
      | "--cli" ->
          cli_budget :=
            Option.value ~default:!cli_budget (int_of_string_opt (arg ()))
      | "-h" | "--help" -> usage ()
      | _ -> ());
      loop (i + 1)
    end
  in
  loop 1

(* ===== Main ===== *)

let mutation_pairs = ref 0
let neutral_over = ref 0

let () =
  parse_args ();
  calibrate ();
  (* The controls run the same checks, so they move the same counters. *)
  ground_truth_cases := 0;
  checked_pairs := 0;
  checked_claims := 0;
  let cli_left = ref !cli_budget in
  let cli () =
    if !cli_left > 0 then (
      decr cli_left;
      true)
    else false
  in
  (* 1. Constructed ground truth. *)
  List.iter
    (fun seed ->
      let m = model ~seed in
      let expected_str = render m in
      match index_of_text expected_str with
      | None -> incr dropped_cases
      | Some idx_before ->
          let before_all = idx_before.rules in
          let rules = model_rules m in
          (* The model and the parse must line up, or an address means nothing.
             A sheet that does not round-trip is dropped and counted. *)
          let aligned =
            List.length before_all = List.length rules
            && List.for_all2
                 (fun e r ->
                   List.length e.props = List.length r.decls
                   && List.for_all2
                        (fun (p, _) d -> String.equal p d.prop)
                        e.props r.decls)
                 before_all rules
          in
          if not aligned then incr dropped_cases
          else
            List.iter
              (fun mutation ->
                let actual_str = render (apply m mutation) in
                match index_of_text actual_str with
                | None -> incr dropped_cases
                | Some idx_after -> (
                    let after_all = idx_after.rules in
                    match
                      site_of ~before:before_all ~after:after_all m mutation
                    with
                    | None -> incr dropped_cases
                    | Some site -> (
                        incr mutation_pairs;
                        let pair =
                          String.concat ""
                            [ "seed-"; string_of_int seed; "/"; label mutation ]
                        in
                        match diff_of ~mode:`Auto expected_str actual_str with
                        | None -> incr dropped_cases
                        | Some result ->
                            check_pair ~pair ~mode:`Auto ~cli:(cli ())
                              ~expected_str ~actual_str result;
                            check_ground_truth ~pair ~expected_str ~actual_str
                              ~site result)))
              (mutations m))
    (List.init !seeds (fun i -> i + 1));
  (* 1b. One-site changes in the at-rules and the nesting the model cannot
     write, each with the names a report has to carry. *)
  List.iter
    (fun tc ->
      let pair = String.concat "" [ "at-rule/"; tc.case ] in
      incr mutation_pairs;
      match diff_of ~mode:`Auto tc.before tc.after with
      | None -> incr dropped_cases
      | Some result ->
          check_pair ~pair ~mode:`Auto ~cli:true ~expected_str:tc.before
            ~actual_str:tc.after result;
          check_ground_truth ~pair ~expected_str:tc.before ~actual_str:tc.after
            ~site:
              {
                empty_site with
                selectors = tc.names;
                containers = tc.under;
                value_change = tc.holds;
              }
            result)
    text_cases;
  (* 2. Neutral transforms under canonical: README says the verdict is equal. *)
  List.iter
    (fun (name, a, b) ->
      let pair = String.concat "" [ "neutral/"; name ] in
      match diff_of ~mode:`Canonical a b with
      | None -> incr dropped_cases
      | Some result ->
          check_pair ~pair ~mode:`Canonical ~cli:true ~expected_str:a
            ~actual_str:b result;
          (match result.C.result with
          | C.No_diff -> ()
          | _ ->
              incr neutral_over;
              report Over ~pair
                ~detail:
                  (String.concat "\n"
                     [
                       "the two sheets are one stylesheet written two ways and \
                        canonical reports a difference";
                       String.concat "" [ "  expected.css: "; a ];
                       String.concat "" [ "  actual.css:   "; b ];
                       String.concat ""
                         [ "  report: "; String.trim (render_report result) ];
                     ])
                ~repro:(cli_repro ~mode:"canonical" ~extra:[]));
          (* The same pair under the structural modes must still make only true
             claims about the two files. *)
          Option.iter
            (fun r ->
              check_pair
                ~pair:(String.concat "" [ pair; "/tree" ])
                ~mode:`Tree ~cli:false ~expected_str:a ~actual_str:b r)
            (diff_of ~mode:`Tree a b))
    neutral_pairs;
  (* 3. Pairs that do differ, in every mode, so the claim checker sees
     reports. *)
  List.iter
    (fun (name, a, b) ->
      List.iter
        (fun (mode : C.mode) ->
          let mode_name =
            match mode with
            | `Auto -> "auto"
            | `Tree -> "tree"
            | `String -> "string"
            | `Canonical -> "canonical"
          in
          let pair = String.concat "" [ "differing/"; name; "/"; mode_name ] in
          match diff_of ~mode a b with
          | None -> incr dropped_cases
          | Some result ->
              check_pair ~pair ~mode ~cli:true ~expected_str:a ~actual_str:b
                result)
        [ `Auto; `Tree; `Canonical ])
    differing_pairs;
  (* 4. The committed corpora, mutated structurally, for claims about shapes the
     model does not write: nesting, @layer, keyframes, descriptors, vendor
     prefixes, repeated selectors. No ground truth here - the claim arm and the
     report's own arithmetic are what these pairs answer. *)
  List.iter
    (fun (name, text) ->
      match parse text with
      | None -> ()
      | Some sheet ->
          let source = Css.to_string sheet in
          let stmts = Css.statements sheet in
          let n = List.length stmts in
          let step = max 1 (n / 6) in
          let indices =
            List.filter (fun j -> j < n) (List.init 6 (fun j -> j * step))
          in
          let variants =
            List.concat_map
              (fun j ->
                [
                  ( String.concat "" [ "drop-rule-"; string_of_int j ],
                    Css.to_string
                      (Css.v (List.filteri (fun k _ -> k <> j) stmts)) );
                ]
                @
                if j + 1 < n then
                  [
                    ( String.concat "" [ "swap-rule-"; string_of_int j ],
                      Css.to_string
                        (Css.v
                           (List.mapi
                              (fun k s ->
                                if k = j then List.nth stmts (j + 1)
                                else if k = j + 1 then List.nth stmts j
                                else s)
                              stmts)) );
                  ]
                else [])
              indices
          in
          List.iter
            (fun (vlabel, variant) ->
              let pair = String.concat "" [ name; "/"; vlabel ] in
              List.iter
                (fun (mode : C.mode) ->
                  match diff_of ~mode source variant with
                  | None -> incr dropped_cases
                  | Some result ->
                      check_pair ~pair ~mode ~cli:(cli ()) ~expected_str:source
                        ~actual_str:variant result)
                [ `Auto; `Tree ])
            variants)
    (example_files () @ sample !corpus (corpus_files ()));
  (* ===== The run ===== *)
  let all = List.rev !findings in
  let by c = List.filter (fun f -> equal_category c f.category) all in
  List.iter
    (fun c ->
      match by c with
      | [] -> ()
      | fs ->
          Fmt.pr "@.=== %s: %d ===@." (category_name c) (List.length fs);
          List.iter
            (fun f ->
              Fmt.pr "FAIL [%s] %s@.  %s@.  repro: %s@." (category_name c)
                f.pair f.detail f.repro)
            fs)
    [ Control; Over; Wrong; Miss; Inconsistent ];
  Fmt.pr "@.diff_report: %d pair(s) checked, %d claim(s) verified@."
    !checked_pairs !checked_claims;
  Fmt.pr "  ground truth: %d mutation pair(s), %d structural report(s)@."
    !mutation_pairs !ground_truth_cases;
  Fmt.pr "  neutral pairs: %d, of which canonical calls %d different@."
    (List.length neutral_pairs)
    !neutral_over;
  Fmt.pr "  binary runs: %d pair(s)%s@." !cli_pairs
    (if Option.is_none !bin then " (no --bin: the exit status is unchecked)"
     else "");
  Fmt.pr "  cases dropped for want of an address: %d@." !dropped_cases;
  let bad = List.length all in
  (* A run that verifies nothing is not a clean run. The model alone produces
     hundreds of mutation pairs on any seed, so a small population means the
     harness stopped working rather than that the library got better. *)
  let blind =
    !mutation_pairs < 100 || !checked_claims < 100 || !ground_truth_cases < 100
  in
  if blind then
    Fmt.pr "FAIL: the population is too small to stand for anything@.";
  Fmt.pr "  findings: %d@." bad;
  if bad > 0 || blind then exit 1
