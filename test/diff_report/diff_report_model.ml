open Diff_report_index

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
