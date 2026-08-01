open Cascade

(* A selector component that cannot hold in a synthesised document raises
   [Skip], carrying the reason the report groups it under. *)
exception Skip of string

type node = {
  tag : string;
  id : string option;
  classes : string list;
  attrs : (string * string) list;
  children : node list;
}

(* Where the element must sit among its siblings. *)
type pos = Anywhere | First | Last | Only | Nth of int | Nth_last of int

type spec = {
  mutable tag : string option;
  mutable id : string option;
  mutable classes : string list;
  mutable attrs : (string * string) list;
  mutable pos : pos;
  mutable of_type : bool; (* the position counts same-tag siblings only *)
  mutable childless : bool; (* :empty *)
  mutable root : bool; (* :root / :scope *)
  mutable avoid : string list; (* tags :not() rules out *)
  mutable inner : node list; (* :has() content *)
  mutable pseudos : string list; (* pseudo-elements to sample *)
}

type t = {
  root_node : node;
  html_classes : string list;
  html_attrs : (string * string) list;
  doc_pseudos : string list;
  probes : string list;
  n_selectors : int;
  n_synthesised : int;
  n_elements : int;
  skips : (string * int) list;
  examples : (string * string) list;
}

let new_spec () =
  {
    tag = None;
    id = None;
    classes = [];
    attrs = [];
    pos = Anywhere;
    of_type = false;
    childless = false;
    root = false;
    avoid = [];
    inner = [];
    pseudos = [];
  }

let restore sp saved =
  sp.tag <- saved.tag;
  sp.id <- saved.id;
  sp.classes <- saved.classes;
  sp.attrs <- saved.attrs;
  sp.pos <- saved.pos;
  sp.of_type <- saved.of_type;
  sp.childless <- saved.childless;
  sp.root <- saved.root;
  sp.avoid <- saved.avoid;
  sp.inner <- saved.inner;
  sp.pseudos <- saved.pseudos

let is_pseudo_element = function
  | Selector.Before _ | Selector.After _ | Selector.First_line _
  | Selector.First_letter _ | Selector.Marker | Selector.Placeholder
  | Selector.Selection | Selector.Backdrop | Selector.File_selector_button
  | Selector.Target_text | Selector.Spelling_error | Selector.Grammar_error
  | Selector.Highlight _ | Selector.Part _ | Selector.Slotted _ | Selector.Cue _
  | Selector.Cue_region _ | Selector.View_transition
  | Selector.View_transition_group _ | Selector.View_transition_image_pair _
  | Selector.View_transition_old _ | Selector.View_transition_new _
  | Selector.Unknown_pseudo_element _ | Selector.Unknown_pseudo_element_call _
  | Selector.Moz_placeholder | Selector.Webkit_input_placeholder
  | Selector.Ms_input_placeholder | Selector.Webkit_scrollbar
  | Selector.Webkit_search_cancel_button | Selector.Webkit_search_decoration
  | Selector.Webkit_details_marker | Selector.Details_content
  | Selector.Webkit_datetime_edit_fields_wrapper
  | Selector.Webkit_date_and_time_value | Selector.Webkit_datetime_edit
  | Selector.Webkit_datetime_edit_year_field
  | Selector.Webkit_datetime_edit_month_field
  | Selector.Webkit_datetime_edit_day_field
  | Selector.Webkit_datetime_edit_hour_field
  | Selector.Webkit_datetime_edit_minute_field
  | Selector.Webkit_datetime_edit_second_field
  | Selector.Webkit_datetime_edit_millisecond_field
  | Selector.Webkit_datetime_edit_meridiem_field
  | Selector.Webkit_inner_spin_button | Selector.Webkit_outer_spin_button
  | Selector.Webkit_calendar_picker_indicator ->
      true
  | _ -> false

(* querySelectorAll never matches a pseudo-element, so the coverage probe tests
   the element the rule is attached to. *)
let rec strip_pseudo_elements sel =
  match sel with
  | Selector.Compound parts -> (
      match List.filter (fun p -> not (is_pseudo_element p)) parts with
      | [] -> Selector.Universal None
      | [ one ] -> strip_pseudo_elements one
      | parts -> Selector.Compound parts)
  | Selector.Combined (l, c, r) ->
      Selector.Combined (strip_pseudo_elements l, c, strip_pseudo_elements r)
  | Selector.List l -> Selector.List (List.map strip_pseudo_elements l)
  | s when is_pseudo_element s -> Selector.Universal None
  | s -> s

let check_ns = function
  | None | Some Selector.Any -> ()
  | Some Selector.None | Some (Selector.Prefix _) ->
      raise (Skip "namespaced selector")

(* createElement rejects anything else, and a selector such as [\\2d foo] does
   reach the synthesiser as the element name [-foo]. *)
let valid_tag name =
  name <> ""
  && (match name.[0] with 'a' .. 'z' -> true | _ -> false)
  && String.for_all
       (function 'a' .. 'z' | '0' .. '9' | '-' -> true | _ -> false)
       name

let set_tag sp name =
  let name = String.lowercase_ascii name in
  if not (valid_tag name) then raise (Skip "element name is not an HTML tag");
  match sp.tag with
  | None -> sp.tag <- Some name
  | Some t when t = name -> ()
  | Some _ -> raise (Skip "two type selectors in one compound")

(* A tag the pseudo-class needs (a form control, a <details>) unless a type
   selector already pinned another one. *)
let want_tag sp name =
  match sp.tag with None -> sp.tag <- Some name | Some _ -> ()

let set_attr sp name value =
  match name with
  | "class" ->
      List.iter
        (fun c ->
          if c <> "" && not (List.mem c sp.classes) then
            sp.classes <- sp.classes @ [ c ])
        (String.split_on_char ' ' value)
  | "id" -> sp.id <- Some value
  | _ ->
      if not (List.mem_assoc name sp.attrs) then
        sp.attrs <- sp.attrs @ [ (name, value) ]

let note_pseudo sp name =
  if not (List.mem name sp.pseudos) then sp.pseudos <- sp.pseudos @ [ name ]

(* A value that satisfies the match, built around the operand so the browser
   agrees rather than merely the synthesiser. *)
let attribute_value = function
  | Selector.Presence -> ""
  | Selector.Exact v | Selector.Exact_quoted (v, _) -> v
  | Selector.Whitespace_list v | Selector.Whitespace_list_quoted (v, _) ->
      "zz " ^ v ^ " yy"
  | Selector.Hyphen_list v | Selector.Hyphen_list_quoted (v, _) -> v ^ "-zz"
  | Selector.Prefix v | Selector.Prefix_quoted (v, _) -> v ^ "zz"
  | Selector.Suffix v | Selector.Suffix_quoted (v, _) -> "zz" ^ v
  | Selector.Substring v | Selector.Substring_quoted (v, _) -> "zz" ^ v ^ "yy"

(* The empty operand of [~=], [|=], [^=], [$=] and [*=] matches nothing, per
   Selectors 4 sec. 6.2. *)
let attribute_matchable = function
  | Selector.Whitespace_list ""
  | Selector.Whitespace_list_quoted ("", _)
  | Selector.Hyphen_list ""
  | Selector.Hyphen_list_quoted ("", _)
  | Selector.Prefix ""
  | Selector.Prefix_quoted ("", _)
  | Selector.Suffix ""
  | Selector.Suffix_quoted ("", _)
  | Selector.Substring ""
  | Selector.Substring_quoted ("", _) ->
      false
  | _ -> true

(* The smallest 1-based index An+B selects, or none when the sequence never
   reaches a real position. *)
let nth_index = function
  | Selector.Odd -> Some 1
  | Selector.Even -> Some 2
  | Selector.Index b -> if b >= 1 then Some b else None
  | Selector.An_plus_b (a, b) ->
      if a = 0 then if b >= 1 then Some b else None
      else
        let rec first k =
          if k > 32 then None
          else
            let v = (a * k) + b in
            if v >= 1 then Some v else first (k + 1)
        in
        first 0

let set_pos sp p =
  match sp.pos with
  | Anywhere -> sp.pos <- p
  | existing when existing = p -> ()
  | _ -> raise (Skip "conflicting position pseudo-classes")

let nth_pos sp of_type wrap n =
  match nth_index n with
  | None -> raise (Skip "An+B selects no position")
  | Some i when i > 12 -> raise (Skip "An+B position out of range")
  | Some i ->
      sp.of_type <- of_type;
      set_pos sp (wrap i)

let filler tag : node =
  { tag; id = None; classes = []; attrs = []; children = [] }

let node_of_spec sp children : node =
  let children = sp.inner @ children in
  if sp.childless && children <> [] then
    raise (Skip ":empty with required content");
  let tag =
    match sp.tag with
    | Some t -> t
    | None -> if List.mem "div" sp.avoid then "section" else "div"
  in
  if List.mem tag sp.avoid then raise (Skip "negated type selector");
  { tag; id = sp.id; classes = sp.classes; attrs = sp.attrs; children }

let rec add sp part =
  match part with
  | Selector.Element (ns, name) ->
      check_ns ns;
      set_tag sp name
  | Selector.Class c ->
      if not (List.mem c sp.classes) then sp.classes <- sp.classes @ [ c ]
  | Selector.Id i -> sp.id <- Some i
  | Selector.Universal ns -> check_ns ns
  | Selector.Attribute (ns, name, m, _flag) ->
      check_ns ns;
      if not (attribute_matchable m) then
        raise (Skip "attribute matches nothing");
      set_attr sp (Pp.to_string Selector.pp_attr_name name) (attribute_value m)
  | Selector.Compound parts -> List.iter (add sp) parts
  (* :is() and :where() hold as soon as one branch does; the CSS Modules and
     legacy vendor spellings behave the same way. *)
  | Selector.Is l
  | Selector.Where l
  | Selector.Moz_any_call l
  | Selector.Webkit_any_call l
  | Selector.Local_call l
  | Selector.Global_call l ->
      add_branch sp l
  | Selector.Not l -> List.iter (negate sp) l
  | Selector.Has l -> add_has sp l
  (* Structural pseudo-classes: satisfied by where the element is placed. *)
  | Selector.Root | Selector.Scope -> sp.root <- true
  | Selector.Empty -> sp.childless <- true
  | Selector.First_child -> set_pos sp First
  | Selector.Last_child -> set_pos sp Last
  | Selector.Only_child -> set_pos sp Only
  | Selector.First_of_type ->
      sp.of_type <- true;
      set_pos sp First
  | Selector.Last_of_type ->
      sp.of_type <- true;
      set_pos sp Last
  | Selector.Only_of_type ->
      sp.of_type <- true;
      set_pos sp Only
  | Selector.Nth_child (n, None) -> nth_pos sp false (fun i -> Nth i) n
  | Selector.Nth_last_child (n, None) ->
      nth_pos sp false (fun i -> Nth_last i) n
  | Selector.Nth_of_type (n, None) -> nth_pos sp true (fun i -> Nth i) n
  | Selector.Nth_last_of_type (n, None) ->
      nth_pos sp true (fun i -> Nth_last i) n
  | Selector.Nth_child (_, Some _)
  | Selector.Nth_last_child (_, Some _)
  | Selector.Nth_of_type (_, Some _)
  | Selector.Nth_last_of_type (_, Some _) ->
      raise (Skip "An+B of S")
  (* Pseudo-classes an attribute or a tag choice settles. *)
  | Selector.Link | Selector.Any_link ->
      want_tag sp "a";
      set_attr sp "href" "#rd"
  | Selector.Enabled | Selector.Optional | Selector.Read_write | Selector.Valid
    ->
      want_tag sp "input"
  | Selector.Disabled ->
      want_tag sp "input";
      set_attr sp "disabled" ""
  | Selector.Checked | Selector.Default ->
      want_tag sp "input";
      set_attr sp "type" "checkbox";
      set_attr sp "checked" ""
  | Selector.Required | Selector.Invalid ->
      want_tag sp "input";
      set_attr sp "required" ""
  | Selector.Read_only ->
      want_tag sp "input";
      set_attr sp "readonly" ""
  | Selector.Placeholder_shown ->
      want_tag sp "input";
      set_attr sp "placeholder" "rd"
  | Selector.In_range | Selector.Out_of_range ->
      want_tag sp "input";
      set_attr sp "type" "number";
      set_attr sp "min" "0";
      set_attr sp "max" (match part with Selector.In_range -> "9" | _ -> "1");
      set_attr sp "value" "5"
  | Selector.Inert -> set_attr sp "inert" ""
  | Selector.Open ->
      want_tag sp "details";
      set_attr sp "open" ""
  | Selector.Dir d -> set_attr sp "dir" d
  | Selector.Lang (code :: _) -> set_attr sp "lang" code
  | Selector.Lang [] -> raise (Skip "empty :lang()")
  | Selector.Heading -> want_tag sp "h1"
  (* Every element the driver builds is a known HTML element. *)
  | Selector.Defined | Selector.Local_scope | Selector.Global_scope -> ()
  (* Pseudo-elements the driver samples. Three of them also pin the element the
     pseudo hangs off. *)
  | Selector.Placeholder ->
      want_tag sp "input";
      set_attr sp "placeholder" "rd";
      note_pseudo sp "::placeholder"
  | Selector.File_selector_button ->
      want_tag sp "input";
      set_attr sp "type" "file";
      note_pseudo sp "::file-selector-button"
  | Selector.Marker ->
      want_tag sp "li";
      note_pseudo sp "::marker"
  | Selector.Before _ -> note_pseudo sp "::before"
  | Selector.After _ -> note_pseudo sp "::after"
  | Selector.First_line _ -> note_pseudo sp "::first-line"
  | Selector.First_letter _ -> note_pseudo sp "::first-letter"
  | Selector.Backdrop -> note_pseudo sp "::backdrop"
  | Selector.Selection -> note_pseudo sp "::selection"
  (* Pseudo-elements getComputedStyle cannot sample, or that need a shadow tree,
     a highlight registry or a running view transition. *)
  | Selector.Target_text | Selector.Spelling_error | Selector.Grammar_error
  | Selector.Highlight _ | Selector.Part _ | Selector.Slotted _ | Selector.Cue _
  | Selector.Cue_region _ | Selector.View_transition
  | Selector.View_transition_group _ | Selector.View_transition_image_pair _
  | Selector.View_transition_old _ | Selector.View_transition_new _
  | Selector.Unknown_pseudo_element _ | Selector.Unknown_pseudo_element_call _
  | Selector.Moz_placeholder | Selector.Webkit_input_placeholder
  | Selector.Ms_input_placeholder | Selector.Webkit_scrollbar
  | Selector.Webkit_search_cancel_button | Selector.Webkit_search_decoration
  | Selector.Webkit_details_marker | Selector.Details_content
  | Selector.Webkit_datetime_edit_fields_wrapper
  | Selector.Webkit_date_and_time_value | Selector.Webkit_datetime_edit
  | Selector.Webkit_datetime_edit_year_field
  | Selector.Webkit_datetime_edit_month_field
  | Selector.Webkit_datetime_edit_day_field
  | Selector.Webkit_datetime_edit_hour_field
  | Selector.Webkit_datetime_edit_minute_field
  | Selector.Webkit_datetime_edit_second_field
  | Selector.Webkit_datetime_edit_millisecond_field
  | Selector.Webkit_datetime_edit_meridiem_field
  | Selector.Webkit_inner_spin_button | Selector.Webkit_outer_spin_button
  | Selector.Webkit_calendar_picker_indicator ->
      raise (Skip "pseudo-element not sampled")
  (* Everything below needs something a static document cannot provide. *)
  | Selector.Hover | Selector.Active | Selector.Focus | Selector.Focus_visible
  | Selector.Focus_within | Selector.Target_within | Selector.User_valid
  | Selector.User_invalid | Selector.Autofill | Selector.Webkit_autofill
  | Selector.Moz_focusring | Selector.Moz_ui_invalid | Selector.Moz_ui_valid ->
      raise (Skip "user-interaction pseudo-class")
  | Selector.Target | Selector.Visited | Selector.Local_link ->
      raise (Skip "navigation-dependent pseudo-class")
  | Selector.Fullscreen | Selector.Modal | Selector.Picture_in_picture
  | Selector.Popover_open | Selector.Playing | Selector.Paused
  | Selector.Seeking | Selector.Buffering | Selector.Stalled | Selector.Muted
  | Selector.Volume_locked ->
      raise (Skip "live-state pseudo-class")
  | Selector.Indeterminate | Selector.Blank ->
      raise (Skip "script-only pseudo-class")
  | Selector.Left | Selector.Right | Selector.First ->
      raise (Skip "page pseudo-class")
  | Selector.Future | Selector.Past | Selector.Current | Selector.Current_of _
    ->
      raise (Skip "timing pseudo-class")
  | Selector.Host _ | Selector.Host_context _ | Selector.State _ ->
      raise (Skip "shadow-tree pseudo-class")
  | Selector.Active_view_transition | Selector.Active_view_transition_type _ ->
      raise (Skip "view-transition pseudo-class")
  | Selector.Nth_col _ | Selector.Nth_last_col _ ->
      raise (Skip "column pseudo-class")
  | Selector.Unknown_pseudo_class _ | Selector.Unknown_pseudo_class_call _
  | Selector.Webkit_any ->
      raise (Skip "unknown pseudo-class")
  | Selector.Nesting -> raise (Skip "nesting selector")
  | Selector.Combined _ | Selector.Relative _ | Selector.List _ ->
      raise (Skip "complex selector inside a compound")

(* The synthesised element carries only what the compound asked for, so a
   negation holds by construction - except when it rules out the default tag,
   pins the element away from a position, or negates a state the element has by
   default. *)
and negate sp = function
  | Selector.Element (_, name) ->
      sp.avoid <- String.lowercase_ascii name :: sp.avoid
  | Selector.First_child -> if sp.pos = Anywhere then sp.pos <- Nth 2
  | Selector.Last_child -> if sp.pos = Anywhere then sp.pos <- Nth_last 2
  | Selector.Enabled ->
      want_tag sp "input";
      set_attr sp "disabled" ""
  | Selector.Dir d -> set_attr sp "dir" (if d = "rtl" then "ltr" else "rtl")
  | Selector.Not l -> List.iter (add sp) l
  | Selector.Compound parts -> List.iter (negate sp) parts
  | Selector.List l | Selector.Is l | Selector.Where l ->
      List.iter (negate sp) l
  | _ -> ()

(* :is(a, b) holds when any branch does, so take the first branch that is a
   plain compound; a complex branch would need its own ancestor chain. *)
and add_branch sp branches =
  let rec first = function
    | [] -> raise (Skip "no satisfiable :is() branch")
    | b :: rest -> (
        let saved = { sp with tag = sp.tag } in
        try add sp b
        with Skip _ ->
          restore sp saved;
          first rest)
  in
  first branches

and add_has sp branches =
  let inner, relation =
    match branches with
    | Selector.Relative (c, s) :: _ -> (s, c)
    | s :: _ -> (s, Selector.Descendant)
    | [] -> raise (Skip "empty :has()")
  in
  match relation with
  | Selector.Descendant | Selector.Child ->
      let child = new_spec () in
      add child inner;
      if child.root then raise (Skip ":root inside :has()");
      sp.pseudos <- sp.pseudos @ child.pseudos;
      sp.inner <- sp.inner @ [ node_of_spec child [] ]
  | _ -> raise (Skip "sibling :has()")

(* Split a complex selector into its steps, each carrying the combinator that
   joins it to the step on its left. *)
let rec steps sel =
  match sel with
  | Selector.Combined (l, c, r) -> (
      match steps r with
      | (_, first) :: rest -> steps l @ ((Some c, first) :: rest)
      | [] -> steps l)
  | Selector.Relative _ -> raise (Skip "relative selector outside :has()")
  | s -> [ (None, s) ]

let pad n tag = List.init (max 0 n) (fun _ -> filler tag)

(* Lay the child out among its siblings so its position pseudo-class holds. *)
let siblings ~parent_inner ~child_spec ~(child : node) before =
  let tag = if child_spec.of_type then child.tag else "span" in
  let list =
    match child_spec.pos with
    | Anywhere | Last -> before @ [ child ]
    | First | Only ->
        if before <> [] then raise (Skip "position conflicts with a sibling")
        else [ child ]
    | Nth n ->
        let need = n - 1 - List.length before in
        if need < 0 then raise (Skip "position conflicts with a sibling")
        else pad need tag @ before @ [ child ]
    | Nth_last n -> before @ [ child ] @ pad (n - 1) tag
  in
  match (parent_inner, child_spec.pos) with
  | [], _ -> list
  | _, (Last | Only | Nth_last _) ->
      raise (Skip ":has() conflicts with a trailing position")
  | inner, _ -> list @ inner

let build_fragment sel =
  let specs =
    List.map
      (fun (comb, part) ->
        let sp = new_spec () in
        add sp part;
        (comb, sp))
      (steps sel)
  in
  (match specs with
  | [] -> raise (Skip "empty selector")
  | _ :: rest ->
      if List.exists (fun (_, sp) -> sp.root) rest then
        raise (Skip ":root is not the outermost step"));
  let pseudos = List.concat_map (fun (_, sp) -> sp.pseudos) specs in
  match List.rev specs with
  | [] -> raise (Skip "empty selector")
  | (first_link, target) :: earlier ->
      let cur = ref (node_of_spec target []) in
      let cur_spec = ref target in
      let before = ref [] in
      let link = ref first_link in
      List.iter
        (fun (comb, sp) ->
          (match !link with
          | None | Some Selector.Descendant | Some Selector.Child ->
              let kids =
                siblings ~parent_inner:sp.inner ~child_spec:!cur_spec
                  ~child:!cur !before
              in
              cur := node_of_spec { sp with inner = [] } kids;
              cur_spec := sp;
              before := []
          | Some Selector.Next_sibling | Some Selector.Subsequent_sibling -> (
              let node = node_of_spec sp [] in
              let tag = if sp.of_type then node.tag else "span" in
              (* The siblings are prepended right to left, so the padding an
                 An+B position needs goes in front of the node itself. *)
              before :=
                match sp.pos with
                | Anywhere | First -> node :: !before
                | Nth n -> pad (n - 1) tag @ (node :: !before)
                | Last | Only | Nth_last _ ->
                    raise (Skip "position conflicts with a sibling"))
          | Some _ -> raise (Skip "legacy combinator"));
          link := comb)
        earlier;
      let nodes =
        siblings ~parent_inner:[] ~child_spec:!cur_spec ~child:!cur !before
      in
      (!cur_spec, nodes, pseudos)

let rec count_nodes n =
  List.fold_left (fun n c -> count_nodes (n + 1) c.children) n

let all_selectors sheet =
  let flat = Css.flatten_nesting sheet in
  List.rev
    (Css.fold
       (fun acc stmt ->
         match Css.as_rule stmt with
         | Some (sel, _, _) -> List.rev_append (Edge.selectors sel) acc
         | None -> acc)
       [] flat)

let bump tbl key =
  match List.assoc_opt key !tbl with
  | Some n -> tbl := (key, n + 1) :: List.remove_assoc key !tbl
  | None -> tbl := !tbl @ [ (key, 1) ]

let of_stylesheet ?(max_elements = 4000) sheet =
  let selectors = all_selectors sheet in
  let cases = ref [] in
  let probes = ref [] in
  let pseudos = ref [] in
  let html_classes = ref [] in
  let html_attrs = ref [] in
  let skipped = ref [] in
  let examples = ref [] in
  let synthesised = ref 0 in
  let count = ref 0 in
  let skip sel reason =
    bump skipped reason;
    if not (List.mem_assoc reason !examples) then
      examples := (reason, Selector.to_string sel) :: !examples
  in
  List.iter
    (fun sel ->
      if !count >= max_elements then skip sel "document element cap"
      else if Selector.matches_nothing sel then
        skip sel "selector matches nothing"
      else
        match build_fragment sel with
        | exception Skip reason -> skip sel reason
        | exception Invalid_argument _ -> skip sel "invalid selector component"
        | outer, nodes, sel_pseudos ->
            incr synthesised;
            count := count_nodes !count nodes;
            probes := Selector.to_string (strip_pseudo_elements sel) :: !probes;
            List.iter
              (fun p ->
                if not (List.mem p !pseudos) then pseudos := !pseudos @ [ p ])
              sel_pseudos;
            if outer.root then (
              List.iter
                (fun c ->
                  if not (List.mem c !html_classes) then
                    html_classes := !html_classes @ [ c ])
                outer.classes;
              List.iter
                (fun (k, v) ->
                  if not (List.mem_assoc k !html_attrs) then
                    html_attrs := !html_attrs @ [ (k, v) ])
                outer.attrs;
              (* The outermost step is <html> itself, so its subtree hangs off
                 <body> rather than off a case wrapper. *)
              cases := !cases @ List.concat_map (fun n -> n.children) nodes)
            else
              cases :=
                !cases
                @ [
                    {
                      tag = "div";
                      id = None;
                      classes = [ "rd-case" ];
                      attrs = [];
                      children = nodes;
                    };
                  ])
    selectors;
  let root_node : node =
    { tag = "body"; id = None; classes = []; attrs = []; children = !cases }
  in
  {
    root_node;
    html_classes = !html_classes;
    html_attrs = !html_attrs;
    doc_pseudos = !pseudos;
    probes = List.rev !probes;
    n_selectors = List.length selectors;
    n_synthesised = !synthesised;
    n_elements = count_nodes 0 [ root_node ];
    skips = List.sort (fun (_, a) (_, b) -> compare b a) !skipped;
    examples = !examples;
  }

let rec json_of_node (n : node) =
  Json.Obj
    ([ ("t", Json.Str n.tag) ]
    @ (match n.id with Some i -> [ ("i", Json.Str i) ] | None -> [])
    @ (if n.classes = [] then []
       else [ ("c", Json.Arr (List.map (fun c -> Json.Str c) n.classes)) ])
    @ (if n.attrs = [] then []
       else
         [
           ( "a",
             Json.Arr
               (List.map
                  (fun (k, v) -> Json.Arr [ Json.Str k; Json.Str v ])
                  n.attrs) );
         ])
    @
    if n.children = [] then []
    else [ ("k", Json.Arr (List.map json_of_node n.children)) ])

let to_json t =
  Json.Obj
    [
      ("dom", json_of_node t.root_node);
      ("htmlClasses", Json.Arr (List.map (fun c -> Json.Str c) t.html_classes));
      ( "htmlAttrs",
        Json.Arr
          (List.map
             (fun (k, v) -> Json.Arr [ Json.Str k; Json.Str v ])
             t.html_attrs) );
      ("pseudos", Json.Arr (List.map (fun p -> Json.Str p) t.doc_pseudos));
      ("probes", Json.Arr (List.map (fun p -> Json.Str p) t.probes));
    ]

let selectors t = t.n_selectors
let synthesised t = t.n_synthesised
let elements t = t.n_elements
let pseudo_elements t = t.doc_pseudos
let skipped t = t.skips
let skipped_example t reason = List.assoc_opt reason t.examples
