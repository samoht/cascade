(* Project a stylesheet onto an element tree, producing the inline-style
   declarations for each element plus the rules that have no inline form
   (:hover, @media, @keyframes, ...). The cascade itself is {!Resolve}; this
   adds the inline-specific policy. It is pure: it returns the declarations to
   apply, it does not mutate the tree (the caller writes them onto each
   node). *)

module SSet = Set.Make (String)

(* Selectors that can be written as an inline style: no state, no condition -
   and nothing the matcher cannot decide. Inlining a rule takes it out of the
   stylesheet, so a selector {!Resolve.supported} does not cover would leave its
   declarations nowhere at all: matched by nobody here, and gone from the sheet
   the browser reads. Deriving the set from the matcher is what stops the two
   from parting ways, and the default reading is {!Resolve.constructor-Browser},
   since what is left has to render the way the page it came from does. *)
let inlinable = Resolve.supported

(* Elements that never carry inline styles ([html] is stylable so :root custom
   properties land on it). *)
let no_style = [ "head"; "meta"; "title"; "base"; "link"; "style"; "script" ]

let rec declaration_is_inherited = function
  | Declaration.Declaration { property; _ } ->
      Properties.property_is_inherited property
  | Declaration.Theme_guarded { decl; _ } -> declaration_is_inherited decl

module SMap = Map.Make (String)

(* The font and text block a form control takes from the UA. The [font]
   shorthand sits with its longhands because it is an inherited property in its
   own right, so an author [font:] on a control has to survive too. *)
let ua_control =
  [
    "color";
    "cursor";
    "font";
    "font-family";
    "font-feature-settings";
    "font-size";
    "font-stretch";
    "font-style";
    "font-variant";
    "font-weight";
    "letter-spacing";
    "line-height";
    "text-align";
    "text-indent";
    "text-shadow";
    "text-transform";
    "word-spacing";
  ]

(* The inherited properties the user-agent stylesheet declares for an element,
   read off a browser in standards mode. css-cascade-5 sec. 6.1 sorts by origin
   before anything else and sec. 6.2 puts the UA origin under the author one, so
   a property the UA declares for the element is cascaded there and never
   inherited: the author declaration covering it has to stay. Over-listing an
   entry only keeps a redundant declaration; missing one changes the render,
   which is what the differential test guards. *)
let ua_groups =
  [
    ([ "a" ], [ "color"; "cursor" ]);
    ([ "address"; "cite"; "dfn"; "em"; "i"; "var" ], [ "font"; "font-style" ]);
    ([ "b"; "strong" ], [ "font"; "font-weight" ]);
    ([ "bdi"; "bdo" ], [ "direction" ]);
    ([ "big"; "small"; "sub"; "sup" ], [ "font"; "font-size" ]);
    ([ "button"; "input" ], ua_control);
    ([ "caption"; "center" ], [ "text-align" ]);
    ([ "code"; "kbd"; "samp"; "tt" ], [ "font"; "font-family" ]);
    ([ "dialog"; "hr"; "mark" ], [ "color" ]);
    ([ "dir"; "menu"; "ol"; "ul" ], [ "list-style"; "list-style-type" ]);
    ( [ "h1"; "h2"; "h3"; "h4"; "h5"; "h6" ],
      [ "font"; "font-size"; "font-weight" ] );
    ([ "label" ], [ "cursor" ]);
    ( [ "listing"; "plaintext"; "pre"; "xmp" ],
      [ "font"; "font-family"; "white-space" ] );
    ([ "marquee" ], [ "text-align"; "white-space" ]);
    ([ "nobr" ], [ "white-space" ]);
    ([ "optgroup" ], [ "font"; "font-weight" ]);
    ([ "option" ], [ "font"; "font-weight"; "white-space" ]);
    ( [ "rt" ],
      [ "font"; "font-size"; "line-height"; "text-align"; "text-indent" ] );
    ([ "ruby" ], [ "text-indent" ]);
    ([ "select" ], "white-space" :: ua_control);
    ([ "summary" ], [ "list-style"; "list-style-image"; "list-style-type" ]);
    ([ "table" ], [ "border-collapse"; "border-spacing"; "text-indent" ]);
    ([ "textarea" ], "overflow-wrap" :: "white-space" :: ua_control);
    ([ "th" ], [ "font"; "font-weight"; "text-align" ]);
  ]

let ua_declared =
  List.fold_left
    (fun m (names, props) ->
      List.fold_left
        (fun m name -> SMap.add name (SSet.of_list props) m)
        m names)
    SMap.empty ua_groups

(* CSS Syntax 3 (ED) sec. 5.5: a style attribute is a declaration list, not a
   rule body, so a declaration's value runs to the next top-level [;] or the end
   of input, where only [{], [(] and [[] open a block. A [}] is then a preserved
   token inside the value rather than a terminator, and splicing the attribute
   into [a{...}] lets it close the rule instead, reviving a declaration no
   browser applies. Recovery drops the invalid declaration and keeps the rest,
   as the cascade does. *)
let parse_inline s =
  let cursor =
    Cursor.of_string s |> Cursor.remaining
    |> Cursor.of_components ~source:s ~recover:true
  in
  match Declaration.read_declarations cursor with
  | decls -> decls
  | exception Error.Parse_error _ -> []

let decl_value d =
  let s = Css.inline_style_of_declarations ~minify:true ~mode:Variables [ d ] in
  match String.index_opt s ':' with
  | Some i -> String.sub s (i + 1) (String.length s - i - 1)
  | None -> s

(* Colour functions (css-color-4 sec. 4 to 12, css-color-5 sec. 3): their
   arguments are channel coordinates, so a percentage or an angle inside one is
   not a fraction of anything the element decides. [light-dark()] is absent on
   purpose - it reads the inherited [color-scheme]. *)
let color_functions =
  [
    "rgb";
    "rgba";
    "hsl";
    "hsla";
    "hwb";
    "lab";
    "lch";
    "oklab";
    "oklch";
    "color";
    "color-mix";
  ]

(* css-values-4 sec. 5.2: the absolute length units. Every other unit resolves
   against something the declaration does not carry - the element's own font
   (sec. 6.1), the root's, the viewport (sec. 6.2), the query container (sec.
   6.4) - so a unit this list does not name counts as relative, and so does one
   CSS grows later. *)
let absolute_units = [ "px"; "cm"; "mm"; "q"; "in"; "pt"; "pc" ]

(* Keywords that read a value off another element: css-fonts-4 sec. 3.5 scales
   [larger]/[smaller] off the parent font size and sec. 3.2 steps
   [bolder]/[lighter] off the parent weight. css-color-4 sec. 15 resolves
   [currentcolor] against the element's own [color], which [color-mix()] then
   bakes into the computed value. *)
let relative_keywords =
  [ "larger"; "smaller"; "bolder"; "lighter"; "currentcolor" ]

(* Whether the value [v] computes to the same thing wherever it lands. The
   restatement check below compares an ancestor's value with a descendant's as
   written, and the two elements differ in font, in query container and in the
   custom properties in scope, so [2em], [5cqw] and [var(--x)] each name two
   values. The scan whitelists: a shape it does not recognise is relative. *)
let element_independent v =
  let lexer = Lexer.of_string v in
  (* [stack] holds, per open paren, whether it is a colour function's. *)
  let rec scan stack =
    let in_color = match stack with c :: _ -> c | [] -> false in
    match (Lexer.next lexer).Token.kind with
    | Token.Eof -> true
    | Token.Function f ->
        List.mem (String.lowercase_ascii f) color_functions
        && scan (true :: stack)
    | Token.Open Token.Paren -> scan (in_color :: stack)
    | Token.Close Token.Paren ->
        scan (match stack with _ :: rest -> rest | [] -> [])
    | Token.Percentage _ -> in_color && scan stack
    | Token.Dimension { unit_; _ } ->
        (in_color || List.mem (String.lowercase_ascii unit_) absolute_units)
        && scan stack
    | Token.Ident i ->
        (not (List.mem (String.lowercase_ascii i) relative_keywords))
        && scan stack
    | _ -> scan stack
  in
  scan []

(* The cascade slots a declaration writes, as {!Shorthand} models them for the
   optimizer's conflict graph. A shorthand names every longhand it resets there
   - [font] names [font-weight] and [line-height] (css-fonts-4 sec. 2.7),
   [list-style] names [list-style-image] (css-lists-3 sec. 3.4), [white-space]
   names [text-wrap-mode] (css-text-4 sec. 3) - which is exactly what the
   restatement check needs, so the reset sets come from that model rather than
   from a second table beside it. [all] resets every property (css-cascade-5
   sec. 3.2), and a property cascade does not type carries no reset set at all -
   [font-variant] is one - so both count as writing every slot. *)
type footprint = Every_slot | Slots of Shorthand.overlap_key list

let rec footprint = function
  | Declaration.Theme_guarded { decl; _ } -> footprint decl
  | Declaration.Declaration { property = All; _ } -> Every_slot
  | Declaration.Declaration { property = Unknown_property _; _ } -> Every_slot
  | d -> Slots (Shorthand.declaration_overlap_keys d)

(* Whether [v] only restates the value the ancestors have in force for [p]. *)
let restates p v ctx =
  match List.assoc_opt p ctx with
  | Some (w, _) -> String.equal w v
  | None -> false

(* Drop every context entry [keys] can write: a value the ancestors put in force
   stops being in force as soon as something on the way down resets one of its
   slots. *)
let forget keys ctx =
  List.filter
    (fun (_, (_, k)) -> not (Shorthand.overlap_keys_intersect keys k))
    ctx

let add_props acc ds =
  List.fold_left
    (fun acc d ->
      SSet.add (String.lowercase_ascii (Declaration.property_name d)) acc)
    acc ds

(* Every declaration a kept (conditional / stateful) rule can set. The descent
   goes through {!Stylesheet.statement_children}, so every block at-rule is
   covered: a declaration missed here is one an inline style could override, and
   the kept rule would lose a fight it wins in the browser.

   The declarations come off [Rule] and [Declarations] alone, not through
   {!Stylesheet.statement_declarations}. This set decides which declarations may
   move into a [style] attribute, which shifts cascade position only against
   author-origin rules of the same importance (css-cascade-5 sec. 6.1), and the
   other declaration-carrying at-rules contribute none: [@keyframes] declares in
   the animation origin, [@position-try] in the position fallback origin,
   [@page] applies to a page box rather than an element, and
   [@supports-condition] is never applied to a box. Widening it forces
   [transform], [opacity] and [color] back into [<style>] on any page that has a
   spinner keyframe. *)
let rec decls_of_stmts acc stmts =
  List.fold_left
    (fun acc s ->
      let acc =
        match s with
        | Stylesheet.Rule r -> List.rev_append (Stylesheet.declarations r) acc
        | Stylesheet.Declarations ds -> List.rev_append ds acc
        | _ -> acc
      in
      decls_of_stmts acc (Stylesheet.statement_children s))
    acc stmts

(* A [@layer] block applies unconditionally: it only orders competing
   declarations, it does not gate them behind a condition the way
   @media/@supports/@container do. So the declarations its rules can override
   are those of its own un-inlinable rules, exactly as at the top level, while a
   conditional block contributes all of them. *)
let rec dynamic_decls acc stmts =
  List.fold_left
    (fun acc s ->
      match s with
      | Stylesheet.Rule r when inlinable (Stylesheet.selector r) -> acc
      | Stylesheet.Layer (_, b) -> dynamic_decls acc b
      | s -> decls_of_stmts acc [ s ])
    acc stmts

(* The kept declarations to test an inlinable one against, one per property
   name: {!Shorthand.declarations_overlap} reads the property, never the value,
   so a second declaration of the same property answers exactly as the first.
   Each keeps its precomputed footprint, since the test runs once per property
   for every declaration in the sheet. *)
let overlap_probes ds =
  let seen = Hashtbl.create 64 in
  List.filter_map
    (fun d ->
      let name = String.lowercase_ascii (Declaration.property_name d) in
      if Hashtbl.mem seen name then None
      else begin
        Hashtbl.add seen name ();
        Some (d, Shorthand.declaration_overlap_keys d)
      end)
    ds

(* Whether a kept rule can overwrite what [d] sets. Matching property names is
   not enough: [margin] and [margin-top] write a common cascade slot under
   different names, so a kept [.mt-6{margin-top}] and an inlinable [p{margin:0}]
   compete. Inlining the shorthand there puts it in a style attribute, which
   outranks every selector, and the kept rule loses a fight it wins in the
   browser. *)
let overlaps_kept probes d =
  let keys = Shorthand.declaration_overlap_keys d in
  List.exists
    (fun (kept, kept_keys) ->
      Shorthand.declarations_overlap_with_keys d keys kept kept_keys)
    probes

(* Split each statement into the part with no inline form and the part that
   projects onto elements. A [@layer] block splits like the top level, but the
   wrapper survives on both sides: the layer decides which of two competing
   declarations wins, so dropping it would change the result. A layer left with
   nothing on one side disappears from that side - it has no rule there to
   order, and removing it does not disturb the relative order of the others. *)
let rec split ~is_dyn stmts =
  let keep, inline =
    List.fold_left
      (fun (keep, inline) stmt ->
        match stmt with
        | Stylesheet.Layer (name, body) ->
            let k, i = split ~is_dyn body in
            ( (if k = [] then keep else Stylesheet.Layer (name, k) :: keep),
              if i = [] then inline else Stylesheet.Layer (name, i) :: inline )
        | Stylesheet.Layer_decl _ ->
            (* The statement orders layers on both sides. *)
            (stmt :: keep, stmt :: inline)
        | Stylesheet.Rule r when inlinable (Stylesheet.selector r) ->
            let sel = Stylesheet.selector r
            and ds = Stylesheet.declarations r in
            let of_part ds =
              Stylesheet.Rule (Stylesheet.rule ~selector:sel ds)
            in
            let keep =
              match List.filter is_dyn ds with
              | [] -> keep
              | res -> of_part res :: keep
            in
            let inline =
              match List.filter (fun d -> not (is_dyn d)) ds with
              | [] -> inline
              | inl -> of_part inl :: inline
            in
            (keep, inline)
        | other -> (other :: keep, inline))
      ([], []) stmts
  in
  (List.rev keep, List.rev inline)

(* Kept rules, for reporting. A block at-rule is not itself a rule: what it
   keeps out of the inline projection is the rules it holds, so a [@media] with
   three rules kept three. One that holds no statements of its own - a
   [@font-face], a [@keyframes], a [@layer] statement - is itself the one thing
   kept, so it counts once. *)
let rec count_kept stmts =
  List.fold_left
    (fun n s ->
      let inner = count_kept (Stylesheet.statement_children s) in
      match s with
      | Stylesheet.Rule _ -> n + 1 + inner
      | _ when inner = 0 -> n + 1
      | _ -> n + inner)
    0 stmts

type 'node assignment = 'node * Declaration.declaration list
(** The inline-style declarations to write onto a node. *)

type 'node result = {
  styles : 'node assignment list;
      (** Each element with the declarations to set on its [style] attribute. *)
  keep_css : string;
      (** The rules with no inline form, serialised as a [<style>] body. *)
  kept : int;  (** How many rules [keep_css] holds, for reporting. *)
}

module Make (Node : Resolve.NODE) = struct
  module R = Resolve.Make (Node)

  (* A node's style: the author cascade from {!R.resolve} with the element's own
     inline style overlaid (inline beats a selector, but an author !important
     beats a normal inline declaration). *)
  let resolved sheet n =
    let author = R.resolve sheet n in
    match Option.map parse_inline (Node.attribute n "style") with
    | None | Some [] -> author
    | Some inline ->
        let overlay map d =
          let k = Declaration.property_name d in
          match List.assoc_opt k map with
          | Some cur
            when Declaration.is_important cur
                 && not (Declaration.is_important d) ->
              map
          | _ -> (k, d) :: List.remove_assoc k map
        in
        (* The overlay accumulates in reverse so the closing [rev_map] hands
           back the author order: css-cascade-5 sec. 6.1 breaks a tie by order
           of appearance, so a longhand written after its shorthand still wins.
           An overlaid declaration conses onto the front and so lands last,
           where it beats the selector it met. *)
        List.fold_left overlay
          (List.rev_map (fun d -> (Declaration.property_name d, d)) author)
          inline
        |> List.rev_map snd

  (* The inherited properties the UA declares for [node]: the ones its element
     name carries, plus the [direction] a [dir] attribute maps onto it. *)
  let ua_props node =
    let named =
      match Node.name node with
      | None -> SSet.empty
      | Some n -> (
          match SMap.find_opt (String.lowercase_ascii n) ua_declared with
          | Some props -> props
          | None -> SSet.empty)
    in
    match Node.attribute node "dir" with
    | Some _ -> SSet.add "direction" named
    | None -> named

  (* [ctx] maps each inherited property to the value in force from the ancestors
     and the slots that value covers, so [minimal] can drop a declaration that
     only restates it, and can tell when something on the way down has reset one
     of those slots. *)
  let rec walk ~minimal sheet ctx node acc =
    let is_no_style =
      match Node.name node with
      | Some n -> List.mem (String.lowercase_ascii n) no_style
      | None -> false
    in
    if is_no_style then
      List.fold_left
        (fun acc child -> walk ~minimal sheet ctx child acc)
        acc (Node.children node)
    else begin
      let decls = resolved sheet node in
      let ua = ua_props node in
      (* Without [minimal] nothing is dropped, so no context is needed. *)
      let kept, ctx' =
        if not minimal then (List.rev decls, ctx)
        else
          List.fold_left
            (fun (kept, ctx) d ->
              let p = String.lowercase_ascii (Declaration.property_name d) in
              match footprint d with
              | Every_slot -> (d :: kept, [])
              | Slots keys when not (declaration_is_inherited d) ->
                  (d :: kept, forget keys ctx)
              | Slots keys ->
                  let v = decl_value d in
                  (* Inheritance only reaches a property nothing declares, so a
                     value the UA would win back is not a restatement, and equal
                     text is only equal computed values where nothing in it
                     resolves against the element. *)
                  if
                    (not (SSet.mem p ua))
                    && restates p v ctx && element_independent v
                  then (kept, ctx)
                  else (d :: kept, (p, (v, keys)) :: forget keys ctx))
            ([], ctx) decls
      in
      (* A UA declaration the element does not override is what its children
         inherit, so the ancestors' value stops here. *)
      let ctx' =
        if SSet.is_empty ua then ctx'
        else
          let own = add_props SSet.empty decls in
          SSet.fold
            (fun p ctx ->
              if SSet.mem p own then ctx else List.remove_assoc p ctx)
            ua ctx'
      in
      let acc = (node, List.rev kept) :: acc in
      List.fold_left
        (fun acc child -> walk ~minimal sheet ctx' child acc)
        acc (Node.children node)
    end

  let compute ?(minimal = false) ~sheet roots =
    (* Flatten nesting up front, so the split and {!R.resolve} see the same flat
       rules. *)
    let stmts = Flatten.block sheet in
    (* Declarations a kept rule can override must stay in the cascade. *)
    let dyn = overlap_probes (dynamic_decls [] stmts) in
    let is_dyn d = overlaps_kept dyn d in
    let keep, inline = split ~is_dyn stmts in
    let keep_css =
      if keep = [] then "" else Css.to_string ~minify:true (Stylesheet.v keep)
    in
    let inline_sheet = Stylesheet.v inline in
    let styles =
      List.fold_left
        (fun acc root -> walk ~minimal inline_sheet [] root acc)
        [] roots
      |> List.rev
    in
    { styles; keep_css; kept = count_kept keep }
end
