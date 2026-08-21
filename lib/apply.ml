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
   from parting ways. *)
let inlinable = Resolve.supported

(* Elements that never carry inline styles ([html] is stylable so :root custom
   properties land on it). *)
let no_style = [ "head"; "meta"; "title"; "base"; "link"; "style"; "script" ]

(* CSS inherited properties (a conservative set: missing one only keeps a
   redundant declaration; a differential test guards against dropping a needed
   one). *)
let inherited =
  [
    "color";
    "font";
    "font-family";
    "font-size";
    "font-weight";
    "font-style";
    "font-variant";
    "font-stretch";
    "font-feature-settings";
    "line-height";
    "letter-spacing";
    "word-spacing";
    "text-align";
    "text-indent";
    "text-transform";
    "text-shadow";
    "white-space";
    "word-break";
    "overflow-wrap";
    "hyphens";
    "tab-size";
    "visibility";
    "cursor";
    "direction";
    "list-style";
    "list-style-type";
    "list-style-position";
    "list-style-image";
    "quotes";
    "caption-side";
    "border-collapse";
    "border-spacing";
    "empty-cells";
  ]

let is_inherited p = List.mem (String.lowercase_ascii p) inherited

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

(* CSS Syntax 3 sec. 5.4: a style attribute is a declaration list, not a rule
   body, so a declaration's value runs to the next top-level [;] or the end of
   input, where only [{], [(] and [[] open a block. A [}] is then a preserved
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
  let s =
    Stylesheet.inline_style_of_declarations ~minify:true ~mode:Variables [ d ]
  in
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

let add_props acc ds =
  List.fold_left
    (fun acc d ->
      SSet.add (String.lowercase_ascii (Declaration.property_name d)) acc)
    acc ds

(* Every property a kept (conditional / stateful) rule can set. The descent goes
   through {!Stylesheet.statement_children}, so every block at-rule is covered:
   a property missed here is one an inline style could override, and the kept
   rule would lose a fight it wins in the browser. *)
let rec props_of_stmts acc stmts =
  List.fold_left
    (fun acc s ->
      let acc =
        match s with
        | Stylesheet.Rule r -> add_props acc (Stylesheet.declarations r)
        | Stylesheet.Declarations ds -> add_props acc ds
        | _ -> acc
      in
      props_of_stmts acc (Stylesheet.statement_children s))
    acc stmts

(* A [@layer] block applies unconditionally: it only orders competing
   declarations, it does not gate them behind a condition the way
   @media/@supports/@container do. So the properties its rules can override are
   those of its own un-inlinable rules, exactly as at the top level, while a
   conditional block contributes all of them. *)
let rec dynamic_props acc stmts =
  List.fold_left
    (fun acc s ->
      match s with
      | Stylesheet.Rule r when inlinable (Stylesheet.selector r) -> acc
      | Stylesheet.Layer (_, b) -> dynamic_props acc b
      | s -> props_of_stmts acc [ s ])
    acc stmts

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

  (* [ctx] maps each inherited property to the value in force from the
     ancestors, so [minimal] can drop a declaration that only restates it. *)
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
      let kept, ctx' =
        List.fold_left
          (fun (kept, ctx) d ->
            let p = String.lowercase_ascii (Declaration.property_name d) in
            if not (minimal && is_inherited p) then (d :: kept, ctx)
            else
              let v = decl_value d in
              (* Inheritance only reaches a property nothing declares, so a
                 value the UA would win back is not a restatement, and equal
                 text is only equal computed values where nothing in it resolves
                 against the element. *)
              if
                (not (SSet.mem p ua))
                && List.assoc_opt p ctx = Some v
                && element_independent v
              then (kept, ctx)
              else (d :: kept, (p, v) :: List.remove_assoc p ctx))
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
    (* Properties a kept rule can override must stay in the cascade. *)
    let dyn = dynamic_props SSet.empty stmts in
    let is_dyn d =
      SSet.mem (String.lowercase_ascii (Declaration.property_name d)) dyn
    in
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
