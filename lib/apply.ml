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

let parse_inline s =
  match Css.of_string ("a{" ^ s ^ "}") with
  | Ok p -> (
      match Stylesheet.rules p.Css.stylesheet with
      | r :: _ -> Stylesheet.declarations r
      | [] -> [])
  | Error _ -> []

let decl_value d =
  let s =
    Stylesheet.inline_style_of_declarations ~minify:true ~mode:Variables [ d ]
  in
  match String.index_opt s ':' with
  | Some i -> String.sub s (i + 1) (String.length s - i - 1)
  | None -> s

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
    match Node.attribute n "style" with
    | None -> author
    | Some s ->
        let overlay map d =
          let k = Declaration.property_name d in
          match List.assoc_opt k map with
          | Some cur
            when Declaration.is_important cur
                 && not (Declaration.is_important d) ->
              map
          | _ -> (k, d) :: List.remove_assoc k map
        in
        List.fold_left overlay
          (List.map (fun d -> (Declaration.property_name d, d)) author)
          (parse_inline s)
        |> List.rev_map snd

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
      let kept, ctx' =
        List.fold_left
          (fun (kept, ctx) d ->
            let p = String.lowercase_ascii (Declaration.property_name d) in
            if minimal && is_inherited p then
              let v = decl_value d in
              match List.assoc_opt p ctx with
              | Some pv when pv = v -> (kept, ctx)
              | _ -> (d :: kept, (p, v) :: List.remove_assoc p ctx)
            else (d :: kept, ctx))
          ([], ctx) (resolved sheet node)
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
