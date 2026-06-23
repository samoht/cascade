(* Project a stylesheet onto an element tree, producing the inline-style
   declarations for each element plus the rules that have no inline form
   (:hover, @media, @keyframes, ...). The cascade itself is {!Resolve}; this
   adds the inline-specific policy. It is pure: it returns the declarations to
   apply, it does not mutate the tree (the caller writes them onto each
   node). *)

module SSet = Set.Make (String)

(* Selectors that can be written as an inline style (no state / condition). *)
let inlinable (sel : Selector.t) =
  let rec ok = function
    | Selector.Universal _ | Selector.Element _ | Selector.Class _
    | Selector.Id _ | Selector.Attribute _ | Selector.Root
    | Selector.First_child | Selector.Last_child | Selector.Only_child
    | Selector.Empty ->
        true
    | Selector.Compound ps
    | Selector.List ps
    | Selector.Is ps
    | Selector.Where ps
    | Selector.Not ps ->
        List.for_all ok ps
    | Selector.Combined (a, _, b) -> ok a && ok b
    | _ -> false
  in
  ok sel

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

(* Every property a kept (conditional / stateful) rule can set, recursing into
   @media / @supports / @container / @layer blocks. *)
let rec props_of_stmts acc stmts =
  List.fold_left
    (fun acc s ->
      match s with
      | Stylesheet.Rule r ->
          List.fold_left
            (fun a d ->
              SSet.add (String.lowercase_ascii (Declaration.property_name d)) a)
            acc
            (Stylesheet.declarations r)
      | Stylesheet.Media (_, b)
      | Stylesheet.Supports (_, b)
      | Stylesheet.Layer (_, b) ->
          props_of_stmts acc b
      | Stylesheet.Container (_, _, b) -> props_of_stmts acc b
      | _ -> acc)
    acc stmts

type 'node assignment = 'node * Declaration.declaration list
(** The inline-style declarations to write onto a node. *)

type 'node result = {
  styles : 'node assignment list;
      (** Each element with the declarations to set on its [style] attribute. *)
  keep_css : string;
      (** The rules with no inline form, serialised as a [<style>] body. *)
  kept : int;  (** Count of kept rules, for reporting. *)
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

  let compute ?(minimal = false) ~css roots =
    match Css.of_string css with
    | Error _ -> { styles = []; keep_css = ""; kept = 0 }
    | Ok p ->
        (* Flatten nesting up front, so the static/dynamic split and
           {!R.resolve} see the same flat rules. *)
        let stmts = Flatten.block p.Css.stylesheet in
        (* Properties a kept rule can override must stay in the cascade. *)
        let dyn =
          props_of_stmts SSet.empty
            (List.filter
               (function
                 | Stylesheet.Rule r -> not (inlinable (Stylesheet.selector r))
                 | _ -> true)
               stmts)
        in
        let is_dyn d =
          SSet.mem (String.lowercase_ascii (Declaration.property_name d)) dyn
        in
        let inline_rules = ref [] in
        let keep =
          List.filter_map
            (fun stmt ->
              match stmt with
              | Stylesheet.Rule r when inlinable (Stylesheet.selector r) -> (
                  let sel = Stylesheet.selector r
                  and ds = Stylesheet.declarations r in
                  (match List.filter (fun d -> not (is_dyn d)) ds with
                  | [] -> ()
                  | inl ->
                      inline_rules :=
                        Stylesheet.Rule (Stylesheet.rule ~selector:sel inl)
                        :: !inline_rules);
                  match List.filter is_dyn ds with
                  | [] -> None
                  | res ->
                      Some (Stylesheet.Rule (Stylesheet.rule ~selector:sel res))
                  )
              | other -> Some other)
            stmts
        in
        let keep_css =
          if keep = [] then ""
          else Css.to_string ~minify:true (Stylesheet.v keep)
        in
        let inline_sheet = Stylesheet.v (List.rev !inline_rules) in
        let styles =
          List.fold_left
            (fun acc root -> walk ~minimal inline_sheet [] root acc)
            [] roots
          |> List.rev
        in
        { styles; keep_css; kept = List.length keep }
end
