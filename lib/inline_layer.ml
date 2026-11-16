open Stylesheet
open Inline_scope

(** {1 Layer-decided custom-property folding} *)

(* A variable redefined across cascade layers on the same element is a real
   override, but - unlike an @media or dark-mode override - its winner is
   statically decidable, so it can be folded to that winner instead of kept
   live. Resolving the winner here is what lets [statements_for_inline] drop the
   [@layer] wrappers: flattened, the losing definitions would compete by
   document order and pick the wrong value. *)

(* Document-order cascade layer names (dotted for nesting), low precedence
   first: the order in which layers are first introduced (css-cascade-5 sec.
   6.4.2). Keyed by the CSS text of the whole path, so a [.] one ident carries
   is not the separator between two (sec. 6.4.1). [None] when a layer sits where
   cascade cannot place it: a conditional group introduces its layers only when
   its condition holds, and an [Origin] block carries its own layer stack. What
   a [@container], [@scope] or [@starting-style] block holds, and what a rule
   nests, always exists, so a layer named there counts where it is written. *)
let layer_order stylesheet : string list option =
  let seen = Hashtbl.create 16 in
  let order = ref [] in
  let undecided = ref false in
  let add path =
    let name = Stylesheet.string_of_layer_name path in
    if not (Hashtbl.mem seen name) then begin
      Hashtbl.add seen name ();
      order := name :: !order
    end
  in
  let rec walk ~placed prefix stmts = List.iter (statement ~placed prefix) stmts
  and statement ~placed prefix stmt =
    match stmt with
    | Stylesheet.Layer_decl names ->
        if not placed then undecided := true;
        List.iter (fun n -> add (prefix @ n)) names
    | Stylesheet.Layer (name, body) ->
        if not placed then undecided := true;
        let prefix =
          match name with
          | None -> prefix
          | Some n ->
              let full = prefix @ n in
              add full;
              full
        in
        walk ~placed prefix body
    | Stylesheet.Media (_, body)
    | Stylesheet.Supports (_, body)
    | Stylesheet.Moz_document (_, body)
    | Stylesheet.When (_, body)
    | Stylesheet.Else (_, body)
    | Stylesheet.Origin (_, body) ->
        walk ~placed:false prefix body
    | Stylesheet.(
        ( Rule _ | Declarations _ | Bang_comment _ | Charset _ | Import _
        | Namespace _ | Property _ | Container _ | Starting_style _
        | Supports_condition _ | Scope _ | Keyframes _ | Webkit_keyframes _
        | Moz_keyframes _ | Font_face _ | Counter_style _ | Page _
        | Page_with_margins _ | Font_palette_values _ | Font_feature_values _
        | View_transition _ | Position_try _ | Viewport _ | Unknown_at_rule _ ))
      as stmt ->
        walk ~placed prefix (Stylesheet.statement_children stmt)
  in
  walk ~placed:true [] stylesheet;
  if !undecided then None else Some (List.rev !order)

module Slot_table = Hashtbl.Make (struct
  type t = Shorthand.overlap_key

  let equal = Shorthand.overlap_key_equal
  let hash = Shorthand.overlap_key_hash
end)

(* One declaration's stake in one cascade slot: where the layer stack ranks it,
   and where specificity and document order leave it once the wrappers are gone.
   [index] keeps both orders total, so two claims a single rule writes never
   compare equal. *)
type layer_claim = {
  rank : int;
  position : int;
  specificity : Selector.specificity;
  important : bool;
  index : int;
}

(* Where specificity and document order leave two claims once the wrappers are
   gone. Ascending, so the last claim standing is the one that wins. *)
let compare_flattened a b =
  match Bool.compare a.important b.important with
  | 0 -> (
      match Selector.compare_specificity a.specificity b.specificity with
      | 0 -> (
          match Int.compare a.position b.position with
          | 0 -> Int.compare a.index b.index
          | order -> order)
      | order -> order)
  | order -> order

(* And where the layer stack leaves them. Importance outranks the stack and
   comes through flattening untouched, so it settles a pair either way; between
   two layers the stack decides, reversed for important declarations (CSS
   Cascade 5 sec. 6.4.2); inside one layer the same things decide as after. *)
let compare_layered a b =
  match Bool.compare a.important b.important with
  | 0 ->
      let rank =
        if a.important then Int.compare b.rank a.rank
        else Int.compare a.rank b.rank
      in
      if rank <> 0 then rank else compare_flattened a b
  | order -> order

(* Two total orders over the same claims are the same order exactly when one's
   sequence is sorted under the other, so whatever subset of them an element
   matches, it keeps the winner it had. *)
let slot_survives_flattening claims =
  let rec ascending = function
    | a :: (b :: _ as rest) -> compare_flattened a b < 0 && ascending rest
    | _ -> true
  in
  ascending (List.stable_sort compare_layered claims)

(* A selector list matches an element through one branch at a time, and it is
   that branch's specificity the cascade weighs. *)
let selector_specificities selector =
  match Selector.as_list selector with
  | Some branches -> List.map Selector.specificity branches
  | None -> [ Selector.specificity selector ]

(* Record what one declaration stakes in every slot it writes. [false] when it
   is broad enough that no footprint tells it apart from anything. *)
let add_claim ~slots ~index ~rank ~position ~specificities decl =
  if Shorthand.declaration_is_broad decl then false
  else begin
    let important = Declaration.is_important decl in
    List.iter
      (fun specificity ->
        incr index;
        let claim =
          { rank; position; specificity; important; index = !index }
        in
        List.iter
          (fun slot ->
            let prev =
              Option.value ~default:[] (Slot_table.find_opt slots slot)
            in
            Slot_table.replace slots slot (claim :: prev))
          (Shorthand.declaration_overlap_keys decl))
      specificities;
    true
  end

(* Every declaration's stake in the slots it writes, indexed by slot, or [None]
   for a sheet holding what this does not reach: an anonymous layer, another
   origin's stack or a [@scope] block's proximity rule, a nested rule whose
   subject it does not resolve, a declaration broad enough to write any slot at
   all, or anything but a style rule inside a layer, [@keyframes] and friends
   included, whose name resolution the stack also orders. *)
let layer_claims ~ranks ~unlayered stylesheet =
  let slots = Slot_table.create 64 in
  let reaches = ref true in
  let position = ref 0 in
  let index = ref 0 in
  let rec walk ~layer ~rank stmts = List.iter (statement ~layer ~rank) stmts
  and statement ~layer ~rank stmt =
    match stmt with
    | Stylesheet.Layer (None, _) | Stylesheet.Origin _ | Stylesheet.Scope _ ->
        reaches := false
    | Stylesheet.Layer (Some name, body) ->
        let layer = layer @ name in
        let rank =
          Option.value ~default:unlayered
            (Hashtbl.find_opt ranks (Stylesheet.string_of_layer_name layer))
        in
        walk ~layer ~rank body
    | Stylesheet.Layer_decl _ -> ()
    | Stylesheet.Rule r ->
        incr position;
        if r.nested <> [] then reaches := false
        else
          let specificities = selector_specificities r.selector in
          let position = !position in
          List.iter
            (fun decl ->
              if
                not
                  (add_claim ~slots ~index ~rank ~position ~specificities decl)
              then reaches := false)
            r.declarations
    | Stylesheet.Media (_, body)
    | Stylesheet.Supports (_, body)
    | Stylesheet.Container (_, _, body)
    | Stylesheet.Moz_document (_, body)
    | Stylesheet.When (_, body)
    | Stylesheet.Else (_, body)
    | Stylesheet.Starting_style body ->
        walk ~layer ~rank body
    | Stylesheet.(
        ( Declarations _ | Bang_comment _ | Charset _ | Import _ | Namespace _
        | Property _ | Supports_condition _ | Keyframes _ | Webkit_keyframes _
        | Moz_keyframes _ | Font_face _ | Counter_style _ | Page _
        | Page_with_margins _ | Font_palette_values _ | Font_feature_values _
        | View_transition _ | Position_try _ | Viewport _ | Unknown_at_rule _ ))
      ->
        if rank <> unlayered then reaches := false
  in
  walk ~layer:[] ~rank:unlayered stylesheet;
  if !reaches then Some slots else None

(* Whether dropping every [@layer] wrapper leaves the same declaration winning
   each cascade slot. Unwrapping replays the layer stack as document order and
   hands the decision back to specificity, so it holds only where the two orders
   already agree on every slot two layers write, and [layer_order] is what ranks
   them: a sheet it cannot rank is one this cannot answer for. *)
let flattening_layers_is_safe stylesheet =
  match layer_order stylesheet with
  | None -> false
  | Some order -> (
      let ranks = Hashtbl.create 16 in
      List.iteri (fun rank name -> Hashtbl.replace ranks name rank) order;
      match layer_claims ~ranks ~unlayered:(List.length order) stylesheet with
      | None -> false
      | Some slots ->
          Slot_table.fold
            (fun _ claims ok -> ok && slot_survives_flattening claims)
            slots true)

(* [Some layer] for a purely-layered path ([layer = None] unlayered, [Some name]
   the dotted layer), [None] when the path is conditional or holds an anonymous
   layer - those are not statically foldable. *)
let foldable_layer_of_at_path at_path : string option option =
  (* Walking an innermost-first path and consing leaves [path] outermost-first,
     which is the order the dotted layer name reads in. *)
  let rec go path : at_node list -> string option option = function
    | [] -> (
        match path with
        | [] -> Some None
        | ns -> Some (Some (Stylesheet.string_of_layer_name (List.concat ns))))
    | Layer (Some n) :: rest -> go (n :: path) rest
    | Layer None :: _ -> None
    | _ :: _ -> None
  in
  go [] at_path

(* A copy of [d] tagged with [layer] and its own importance, for the cascade
   resolver, or [None] when [d] is not a pair this library writes back. *)
let annotate_layer (layer : string option) d =
  let name = Declaration.property_name d in
  let value = Declaration.string_of_value ~minify:true d in
  Option.map
    (fun base ->
      if Declaration.is_important d then Declaration.important base else base)
    (rebuild_custom ?layer name value)

(* Every definition of one name tagged with its layer, or [None] as soon as one
   of them is not a pair this library writes back: resolving a subset would
   resolve a different cascade. *)
let annotate_every_layer entries =
  List.fold_left
    (fun acc (_, fl, d) ->
      Option.bind acc (fun acc ->
          Option.bind fl (fun l ->
              Option.map (fun a -> a :: acc) (annotate_layer l d))))
    (Option.Some []) entries

(* Names whose every definition is unconditional, on one selector, and spread
   over two or more layers, mapped to the resolved cascade winner ([`Unset] when
   it resolves to no value). A single scope with repeated declarations is left
   alone: the census already treats it as one inlinable definition. *)
let layer_decided_customs ~keep stylesheet =
  let fold = Hashtbl.create 16 in
  match layer_order stylesheet with
  | None -> fold
  | Some layer_order ->
      let scopes = (collect_scopes ~kept:keep stylesheet).all in
      let by_name = Hashtbl.create 64 in
      List.iter
        (fun (s : scope) ->
          let sel = Selector.to_string ~minify:true s.selector in
          let fl = foldable_layer_of_at_path s.at_path in
          List.iter
            (fun d ->
              match custom_name d with
              | None -> ()
              | Some name ->
                  let prev =
                    Option.value ~default:[] (Hashtbl.find_opt by_name name)
                  in
                  Hashtbl.replace by_name name ((sel, fl, d) :: prev))
            s.customs)
        scopes;
      Hashtbl.iter
        (fun name entries ->
          let entries = List.rev entries in
          let unconditional =
            List.for_all (fun (_, fl, _) -> Option.is_some fl) entries
          in
          let one_selector =
            match entries with
            | (sel0, _, _) :: rest ->
                List.for_all (fun (s, _, _) -> s = sel0) rest
            | [] -> true
          in
          let distinct_layers =
            entries
            |> List.filter_map (fun (_, fl, _) -> fl)
            |> List.sort_uniq compare |> List.length
          in
          if unconditional && one_selector && distinct_layers >= 2 then
            match annotate_every_layer entries with
            | Option.None -> ()
            | Option.Some annotated -> (
                match
                  Context.winning_custom_declaration ~layer_order annotated
                with
                | Some w ->
                    Hashtbl.replace fold name
                      (`Value (Declaration.string_of_value ~minify:true w))
                | None -> Hashtbl.replace fold name `Unset))
        by_name;
      fold

(* Replace every layer-decided definition with a single unlayered definition of
   the winner (or drop it entirely when unset), so the census sees one inlinable
   definition. *)
let collapse_layer_decided ~keep stylesheet =
  let fold = layer_decided_customs ~keep stylesheet in
  if Hashtbl.length fold = 0 then stylesheet
  else
    let emitted = Hashtbl.create 16 in
    let filter_decls decls =
      List.filter_map
        (fun d ->
          match custom_name d with
          | Some n when Hashtbl.mem fold n -> (
              match Hashtbl.find fold n with
              | `Unset -> None
              | `Value _ when Hashtbl.mem emitted n -> None
              | `Value v ->
                  Hashtbl.add emitted n ();
                  Some (Option.value ~default:d (rebuild_custom n v)))
          | _ -> Some d)
        decls
    in
    let rec map_stmt stmt =
      match at_wrapper stmt with
      | Some (_, body, rebuild) -> rebuild (List.map map_stmt body)
      | None -> (
          match stmt with
          | Rule r ->
              Rule
                {
                  r with
                  declarations = filter_decls r.declarations;
                  nested = List.map map_stmt r.nested;
                }
          | Declarations decls -> Declarations (filter_decls decls)
          | Stylesheet.(
              ( Bang_comment _ | Charset _ | Import _ | Namespace _ | Property _
              | Layer_decl _ | Layer _ | Media _ | Container _ | Supports _
              | Moz_document _ | Starting_style _ | When _ | Else _
              | Supports_condition _ | Origin _ | Scope _ | Keyframes _
              | Webkit_keyframes _ | Moz_keyframes _ | Font_face _
              | Counter_style _ | Page _ | Page_with_margins _
              | Font_palette_values _ | Font_feature_values _
              | View_transition _ | Position_try _ | Viewport _
              | Unknown_at_rule _ )) as other ->
              other)
    in
    List.map map_stmt stylesheet
