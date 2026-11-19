(* The cascade-layer half of the canonical projection: the layer pins that
   declare nothing, the order of layer blocks, and the one [@layer] statement a
   run of them normalises to. *)

open Stylesheet

(* CSS Cascade 5 sec. 6.4.3: cascade layers are sorted by the order in which
   they first are declared, so all an [@layer a;] statement contributes is the
   position it gives [a] in that order (sec. 6.4.4.2). A name whose removal
   leaves the whole order untouched contributes nothing, and the projection
   drops it, so [@layer a;@layer a{...}] and [@layer a{...}] read as one sheet.
   Emission keeps the statement: it is visible through the CSSOM, and a sheet
   concatenated after this one can bind the position it fixes. *)

(* One position in the layer order. [Named] dedups by layer name, a
   re-declaration naming no new layer. [Opaque] stands for a position whose
   names cannot be read here: an anonymous layer (sec. 6.4.2.1), the layers an
   [@import] carries in with it, or a layer declared inside a conditional group
   rule, which sec. 6.4.3 has contribute to the order only when the condition
   holds. Each carries the index of the statement that raised it, so the two
   orders being weighed line their opaque positions up while no name ever dedups
   into one: a pin whose name is refilled across an unreadable position is left
   alone, which is what makes reading the sheet this coarsely safe. *)
type slot = Named of layer_name | Opaque of int

let equal_slot a b =
  match (a, b) with
  | Named a, Named b -> equal_layer_name a b
  | Opaque a, Opaque b -> Int.equal a b
  | (Named _ | Opaque _), _ -> false

(* Sec. 6.4.2: [a.b] is shorthand for those layers nested in order, so naming a
   sublayer declares each of its parents first. *)
let layer_name_slots (name : layer_name) : slot list =
  let rec loop acc rev_prefix = function
    | [] -> List.rev acc
    | ident :: rest ->
        let rev_prefix = ident :: rev_prefix in
        loop (Named (List.rev rev_prefix) :: acc) rev_prefix rest
  in
  loop [] [] name

let rec declares_layer (stmts : statement list) : bool =
  List.exists
    (fun stmt ->
      match stmt with
      | Layer _ | Layer_decl _ | Import _ -> true
      | ( Rule _ | Declarations _ | Bang_comment _ | Charset _ | Namespace _
        | Property _ | Media _ | Container _ | Supports _ | Moz_document _
        | Starting_style _ | When _ | Else _ | Supports_condition _ | Origin _
        | Scope _ | Keyframes _ | Webkit_keyframes _ | Moz_keyframes _
        | Font_face _ | Counter_style _ | Page _ | Page_with_margins _
        | Font_palette_values _ | Font_feature_values _ | View_transition _
        | Position_try _ | Viewport _ | Unknown_at_rule _ ) as stmt ->
          declares_layer (Stylesheet.statement_children stmt))
    stmts

(* The positions statement [i] adds to the layer order. A named [@layer] block
   or an [@import ... layer(x)] declares its own name, and whatever either holds
   inside is a sublayer sorting within it rather than a position out here. *)
let statement_slots i (stmt : statement) : slot list =
  match stmt with
  | Layer_decl names -> List.concat_map layer_name_slots names
  | Layer (Some name, _) -> layer_name_slots name
  | Import { layer = Some (_ :: _ as name); _ } -> layer_name_slots name
  | Layer (None, _) | Import _ -> [ Opaque i ]
  | Rule _ | Declarations _ | Bang_comment _ | Charset _ | Namespace _
  | Property _ | Media _ | Container _ | Supports _ | Moz_document _
  | Starting_style _ | When _ | Else _ | Supports_condition _ | Origin _
  | Scope _ | Keyframes _ | Webkit_keyframes _ | Moz_keyframes _ | Font_face _
  | Counter_style _ | Page _ | Page_with_margins _ | Font_palette_values _
  | Font_feature_values _ | View_transition _ | Position_try _ | Viewport _
  | Unknown_at_rule _ ->
      if declares_layer (Stylesheet.statement_children stmt) then [ Opaque i ]
      else []

(* [fixed.(i)] is what statement [i] contributes when it is not a pin, read
   once; [pins.(i)] is the names a pin still carries, which the pruning below
   shortens. A pin left with no name contributes nothing and keeps its index, so
   the opaque positions stay keyed the way [target] saw them. *)
let layer_order ~(fixed : slot list array)
    ~(pins : layer_name list option array) =
  let add (seen, rev) slot =
    match slot with
    | Named name when List.exists (equal_layer_name name) seen -> (seen, rev)
    | Named name -> (name :: seen, slot :: rev)
    | Opaque _ -> (seen, slot :: rev)
  in
  let rec loop i acc =
    if i >= Array.length fixed then List.rev (snd acc)
    else
      let slots =
        match pins.(i) with
        | None -> fixed.(i)
        | Some names -> List.concat_map layer_name_slots names
      in
      loop (i + 1) (List.fold_left add acc slots)
  in
  loop 0 ([], [])

(* Weigh one pin's names left to right, each candidate against the order the
   sheet came in with rather than against the last candidate, so every name kept
   is one the sheet's own order needs. *)
let rec prune_pin ~(fixed : slot list array)
    ~(pins : layer_name list option array) ~target i kept_rev = function
  | [] -> List.rev kept_rev
  | name :: rest ->
      pins.(i) <- Some (List.rev_append kept_rev rest);
      if List.equal equal_slot (layer_order ~fixed ~pins) target then
        prune_pin ~fixed ~pins ~target i kept_rev rest
      else begin
        pins.(i) <- Some (List.rev_append kept_rev (name :: rest));
        prune_pin ~fixed ~pins ~target i (name :: kept_rev) rest
      end

(* Top level only. A pin written inside a [@layer] block names a sublayer of it,
   and the sublayer order runs across every block of that layer rather than
   within one, so a block on its own does not say whether such a pin binds. *)
let fold_layer_pins (stmts : statement list) : statement list =
  let is_pin = function Layer_decl (_ :: _) -> true | _ -> false in
  if not (List.exists is_pin stmts) then stmts
  else
    let work = Array.of_list stmts in
    let pins : layer_name list option array =
      Array.map (function Layer_decl names -> Some names | _ -> None) work
    in
    let fixed =
      Array.mapi
        (fun i stmt ->
          match stmt with Layer_decl _ -> [] | stmt -> statement_slots i stmt)
        work
    in
    let target = layer_order ~fixed ~pins in
    (* One sweep is not enough: a name only reads as needed while a later one it
       is weighed against is still there, and drops out once that one goes. Each
       sweep only removes, so repeating to a fixed point terminates, and every
       candidate is still weighed against the order the sheet came in with. *)
    let sweep () =
      let dropped = ref false in
      Array.iteri
        (fun i names ->
          match names with
          | Some (_ :: _ as names) ->
              let kept = prune_pin ~fixed ~pins ~target i [] names in
              if List.compare_lengths kept names <> 0 then dropped := true
          | Some [] | None -> ())
        pins;
      !dropped
    in
    let rec settle changed = if sweep () then settle true else changed in
    if not (settle false) then stmts
    else
      let rebuild i (stmt : statement) : statement option =
        match (stmt, pins.(i)) with
        | Layer_decl _, Some [] -> None
        | Layer_decl _, Some (_ :: _ as names) -> Some (Layer_decl names)
        | stmt, (None | Some _) -> Some stmt
      in
      List.filter_map Fun.id (List.mapi rebuild stmts)

(* Sec. 6.1 sorts declarations by layer, in the order sec. 6.4.3 gives the
   layers, before order of appearance, so the blocks of two different layers
   decide no tie between themselves whichever comes first. A run of layer blocks
   therefore reads in the sheet's layer order, a stable sort keeping two blocks
   of one layer in the order they came. Sorting by that order leaves the order
   itself where it was: a name the run is first to declare only moves behind
   names ranked before it, which were declared first.

   Only a block holding style rules alone moves: plain rules, under [@media],
   [@supports], [@container] or [@starting-style], whose rules sec. 6.1 sorts by
   layer like any other (CSS Transitions 2 sec. 3.1 has [@starting-style] rules
   cascade as ordinary style rules). It moves across what it decides nothing
   with: unlayered style rules of that same shape, which sec. 6.1 sorts apart
   from every layer; an [@property], whose registration holds for the whole
   document wherever it is written; and an [@keyframes], which a block of style
   rules neither defines nor overrides. Those keep their own order and come
   first, the blocks after them. A block holding anything else, a nested
   [@layer] above all, which writes into a sublayer another block may name
   outright ([a{@layer b}] and [a.b]), stays where it is and ends the run, as
   does an [@layer] statement, which fixes a position. A position in the order
   that cannot be read ({!Opaque}) leaves no rank to sort by, so the sheet is
   left as it came. The pins are folded again afterwards: one whose order the
   blocks now spell by themselves contributes nothing. *)
let rec style_only (stmt : statement) =
  match stmt with
  | Rule r -> r.nested = []
  | Media (_, b) | Supports (_, b) | Container (_, _, b) | Starting_style b ->
      List.for_all style_only b
  | Declarations _ | Bang_comment _ | Charset _ | Import _ | Namespace _
  | Property _ | Layer_decl _ | Layer _ | Moz_document _ | When _ | Else _
  | Supports_condition _ | Origin _ | Scope _ | Keyframes _ | Webkit_keyframes _
  | Moz_keyframes _ | Font_face _ | Counter_style _ | Page _
  | Page_with_margins _ | Font_palette_values _ | Font_feature_values _
  | View_transition _ | Position_try _ | Viewport _ | Unknown_at_rule _ ->
      false

(* What a layer block moves across without deciding anything with it. *)
let passes_layer_blocks (stmt : statement) =
  match stmt with
  | Property _ | Keyframes _ | Webkit_keyframes _ | Moz_keyframes _ -> true
  | stmt -> style_only stmt

(* The rank of each name in the sheet's layer order, or [None] when a position
   in that order cannot be read. *)
let layer_ranks (stmts : statement list) =
  let work = Array.of_list stmts in
  let pins =
    Array.map (function Layer_decl names -> Some names | _ -> None) work
  in
  let fixed =
    Array.mapi
      (fun i stmt ->
        match stmt with Layer_decl _ -> [] | stmt -> statement_slots i stmt)
      work
  in
  let order = layer_order ~fixed ~pins in
  if List.exists (function Opaque _ -> true | Named _ -> false) order then
    Option.None
  else
    Option.Some
      (fun name ->
        let rec find i = function
          | [] -> max_int
          | Named n :: _ when equal_layer_name n name -> i
          | _ :: rest -> find (i + 1) rest
        in
        find 0 order)

(* One run from the head of [stmts]: what passes, in order, the movable blocks
   with their rank, and the rest. [moved] is set when a block and something that
   passes change places. *)
let take_layer_run ~block ~moved stmts =
  let rec take others blocks = function
    | s :: rest as l -> (
        match block s with
        | Some r ->
            if others <> [] then moved := true;
            take others ((r, s) :: blocks) rest
        | None ->
            if passes_layer_blocks s then begin
              if blocks <> [] then moved := true;
              take (s :: others) blocks rest
            end
            else (List.rev others, List.rev blocks, l))
    | [] -> (List.rev others, List.rev blocks, [])
  in
  take [] [] stmts

let sort_layer_blocks (stmts : statement list) : statement list =
  match layer_ranks stmts with
  | Option.None -> stmts
  | Option.Some rank ->
      let block (stmt : statement) =
        match stmt with
        | Layer (Some name, body) when List.for_all style_only body ->
            Some (rank name)
        | _ -> None
      in
      let moved = ref false in
      let rec go = function
        | [] -> []
        | stmt :: rest
          when Option.is_none (block stmt) && not (passes_layer_blocks stmt) ->
            stmt :: go rest
        | stmts ->
            let others, blocks, rest = take_layer_run ~block ~moved stmts in
            let sorted =
              List.stable_sort (fun (a, _) (b, _) -> Int.compare a b) blocks
            in
            if not (List.for_all2 (fun (_, a) (_, b) -> a == b) blocks sorted)
            then moved := true;
            others @ List.map snd sorted @ go rest
      in
      let sorted = go stmts in
      if !moved then fold_layer_pins sorted else stmts

(* A sheet may name its layers in one [@layer a, b;] statement, in per-layer
   statements, or in the blocks themselves. Two sheets that establish the same
   layer order in the same places reach two different canonical forms, because
   the optimizer folds an empty layer block into a trailing declaration while
   the pin logic splits a leading multi-name statement, and the comparison then
   reports the two render-equal sheets as different. This is the normal form
   that removes the dependence on the spelling.

   A block's statements are read as runs separated by a barrier: a statement
   that is not itself a layer construct but declares a layer below it, such as
   an [@media] holding an [@layer] or an [@import] with a layer. A barrier's
   contribution to the order depends on its condition, so no name crosses one.
   Within a barrier-free run, every layer name the run introduces, each [a.b]
   path expanded to its prefixes [a] then [a.b], is written once in
   first-appearance order as one [@layer <names>;] statement at the run's first
   name-bearing construct. The run's other layer statements and empty layer
   blocks go, since the statement carries their only effect. Nothing moves past
   a barrier and the names keep first-appearance order, so the order the sheet
   renders with is unchanged, and a nested layer is normalized inside the block
   that holds it. *)

(* The prefixes a layer path declares, outermost first: [a.b] declares [a] then
   [a.b], and an ident that holds an escaped dot is one name with no prefixes to
   split. *)
let layer_name_prefixes (name : layer_name) : layer_name list =
  let rec go acc cur = function
    | [] -> List.rev acc
    | x :: rest ->
        let cur = cur @ [ x ] in
        go (cur :: acc) cur rest
  in
  go [] [] name

(* The layer names a run introduces, each path expanded, in first-appearance
   order and deduplicated. *)
let run_layer_names (run : statement list) : layer_name list =
  let seen = ref [] in
  let add name =
    List.iter
      (fun prefix ->
        if not (List.exists (Stylesheet.equal_layer_name prefix) !seen) then
          seen := !seen @ [ prefix ])
      (layer_name_prefixes name)
  in
  let names_of stmt =
    match stmt with
    | Layer (Some name, _) -> [ name ]
    | Layer_decl names -> names
    | Import rule -> (
        match Stylesheet.import_layer_name rule with
        | Some (_ :: _ as name) -> [ name ]
        | Some [] | None -> [])
    | Layer (None, _)
    | Rule _ | Declarations _ | Bang_comment _ | Charset _ | Namespace _
    | Property _ | Media _ | Container _ | Supports _ | Moz_document _
    | Starting_style _ | When _ | Else _ | Supports_condition _ | Origin _
    | Scope _ | Keyframes _ | Webkit_keyframes _ | Moz_keyframes _ | Font_face _
    | Counter_style _ | Page _ | Page_with_margins _ | Font_palette_values _
    | Font_feature_values _ | View_transition _ | Position_try _ | Viewport _
    | Unknown_at_rule _ ->
        []
  in
  List.iter (fun stmt -> List.iter add (names_of stmt)) run;
  !seen

(* Put a run's style-only layer blocks into rank order. A block holding a nested
   layer keeps its place, since the sublayer it holds contributes in source
   order; moving it would change which of two writes to the sublayer wins. *)
let sort_layer_blocks_by_rank (names : layer_name list) (stmts : statement list)
    : statement list =
  let rank name =
    let rec go i = function
      | [] -> max_int
      | x :: rest ->
          if Stylesheet.equal_layer_name x name then i else go (i + 1) rest
    in
    go 0 names
  in
  let name = function Layer (Some name, _) -> name | _ -> [] in
  let sortable = function
    | Layer (Some _, body) -> List.for_all style_only body
    | _ -> false
  in
  let rec go = function
    | [] -> []
    | block :: _ as stmts when sortable block ->
        let rec take acc = function
          | block :: rest when sortable block -> take (block :: acc) rest
          | rest -> (List.rev acc, rest)
        in
        let blocks, rest = take [] stmts in
        List.stable_sort
          (fun a b -> Int.compare (rank (name a)) (rank (name b)))
          blocks
        @ go rest
    | stmt :: rest -> stmt :: go rest
  in
  go stmts

(* Rewrite one barrier-free run to its normal form: one [@layer <names>;]
   statement at the run's first name-bearing construct, the other declarations
   and empty layer blocks gone, the style-only blocks in rank order. *)
type layer_verdict = Keep | Drop | Other

let canonical_layer_run (run : statement list) : statement list =
  match run_layer_names run with
  | [] -> run
  | names ->
      let empty block =
        List.for_all
          (function
            | Rule { declarations = []; nested = []; _ } -> true
            | Rule _ | Declarations _ | Bang_comment _ | Charset _ | Import _
            | Namespace _ | Property _ | Layer_decl _ | Layer _ | Media _
            | Container _ | Supports _ | Moz_document _ | Starting_style _
            | When _ | Else _ | Supports_condition _ | Origin _ | Scope _
            | Keyframes _ | Webkit_keyframes _ | Moz_keyframes _ | Font_face _
            | Counter_style _ | Page _ | Page_with_margins _
            | Font_palette_values _ | Font_feature_values _ | View_transition _
            | Position_try _ | Viewport _ | Unknown_at_rule _ ->
                false)
          block
      in
      let verdict = function
        | Layer (Some _, block) -> if empty block then Drop else Keep
        | Layer_decl (_ :: _) -> Drop
        | Import rule -> (
            match Stylesheet.import_layer_name rule with
            | Some (_ :: _) -> Keep
            | Some [] | None -> Other)
        | Layer (None, _)
        | Layer_decl []
        | Rule _ | Declarations _ | Bang_comment _ | Charset _ | Namespace _
        | Property _ | Media _ | Container _ | Supports _ | Moz_document _
        | Starting_style _ | When _ | Else _ | Supports_condition _ | Origin _
        | Scope _ | Keyframes _ | Webkit_keyframes _ | Moz_keyframes _
        | Font_face _ | Counter_style _ | Page _ | Page_with_margins _
        | Font_palette_values _ | Font_feature_values _ | View_transition _
        | Position_try _ | Viewport _ | Unknown_at_rule _ ->
            Other
      in
      let emitted = ref false in
      let out = ref [] in
      let emit () =
        if not !emitted then (
          emitted := true;
          out := Layer_decl names :: !out)
      in
      List.iter
        (fun stmt ->
          match verdict stmt with
          | Keep ->
              emit ();
              out := stmt :: !out
          | Drop -> emit ()
          | Other -> out := stmt :: !out)
        run;
      sort_layer_blocks_by_rank names (List.rev !out)

(* A statement that is a layer construct itself, and one whose transitive
   content declares a layer, the second being a barrier for the run around
   it. *)
let is_layer_construct = function
  | Layer (Some _, _) | Layer_decl (_ :: _) -> true
  | Layer (None, _)
  | Layer_decl []
  | Rule _ | Declarations _ | Bang_comment _ | Charset _ | Import _
  | Namespace _ | Property _ | Media _ | Container _ | Supports _
  | Moz_document _ | Starting_style _ | When _ | Else _ | Supports_condition _
  | Origin _ | Scope _ | Keyframes _ | Webkit_keyframes _ | Moz_keyframes _
  | Font_face _ | Counter_style _ | Page _ | Page_with_margins _
  | Font_palette_values _ | Font_feature_values _ | View_transition _
  | Position_try _ | Viewport _ | Unknown_at_rule _ ->
      false

let rec statement_declares_layer stmt =
  is_layer_construct stmt
  || (match stmt with
    | Import rule -> (
        match Stylesheet.import_layer_name rule with
        | Some (_ :: _) -> true
        | Some [] | None -> false)
    | _ -> false)
  || Stylesheet.fold_statements
       (fun acc child -> acc || statement_declares_layer child)
       false
       (Stylesheet.statement_children stmt)

let is_barrier stmt =
  (not (is_layer_construct stmt)) && statement_declares_layer stmt

let rec canonical_layer_declarations (stmts : statement list) : statement list =
  let stmts =
    List.map
      (Stylesheet.map_statement_children canonical_layer_declarations)
      stmts
  in
  let rec go run acc = function
    | [] -> List.rev_append (canonical_layer_run (List.rev run)) acc
    | stmt :: rest when is_barrier stmt ->
        go []
          (stmt :: List.rev_append (canonical_layer_run (List.rev run)) acc)
          rest
    | stmt :: rest -> go (stmt :: run) acc rest
  in
  List.rev (go [] [] stmts)
