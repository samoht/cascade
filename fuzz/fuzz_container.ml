(** Fuzz tests for the CSS Container module. *)

open Cascade
open Alcobar

let byte_at buf i =
  if String.length buf = 0 then 0 else Char.code buf.[i mod String.length buf]

let pick xs buf i = List.nth xs (byte_at buf i mod List.length xs)
let name buf i = pick [ "card"; "sidebar"; "layout"; "main" ] buf i

let recovered_css label css =
  match Css.of_string ~strict:false css with
  | Ok parsed -> parsed
  | Error err ->
      failf "%s did not recover leniently: %s" label
        (Cascade.Error.to_string err)

let assert_invalid_container_contract label input =
  let css = "@container " ^ input ^ "{.x{color:red}}" in
  match Css.of_string ~strict:true css with
  | Ok parsed ->
      failf "%s parsed strictly as invalid container query: %S -> %S" label
        input
        (Css.to_string ~minify:true parsed.stylesheet)
  | Error _ ->
      let { Css.warnings; stylesheet; _ } = recovered_css label css in
      ignore (Css.to_string ~minify:true stylesheet : string);
      if warnings = [] then
        failf "%s recovered without a lenient warning: %S" label input

let raw buf i =
  (pick Cascade_spec_inventory.Query_grammar.container_positive buf i).input

let condition buf i =
  let open Css.Container in
  match byte_at buf i mod 5 with
  | 0 -> Min_width_rem (Float.of_int (byte_at buf (i + 1)) /. 4.)
  | 1 -> Min_width_px (byte_at buf (i + 1))
  | 2 -> Named (name buf (i + 1), Min_width_rem 24.)
  | 3 -> Named (name buf (i + 1), of_string (raw buf (i + 2)))
  | _ -> of_string (raw buf (i + 1))

let test_non_empty_string_output buf =
  let s = Css.Container.to_string (condition buf 0) in
  if s = "" then fail "container serialization produced an empty query"

let test_named_kind_matches_inner buf =
  let inner = condition buf 3 in
  let named = Css.Container.Named (name buf 0, inner) in
  if
    not
      (Css.Container.equal_kind (Css.Container.kind named)
         (Css.Container.kind inner))
  then fail "named container query changed kind bucket"

let test_compare_antisymmetric buf =
  let a = condition buf 0 in
  let b = condition buf 7 in
  let ab = Css.Container.compare a b in
  let ba = Css.Container.compare b a in
  if ab = 0 && ba <> 0 then fail "container compare equality not symmetric";
  if ab < 0 && ba <= 0 then fail "container compare not antisymmetric";
  if ab > 0 && ba >= 0 then fail "container compare not antisymmetric"

let test_compare_transitive buf =
  let sorted =
    List.sort Css.Container.compare
      [ condition buf 0; condition buf 5; condition buf 9 ]
  in
  match sorted with
  | [ a; b; c ] ->
      if Css.Container.compare a b > 0 || Css.Container.compare b c > 0 then
        fail "container compare sort result is not ordered"
  | _ -> fail "container sort changed list length"

let test_named_prefix_stable buf =
  let name = name buf 0 in
  let inner = condition buf 4 in
  let query = Css.Container.Named (name, inner) in
  let serialized = Css.Container.to_string query in
  let expected = name ^ " " ^ Css.Container.to_string inner in
  if serialized <> expected then
    failf "named container query serialization changed: %S <> %S" expected
      serialized

let test_container_context_shape buf =
  let open Css.Values in
  let ctx =
    {
      Css.Context.empty with
      container_width = Some (Px (float_of_int (byte_at buf 0 + 1)));
      container_height = Some (Px (float_of_int (byte_at buf 1 + 1)));
    }
  in
  if ctx.container_width = None || ctx.container_height = None then
    fail "container context dimensions were not preserved"

let test_raw_query_stable buf =
  let raw = raw buf 0 in
  let query = Css.Container.of_string raw in
  if Css.Container.to_string query <> raw then
    fail "raw container query serialization changed"

let test_spec_container_vectors buf =
  let open Css.Container in
  let row =
    pick Cascade_spec_inventory.Query_grammar.container_positive buf 0
  in
  let query = of_string row.input in
  let expected = row.expected in
  let actual = to_string query in
  if actual <> expected then
    failf "container spec vector changed: %S <> %S" expected actual

let test_invalid_container_vectors buf =
  let valid =
    pick Cascade_spec_inventory.Query_grammar.container_positive buf 0
  in
  let input =
    if byte_at buf 1 mod 2 = 0 then
      (pick Cascade_spec_inventory.Query_grammar.container_negative buf 2).input
    else
      (Cascade_spec_inventory.Query_grammar.mutate_invalid valid (byte_at buf 3))
        .input
  in
  assert_invalid_container_contract "invalid container query vector" input

(* ===== Soundness of the equivalence [Container.equal] decides ===== *)

(* [Container.equal] gates block merging: two [@container] blocks it calls equal
   have their declarations concatenated under one condition. So whatever it
   calls equal must select the same query containers, and that is a property,
   not a table:

   Container.equal a b => a and b match every sampled container state alike

   Only this direction. Two equivalent queries are free to compare unequal; that
   costs a merge and never correctness, and demanding the converse would mean
   deciding container query equivalence.

   The oracle is [Context.matches_container], which evaluates a query against a
   described query container the way a UA does. It knows nothing about how
   [equal] is derived, so it is free to disagree with it. *)

let px f : Css.Media.value = Length (Css.Values.Px f)
let em f : Css.Media.value = Length (Css.Values.Em f)
let rem f : Css.Media.value = Length (Css.Values.Rem f)

let sizes_in unit_ v =
  List.map
    (fun axis -> Css.Container.feature axis (unit_ v))
    [ "width"; "height"; "inline-size"; "block-size" ]

(* Sizes land exactly on every bound the generator and the spec inventory can
   emit, and on either side of it: an inclusive bound read as a strict one shows
   only at the bound itself. Each length unit appears on its own, since a
   normaliser that rewrites a bound must carry its unit across. *)
let sampled_sizes =
  let bounds =
    [ 0.; 9.; 10.; 11.; 20.; 24.; 30.; 45.; 60.; 100.; 400.; 700.; 1200. ]
  in
  List.concat_map
    (fun v -> [ sizes_in px v; sizes_in em v; sizes_in rem v ])
    bounds

(* The rest of what a query container exposes: the discrete size features, the
   computed values a [style()] query reads, the scroll state a [scroll-state()]
   query reads, and the name a [<container-name>] filters on. *)
let sampled_contexts =
  [
    (None, []);
    (None, [ Css.Container.feature "orientation" (Ident Portrait) ]);
    (None, [ Css.Container.feature "orientation" (Ident Landscape) ]);
    (None, [ Css.Container.feature "aspect-ratio" (Ratio (16, 9)) ]);
    (None, [ Css.Container.feature "aspect-ratio" (Ratio (4, 3)) ]);
    (None, [ Css.Container.style ~value:"dark" "--theme" ]);
    (None, [ Css.Container.style ~value:"featured" "--variant" ]);
    (None, [ Css.Container.style ~value:"15px" "--gap" ]);
    (None, [ Css.Container.style ~value:"red" "color" ]);
    (None, [ Css.Container.scroll_state "stuck" "top" ]);
    (None, [ Css.Container.scroll_state "stuck" "left" ]);
    (None, [ Css.Container.scroll_state "snapped" "block" ]);
    (None, [ Css.Container.scroll_state "scrollable" "inline" ]);
    (None, [ Css.Container.scroll_state "scrolled" "y" ]);
    (Some "card", []);
    ( Some "card",
      [
        Css.Container.style ~value:"featured" "--variant";
        Css.Container.scroll_state "stuck" "top";
      ] );
    (Some "sidebar", []);
    (Some "sidebar", [ Css.Container.style ~value:"dark" "--theme" ]);
  ]

let sampled_states =
  List.concat_map
    (fun size ->
      List.map
        (fun (container_name, extra) ->
          Css.Context.query ?container_name ~container_features:(size @ extra)
            ())
        sampled_contexts)
    sampled_sizes

(* The first sampled container state the two queries disagree on, if any. *)
let disagreement a b =
  List.find_opt
    (fun q ->
      Bool.compare
        (Css.Context.matches_container q a)
        (Css.Context.matches_container q b)
      <> 0)
    sampled_states

let report_disagreement label a b q =
  failf
    "%s called %S and %S equal, but they disagree on container state %s (name \
     %s)"
    label
    (Css.Container.to_string a)
    (Css.Container.to_string b)
    (String.concat " "
       (List.map Css.Container.to_string q.Css.Context.container_features))
    (Option.value ~default:"(none)" q.Css.Context.container_name)

let test_equal_is_sound buf =
  let a = condition buf 0 in
  let b = condition buf 7 in
  if Css.Container.equal a b then
    match disagreement a b with
    | Some q -> report_disagreement "Container.equal" a b q
    | None -> ()

(* Bound and function spellings the generator above never pairs, drawn so that a
   normaliser which flips a comparison, drops a unit, loses the container name
   or reads an escaped ident as the query it spells is caught. *)
let spelling_pair buf i =
  let open Css.Container in
  pick
    [
      ("(min-width: 10px)", "(width >= 10px)");
      ("(min-width: 10px)", "(width > 10px)");
      ("(min-width: 24rem)", "(width >= 24rem)");
      ("(max-inline-size: 30em)", "(inline-size <= 30em)");
      ("(30em >= inline-size)", "(inline-size <= 30em)");
      ("(30em < inline-size)", "(inline-size > 30em)");
      ("(30em <= inline-size <= 60em)", "(60em >= inline-size >= 30em)");
      ("(30em <= inline-size < 60em)", "(60em > inline-size >= 30em)");
      ("(min-width: 10px)", "(min-height: 10px)");
      ("(inline-size > 30em)", "(block-size > 30em)");
      ({|(inline-size\ \>\=\ 10px)|}, "(inline-size >= 10px)");
      ( {|(\31 0px\ \<\=\ inline-size\ \<\=\ 20px)|},
        "(10px <= inline-size <= 20px)" );
      ({|(orientation\:\ landscape)|}, "(orientation: landscape)");
      ("STYLE(--theme: dark)", "style(--theme: dark)");
      ("style(--theme: dark)", "style(--theme: light)");
      ("style(--theme:dark)", "style(--theme: dark)");
      ("SCROLL-STATE(stuck: top)", "scroll-state(stuck: top)");
      ("scroll-state(stuck: top)", "scroll-state(stuck: left)");
      ("card (inline-size > 30em)", "sidebar (inline-size > 30em)");
      ("card (inline-size > 30em)", "card (30em < inline-size)");
      ("not (inline-size > 30em)", "not (30em < inline-size)");
      ("theme(static)", "theme(dynamic)");
    ]
    buf i
  |> fun (a, b) -> (of_string a, of_string b)

let test_spelling_pairs_sound buf =
  let a, b = spelling_pair buf 0 in
  if Css.Container.equal a b then
    match disagreement a b with
    | Some q -> report_disagreement "Container.equal" a b q
    | None -> ()

(* Control: the sweep above only means something if it can see a wrong merge. An
   unknown container feature selects no query container (Conditional Rules 5
   sec. 5.4), so these two queries match different containers. If the sweep
   cannot separate them it cannot separate anything. *)
let test_sweep_catches_wrong_equality _buf =
  let escaped = Css.Container.of_string {|(inline-size\ \>\=\ 10px)|} in
  let real = Css.Container.of_string "(inline-size >= 10px)" in
  match disagreement escaped real with
  | Some _ -> ()
  | None ->
      fail
        "the sampled container states cannot tell an unknown container feature \
         from the size range it spells"

let suite =
  ( "container",
    [
      test_case "to_string non-empty" [ bytes ] test_non_empty_string_output;
      test_case "named kind matches inner" [ bytes ]
        test_named_kind_matches_inner;
      test_case "compare antisymmetric" [ bytes ] test_compare_antisymmetric;
      test_case "compare transitive" [ bytes ] test_compare_transitive;
      test_case "named serialization keeps name prefix" [ bytes ]
        test_named_prefix_stable;
      test_case "container context shape invariant" [ bytes ]
        test_container_context_shape;
      test_case "raw style/scroll-state serialization stable" [ bytes ]
        test_raw_query_stable;
      test_case "spec container query vectors" [ bytes ]
        test_spec_container_vectors;
      test_case "invalid container query vectors rejected" [ bytes ]
        test_invalid_container_vectors;
      test_case "equal is sound" [ bytes ] test_equal_is_sound;
      test_case "equal is sound on spelling pairs" [ bytes ]
        test_spelling_pairs_sound;
      test_case "state sweep catches a wrong equality" [ bytes ]
        test_sweep_catches_wrong_equality;
    ] )
