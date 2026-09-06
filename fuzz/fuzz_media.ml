(** Fuzz tests for the CSS Media module. *)

open Cascade
open Alcobar

let byte_at buf i =
  if String.length buf = 0 then 0 else Char.code buf.[i mod String.length buf]

let pick xs buf i = List.nth xs (byte_at buf i mod List.length xs)
let cond f : Css.Media.t = Css.Media.Cond (Css.Media.Feature f)

let plain name value : Css.Media.t =
  cond (Css.Media.Plain (Css.Media.name_of_string name, value))

let not_all_and f : Css.Media.t =
  Css.Media.Type
    {
      prefix = Some Css.Media.Not;
      type_ = Css.Media.All;
      trailing = Some (Css.Media.Feature f);
    }

let media buf i =
  let open Css.Media in
  let px () = Length (Css.Values.Px (Float.of_int (byte_at buf (i + 1)))) in
  let rem () =
    Length (Css.Values.Rem (Float.of_int (byte_at buf (i + 1)) /. 4.))
  in
  match byte_at buf i mod 22 with
  | 0 -> plain "min-width" (px ())
  | 1 -> plain "max-width" (px ())
  | 2 -> not_all_and (Plain (Min Width, px ()))
  | 3 -> plain "min-width" (rem ())
  | 4 -> not_all_and (Plain (Min Width, rem ()))
  | 5 -> plain "prefers-reduced-motion" (Ident No_preference)
  | 6 -> plain "prefers-reduced-motion" (Ident Reduce)
  | 7 -> plain "prefers-contrast" (Ident More)
  | 8 -> plain "prefers-contrast" (Ident Less)
  | 9 -> plain "prefers-color-scheme" (Ident Dark)
  | 10 -> plain "prefers-color-scheme" (Ident Light)
  | 11 -> plain "forced-colors" (Ident Active)
  | 12 -> plain "forced-colors" (Ident Css.Media.None)
  | 13 -> plain "inverted-colors" (Ident Inverted)
  | 14 -> plain "pointer" (Ident Coarse)
  | 15 -> plain "pointer" (Ident Fine)
  | 16 -> plain "any-pointer" (Ident Coarse)
  | 17 -> plain "scripting" (Ident Enabled)
  | 18 -> plain "hover" (Ident Hover)
  | 19 -> Type { prefix = None; type_ = Print; trailing = None }
  | 20 -> plain "orientation" (Ident Landscape)
  | _ ->
      of_string
        (pick
           [
             "(width >= 40em)";
             "(30em <= width < 60em)";
             "(dynamic-range: high)";
             "(prefers-reduced-data: reduce)";
           ]
           buf (i + 2))

let test_non_empty_string_output buf =
  let s = Css.Media.to_string (media buf 0) in
  if s = "" then fail "media serialization produced an empty query"

let test_pp_matches_to_string buf =
  let query = media buf 0 in
  let direct = Css.Media.to_string query in
  let pretty = Css.Pp.to_string Css.Media.pp query in
  if direct <> pretty then
    failf "media pp/to_string mismatch: %S <> %S" direct pretty

let test_compare_antisymmetric buf =
  let a = media buf 0 in
  let b = media buf 7 in
  let ab = Css.Media.compare a b in
  let ba = Css.Media.compare b a in
  if ab = 0 && ba <> 0 then fail "media compare equality not symmetric";
  if ab < 0 && ba <= 0 then fail "media compare not antisymmetric";
  if ab > 0 && ba >= 0 then fail "media compare not antisymmetric"

let test_compare_transitive buf =
  let sorted =
    List.sort Css.Media.compare [ media buf 0; media buf 5; media buf 9 ]
  in
  match sorted with
  | [ a; b; c ] ->
      if Css.Media.compare a b > 0 || Css.Media.compare b c > 0 then
        fail "media compare sort result is not ordered"
  | _ -> fail "media sort changed list length"

let test_negated_kind_invariant buf =
  (* A double negation [not (not c)] is semantically equivalent to [c], so it
     must land in the same kind bucket. (A single negation flips a width lower
     bound into an upper bound, which legitimately changes the bucket.) *)
  match media buf 0 with
  | Css.Media.Cond c ->
      let query = Css.Media.Cond c in
      let twice = Css.Media.Cond (Css.Media.Not (Css.Media.Not c)) in
      if
        not (Css.Media.equal_kind (Css.Media.kind query) (Css.Media.kind twice))
      then fail "double-negated media query changed kind bucket"
  | _ -> ()

let test_media_context_shape buf =
  let open Css.Values in
  let ctx =
    {
      Css.Context.empty with
      viewport_width = Some (Px (float_of_int (byte_at buf 0 + 1)));
      viewport_height = Some (Px (float_of_int (byte_at buf 1 + 1)));
    }
  in
  if ctx.viewport_width = None || ctx.viewport_height = None then
    fail "media context dimensions were not preserved"

let test_raw_range_serialization_stable buf =
  let row = pick Cascade_spec_inventory.Query_grammar.media_positive buf 0 in
  let raw = row.input in
  let once = Css.Media.to_string (Css.Media.of_string raw) in
  let twice = Css.Media.to_string (Css.Media.of_string once) in
  if once <> twice then
    failf "media of_string/to_string not idempotent: %S vs %S" once twice

let spec_media_vector buf =
  let open Css.Media in
  let length l = Length l in
  let print_type = Type { prefix = None; type_ = Print; trailing = None } in
  pick
    [
      ("(min-width: 640px)", plain "min-width" (length (Css.Values.Px 640.)));
      ("(max-width: 768px)", plain "max-width" (length (Css.Values.Px 768.)));
      ( "(prefers-reduced-motion: reduce)",
        plain "prefers-reduced-motion" (Ident Reduce) );
      ("print", print_type);
      ("not print", Type { prefix = Some Not; type_ = Print; trailing = None });
      ( "not all and (min-width: 40px)",
        not_all_and (Plain (Min Width, length (Css.Values.Px 40.))) );
      ("(width > 40em)", cond (Range (Width, Gt, length (Css.Values.Em 40.))));
      ( "(40em < width)",
        cond (Range_rev (length (Css.Values.Em 40.), Lt, Width)) );
      ( "(30em <= width < 60em)",
        cond
          (Interval
             ( length (Css.Values.Em 30.),
               Le,
               Width,
               Lt,
               length (Css.Values.Em 60.) )) );
      ( "screen and (hover: hover)",
        Type
          {
            prefix = None;
            type_ = Screen;
            trailing = Some (Feature (Plain (Hover, Ident Hover)));
          } );
      ( "screen and (width >= 40em), print",
        List
          [
            Type
              {
                prefix = None;
                type_ = Screen;
                trailing =
                  Some (Feature (Range (Width, Ge, length (Css.Values.Em 40.))));
              };
            print_type;
          ] );
    ]
    buf 0

let test_spec_media_structural_vectors buf =
  let input, expected = spec_media_vector buf in
  let actual = Css.Media.of_string input in
  if not (Css.Media.equal expected actual) then
    failf "media vector parsed to wrong AST: %S -> %S" input
      (Css.Media.to_string actual)

let test_media_error_recovery buf =
  let row = pick Cascade_spec_inventory.Query_grammar.media_negative buf 0 in
  let input = row.input in
  let actual = Css.Media.to_string (Css.Media.of_string input) in
  if actual <> "not all" then
    failf "invalid media vector did not recover to not all: %S -> %S" input
      actual

let test_media_list_recovery buf =
  let row = pick Cascade_spec_inventory.Query_grammar.media_recovery buf 0 in
  let actual = Css.Media.to_string (Css.Media.of_string row.input) in
  if actual <> row.expected then
    failf "media list recovery drifted: %S -> %S, expected %S" row.input actual
      row.expected

let test_media_feature_family buf =
  let row = pick Cascade_spec_inventory.Query_grammar.media_positive buf 2 in
  let input = row.input in
  let once = Css.Media.to_string (Css.Media.of_string input) in
  let twice = Css.Media.to_string (Css.Media.of_string once) in
  if once <> twice then
    failf "media feature family serialization drifted: %S -> %S" once twice

let test_media_feature_recovery buf =
  let valid = pick Cascade_spec_inventory.Query_grammar.media_positive buf 3 in
  let input, expected =
    if byte_at buf 4 mod 2 = 0 then
      ( (pick Cascade_spec_inventory.Query_grammar.media_negative buf 5).input,
        "not all" )
    else
      let m =
        Cascade_spec_inventory.Query_grammar.mutate_invalid valid
          (byte_at buf 6)
      in
      (m.input, m.recovery)
  in
  let actual = Css.Media.to_string (Css.Media.of_string input) in
  if actual <> expected then
    failf "invalid media feature family did not recover: %S -> %S, expected %S"
      input actual expected

(* ===== Soundness of the equivalence [Media.equal] decides ===== *)

(* [Media.equal] gates block merging: two [@media] blocks it calls equal have
   their declarations concatenated under one condition. So whatever it calls
   equal must select the same media, and that is a property, not a table:

   Media.equal a b => a and b match every sampled media state alike

   Only this direction. Two equivalent queries are free to compare unequal; that
   costs a merge and never correctness, and demanding the converse would mean
   deciding media-query equivalence.

   The oracle is [Context.matches_media], which evaluates a query against a
   described environment the way a UA does. It knows nothing about how [equal]
   is derived, so it is free to disagree with it. *)

let px f : Css.Media.value = Length (Css.Values.Px f)
let em f : Css.Media.value = Length (Css.Values.Em f)

let state ?media_type features : Css.Context.query =
  { Css.Context.empty_query with media_type; media_features = features }

(* Widths land on, just below and just above every bound the generator can emit,
   because an inclusive bound read as a strict one only shows at the bound
   itself. Both length units appear, since a normaliser that rewrites a bound
   must carry its unit across. *)
let sampled_states =
  let widths = [ 0.; 1.; 9.; 10.; 11.; 39.; 40.; 41.; 255.; 256. ] in
  let sizes =
    List.concat_map
      (fun w ->
        [
          [
            Css.Media.feature "width" (px w); Css.Media.feature "height" (px w);
          ];
          [
            Css.Media.feature "width" (em w); Css.Media.feature "height" (em w);
          ];
        ])
      widths
  in
  let discrete =
    [
      [];
      [ Css.Media.feature "orientation" (Ident Landscape) ];
      [ Css.Media.feature "orientation" (Ident Portrait) ];
      [ Css.Media.feature "hover" (Ident Hover) ];
      [ Css.Media.feature "pointer" (Ident Coarse) ];
      [ Css.Media.feature "prefers-color-scheme" (Ident Dark) ];
      [ Css.Media.feature "prefers-reduced-motion" (Ident Reduce) ];
      [ Css.Media.feature "prefers-contrast" (Ident More) ];
      [ Css.Media.feature "forced-colors" (Ident Active) ];
      [ Css.Media.feature "scripting" (Ident Enabled) ];
      [ Css.Media.feature "dynamic-range" (Ident High) ];
      [ Css.Media.feature "prefers-reduced-data" (Ident Reduce) ];
    ]
  in
  let types = [ None; Some "screen"; Some "print"; Some "tv" ] in
  List.concat_map
    (fun media_type ->
      List.concat_map
        (fun size -> List.map (fun d -> state ?media_type (size @ d)) discrete)
        sizes)
    types

(* The first sampled state the two queries disagree on, if any. *)
let disagreement a b =
  List.find_opt
    (fun q ->
      Bool.compare
        (Css.Context.matches_media q a)
        (Css.Context.matches_media q b)
      <> 0)
    sampled_states

let report_disagreement label a b q =
  failf "%s called %S and %S equal, but they disagree on media state %s" label
    (Css.Media.to_string a) (Css.Media.to_string b)
    (String.concat " "
       (Option.value ~default:"(no type)" q.Css.Context.media_type
       :: List.map Css.Media.to_string q.Css.Context.media_features))

let test_equal_is_sound buf =
  let a = media buf 0 in
  let b = media buf 7 in
  if Css.Media.equal a b then
    match disagreement a b with
    | Some q -> report_disagreement "Media.equal" a b q
    | None -> ()

(* Bound spellings the generator above never pairs, drawn so that a normaliser
   which flips a comparison, drops a unit or loses the media type is caught. *)
let bound_pair buf i =
  let open Css.Media in
  pick
    [
      ("(min-width: 10px)", "(width >= 10px)");
      ("(min-width: 10px)", "(width > 10px)");
      ("(max-width: 40em)", "(width <= 40em)");
      ("(max-width: 40em)", "(40em >= width)");
      ("(10px <= width)", "(width >= 10px)");
      ("(10px < width)", "(width > 10px)");
      ("(10em <= width <= 40em)", "(40em >= width >= 10em)");
      ("(10em <= width < 40em)", "(40em > width >= 10em)");
      ("(min-height: 10px)", "(height >= 10px)");
      ("(min-width: 10px)", "(min-height: 10px)");
      ("all and (min-width: 10px)", "(width >= 10px)");
      ("screen and (min-width: 10px)", "screen and (width >= 10px)");
      ("screen and (min-width: 10px)", "print and (width >= 10px)");
      ("not all and (min-width: 10px)", "not (width >= 10px)");
      ({|screen\ and\ \(min-width\:\ 10px\)|}, "screen and (min-width: 10px)");
      ({|print\,screen|}, "print, screen");
      ("theme(static)", "theme(dynamic)");
      ("(unknown-feature: 1)", "theme(static)");
      ("(min-width: 10px), print", "(width >= 10px), print");
    ]
    buf i
  |> fun (a, b) -> (of_string a, of_string b)

let test_bound_spellings_sound buf =
  let a, b = bound_pair buf 0 in
  if Css.Media.equal a b then
    match disagreement a b with
    | Some q -> report_disagreement "Media.equal" a b q
    | None -> ()

(* Control: the sweep above only means something if it can see a wrong merge. An
   unknown media type never matches, so these two queries select different
   media. If the sweep cannot separate them it cannot separate anything. *)
let test_sweep_catches_wrong_equality _buf =
  let escaped = Css.Media.of_string {|screen\ and\ \(min-width\:\ 10px\)|} in
  let real = Css.Media.of_string "screen and (min-width: 10px)" in
  match disagreement escaped real with
  | Some _ -> ()
  | None ->
      fail
        "the sampled media states cannot tell an unknown media type from a \
         media type plus a condition"

let suite =
  ( "media",
    [
      test_case "to_string non-empty" [ bytes ] test_non_empty_string_output;
      test_case "pp matches to_string" [ bytes ] test_pp_matches_to_string;
      test_case "compare antisymmetric" [ bytes ] test_compare_antisymmetric;
      test_case "compare transitive" [ bytes ] test_compare_transitive;
      test_case "negated kind invariant" [ bytes ] test_negated_kind_invariant;
      test_case "media context shape invariant" [ bytes ]
        test_media_context_shape;
      test_case "raw range serialization stable" [ bytes ]
        test_raw_range_serialization_stable;
      test_case "spec media structural vectors" [ bytes ]
        test_spec_media_structural_vectors;
      test_case "spec media error recovery vectors" [ bytes ]
        test_media_error_recovery;
      test_case "spec media list recovery vectors" [ bytes ]
        test_media_list_recovery;
      test_case "spec media feature family vectors" [ bytes ]
        test_media_feature_family;
      test_case "spec media feature family recovery vectors" [ bytes ]
        test_media_feature_recovery;
      test_case "equal is sound" [ bytes ] test_equal_is_sound;
      test_case "equal is sound on bound spellings" [ bytes ]
        test_bound_spellings_sound;
      test_case "state sweep catches a wrong equality" [ bytes ]
        test_sweep_catches_wrong_equality;
    ] )
