(* The report [Browser_compare] writes for a reader, over reports built by hand:
   what it says about a run is read off the report alone, so no browser is
   needed to check it. *)

let difference ?(viewport = "1024x768") ?(state = "none") ?(pseudo = "")
    ~element property first second =
  { Browser_compare.viewport; state; element; pseudo; property; first; second }

let render ?(viewport = "1024x768") ?(state = "none") ?(size = "1024x768")
    ?(second_size = size) x y width height =
  {
    Browser_compare.viewport;
    state;
    x;
    y;
    width;
    height;
    first_size = size;
    second_size;
  }

let report ?(meta = []) ?renders differences =
  let renders =
    match renders with
    | Some rs -> rs
    | None ->
        List.sort_uniq compare
          (List.map
             (fun (d : Browser_compare.difference) ->
               render ~viewport:d.viewport ~state:d.state 0 0 10 10)
             differences)
  in
  {
    Browser_compare.meta =
      [
        ("browser", "152.0");
        ("viewports", "320x768 1024x768");
        ("states", "none hover");
        ("captures", string_of_int (2 * List.length renders));
        ("differences", string_of_int (List.length differences));
      ]
      @ meta;
    renders;
    differences;
  }

let text report =
  Browser_compare.to_string ~first:"a.css" ~second:"b.css" ~html:"doc.html"
    report

let contains haystack needle = Astring.String.is_infix ~affix:needle haystack

let occurrences haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec go i acc =
    if i + n > h then acc
    else if String.sub haystack i n = needle then go (i + n) (acc + 1)
    else go (i + 1) acc
  in
  go 0 0

(* A difference holding under every viewport and every state is written once,
   with no context after it: naming every pair would say nothing. *)
let test_everywhere_is_unqualified () =
  let everywhere =
    List.concat_map
      (fun viewport ->
        List.map
          (fun state ->
            difference ~viewport ~state ~element:"p" "color" "red" "blue")
          [ "none"; "hover" ])
      [ "320x768"; "1024x768" ]
  in
  let out = text (report everywhere) in
  Alcotest.(check int) "written once" 1 (occurrences out "color: red -> blue");
  Alcotest.(check bool)
    "with no context" true
    (contains out "  color: red -> blue\n")

(* One that holds under one state at every viewport is named by that state. *)
let test_one_state_is_named () =
  let hover =
    List.map
      (fun viewport ->
        difference ~viewport ~state:"hover" ~element:"p" "color" "red" "blue")
      [ "320x768"; "1024x768" ]
  in
  let other = difference ~state:"none" ~element:"q" "margin" "0px" "1px" in
  let out = text (report (other :: hover)) in
  Alcotest.(check bool)
    "named by the state" true
    (contains out "color: red -> blue  [hover]")

(* A pseudo-element repeating its element's difference in the same contexts is
   counted, not listed; one that differs on its own is listed. *)
let test_inherited_pseudo_is_counted () =
  let own = difference ~element:"p" "color" "red" "blue" in
  let inherited =
    difference ~element:"p" ~pseudo:"::before" "color" "red" "blue"
  in
  let distinct =
    difference ~element:"p" ~pseudo:"::after" "content" "\"x\"" "none"
  in
  let out = text (report [ own; inherited; distinct ]) in
  Alcotest.(check bool)
    "the inherited one is not listed" false (contains out "p::before");
  Alcotest.(check bool)
    "it is counted" true
    (contains out "1 pseudo-element value(s) repeat their element's difference");
  Alcotest.(check bool)
    "the distinct one is listed" true
    (contains out "p::after\n  content: \"x\" -> none")

(* A render that differs says where, in pixels, and one whose two captures are
   not the same size says how big each page laid out. *)
let test_renders_are_placed () =
  let r =
    report
      ~renders:
        [
          render ~state:"hover" 8 10 32 13;
          render ~viewport:"320x768" ~size:"320x900" ~second_size:"320x940" 0 0
            320 940;
        ]
      []
  in
  let out = text r in
  Alcotest.(check bool)
    "counted in the header" true
    (contains out "Renders that differ: 2");
  Alcotest.(check bool)
    "the box is named" true
    (contains out "  1024x768 hover: 32x13 pixels differ at (8,10)\n");
  Alcotest.(check bool)
    "a size change is named" true
    (contains out
       "  320x768 none: the page is 320x900 under the first sheet and 320x940 \
        under the second\n");
  Alcotest.(check bool)
    "the report is not identical" false
    (Browser_compare.identical r);
  Alcotest.(check bool)
    "a report with no render that differs is" true
    (Browser_compare.identical (report []))

let test_truncated_listing_says_so () =
  let r =
    report
      ~meta:[ ("truncated", "5000") ]
      [ difference ~element:"p" "color" "red" "blue" ]
  in
  Alcotest.(check bool)
    "the listing is marked short" true
    (contains (text r) ", listing the first 5000)")

let suite =
  ( "browser_compare",
    [
      Alcotest.test_case "everywhere is unqualified" `Quick
        test_everywhere_is_unqualified;
      Alcotest.test_case "one state is named" `Quick test_one_state_is_named;
      Alcotest.test_case "inherited pseudo is counted" `Quick
        test_inherited_pseudo_is_counted;
      Alcotest.test_case "renders are placed" `Quick test_renders_are_placed;
      Alcotest.test_case "truncated listing says so" `Quick
        test_truncated_listing_says_so;
    ] )
