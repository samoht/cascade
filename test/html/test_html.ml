(* What [Cascade_html.Html] keeps of a page and gives back. *)

module Html = Cascade_html.Html

let contains haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec at i =
    i + n <= h && (String.equal (String.sub haystack i n) needle || at (i + 1))
  in
  at 0

let tags doc = List.map (fun (e : Html.element) -> e.tag) (Html.roots doc)

(* A fragment is its own elements; a doctype makes it a whole document, with the
   [html] wrapper a browser builds around it. *)
let test_roots () =
  Alcotest.(check (list string))
    "a fragment" [ "p" ]
    (tags (Html.parse "<p>x</p>"));
  Alcotest.(check (list string))
    "a document" [ "html" ]
    (tags (Html.parse "<!doctype html><p>x</p>"))

(* A comment between two text nodes is content: it keeps them two nodes, and it
   is not a text child, so an element holding only a comment is still empty of
   text. *)
let test_comments_are_kept () =
  let doc = Html.parse "<p>v<!-- -->4.3</p><i><!-- c --></i>" in
  match Html.roots doc with
  | [ p; i ] ->
      Alcotest.(check (list string))
        "two text nodes" [ "v"; "4.3" ] (Html.text_children p);
      Alcotest.(check (list string))
        "a comment is no text" [] (Html.text_children i);
      Alcotest.(check bool)
        "printed back" true
        (contains (Html.to_string doc) "<!-- -->")
  | _ -> Alcotest.fail "expected two elements"

(* HTML sec. 2.4.7 splits a class attribute on ASCII whitespace, tabs and line
   breaks included, and an empty token is no class. *)
let test_classes () =
  match Html.roots (Html.parse "<p class=\" a\tb\n c  \">x</p>") with
  | [ p ] ->
      Alcotest.(check (list string)) "tokens" [ "a"; "b"; "c" ] (Html.classes p)
  | _ -> Alcotest.fail "expected one element"

(* A framework's prefixed attribute survives a round trip under the name the
   source wrote. *)
let test_prefixed_attribute_round_trips () =
  let doc = Html.parse "<button x-on:click=\"go()\">x</button>" in
  Alcotest.(check bool)
    "name kept" true
    (contains (Html.to_string doc) "x-on:click=\"go()\"")

let suite =
  ( "html",
    [
      Alcotest.test_case "roots" `Quick test_roots;
      Alcotest.test_case "comments are kept" `Quick test_comments_are_kept;
      Alcotest.test_case "classes" `Quick test_classes;
      Alcotest.test_case "prefixed attribute round trips" `Quick
        test_prefixed_attribute_round_trips;
    ] )
