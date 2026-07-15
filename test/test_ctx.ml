open Cascade

let test_fragment_defaults () =
  let ctx = Ctx.fragment in
  Alcotest.(check bool) "fragment scope" true (Ctx.scope ctx = `Fragment);
  Alcotest.(check bool) "lossless disabled" false (Ctx.lossless ctx);
  Alcotest.(check bool)
    "nothing registered" false
    (Ctx.registered ctx "--brand")

let test_of_scope_preserves_defaults () =
  let stylesheet = Ctx.of_scope (Some `Stylesheet) in
  Alcotest.(check bool)
    "stylesheet scope" true
    (Ctx.scope stylesheet = `Stylesheet);
  Alcotest.(check bool) "lossless default" false (Ctx.lossless stylesheet);
  Alcotest.(check bool)
    "registered default" false
    (Ctx.registered stylesheet "--brand");
  let lossless_fragment = Ctx.of_scope ~lossless:true None in
  Alcotest.(check bool)
    "none maps to fragment" true
    (Ctx.scope lossless_fragment = `Fragment);
  Alcotest.(check bool)
    "lossless override" true
    (Ctx.lossless lossless_fragment)

let test_pp () =
  Alcotest.(check string)
    "fragment pp"
    "{scope=fragment;lossless=false;aggressive=false;extend_lists=false;closed_world=false;objective=transfer;enforce_spec=false}"
    (Pp.to_string ~minify:true Ctx.pp Ctx.fragment);
  Alcotest.(check string)
    "stylesheet lossless pp"
    "{scope=stylesheet;lossless=true;aggressive=false;extend_lists=false;closed_world=false;objective=transfer;enforce_spec=false}"
    (Pp.to_string ~minify:true Ctx.pp
       (Ctx.of_scope ~lossless:true (Some `Stylesheet)))

let suite =
  ( "ctx",
    [
      Alcotest.test_case "fragment defaults" `Quick test_fragment_defaults;
      Alcotest.test_case "of_scope preserves defaults" `Quick
        test_of_scope_preserves_defaults;
      Alcotest.test_case "pp" `Quick test_pp;
    ] )
