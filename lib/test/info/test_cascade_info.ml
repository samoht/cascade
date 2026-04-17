let test_version_not_empty () =
  Alcotest.(check bool)
    "version is non-empty" true
    (String.length Cascade_info.version > 0)

let suite =
  ( "cascade_info",
    [ Alcotest.test_case "version not empty" `Quick test_version_not_empty ] )
