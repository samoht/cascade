let () =
  Alcotest.run "browser" [ Test_browser.suite; Test_browser_compare.suite ]
