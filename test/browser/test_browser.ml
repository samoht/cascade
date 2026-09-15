(* [Browser] answers for the environment it runs in, so every case here builds
   its own: a directory it may write, stand-in executables it puts there, and
   the variables the locator reads set for the one call. Nothing is asked of the
   machine's real browsers. *)

let ( // ) = Filename.concat

let with_dir f =
  let dir =
    Filename.get_temp_dir_name ()
    // Fmt.str "cascade-test-browser-%d" (Unix.getpid ())
  in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  f dir

let write ?(mode = 0o644) path contents =
  Out_channel.with_open_bin path (fun oc -> output_string oc contents);
  Unix.chmod path mode

(* The variable is set for [f] alone and put back after, whatever [f] does. *)
let with_env name value f =
  let saved = Sys.getenv_opt name in
  Unix.putenv name value;
  Fun.protect
    ~finally:(fun () ->
      match saved with
      | Some v -> Unix.putenv name v
      | None -> Unix.putenv name "")
    f

let getenv_empty_is_unset () =
  with_env "CASCADE_TEST_EMPTY" "" (fun () ->
      Alcotest.(check (option string))
        "an empty variable is unset" None
        (Browser.getenv "CASCADE_TEST_EMPTY"));
  with_env "CASCADE_TEST_SET" "x" (fun () ->
      Alcotest.(check (option string))
        "a set variable is its value" (Some "x")
        (Browser.getenv "CASCADE_TEST_SET"))

let executable_is_a_runnable_file () =
  with_dir (fun dir ->
      let script = dir // "runnable" and plain = dir // "plain" in
      write ~mode:0o755 script "#!/bin/sh\nexit 0\n";
      write plain "not a program\n";
      Alcotest.(check bool) "a runnable file" true (Browser.executable script);
      Alcotest.(check bool)
        "a file without the execute bit" false (Browser.executable plain);
      Alcotest.(check bool) "a directory" false (Browser.executable dir);
      Alcotest.(check bool)
        "a path that does not exist" false
        (Browser.executable (dir // "missing")))

(* CHROME and NODE name the binary outright, and only an executable counts: a
   name that is not one falls through to the search rather than being
   answered. *)
let env_names_the_binary () =
  with_dir (fun dir ->
      let chrome = dir // "a-browser" and node = dir // "a-node" in
      write ~mode:0o755 chrome "#!/bin/sh\nexit 0\n";
      write ~mode:0o755 node "#!/bin/sh\nexit 0\n";
      with_env "CHROME" chrome (fun () ->
          Alcotest.(check (option string))
            "CHROME names the browser" (Some chrome) (Browser.chrome_binary ()));
      with_env "NODE" node (fun () ->
          Alcotest.(check (option string))
            "NODE names node" (Some node) (Browser.node_binary ()));
      let plain = dir // "plain" in
      write plain "";
      with_env "NODE" plain (fun () ->
          with_env "PATH" dir (fun () ->
              Alcotest.(check (option string))
                "a NODE that is not executable is not node, and an empty PATH \
                 has none"
                None (Browser.node_binary ()))))

(* The version is read off the banner the binary prints, whatever precedes the
   number; a binary that prints nothing has no version. *)
let version_from_the_banner () =
  with_dir (fun dir ->
      let banner text =
        let path = dir // "versioned" in
        write ~mode:0o755 path
          (String.concat "" [ "#!/bin/sh\necho '"; text; "'\n" ]);
        Browser.chrome_version path
      in
      Alcotest.(check (option (pair int int)))
        "a Chromium banner"
        (Some (153, 0))
        (banner "Chromium 153.0.7100.1");
      Alcotest.(check (option (pair int int)))
        "a Google Chrome banner"
        (Some (140, 0))
        (banner "Google Chrome 140.0.7339.207");
      Alcotest.(check (option (pair int int)))
        "a Chrome for Testing banner"
        (Some (153, 0))
        (banner "Google Chrome for Testing 153.0.8010.12");
      Alcotest.(check (option (pair int int)))
        "no number at all" None (banner "no version here");
      Alcotest.(check (option (pair int int)))
        "an empty banner" None (banner ""))

let suite =
  ( "browser",
    [
      Alcotest.test_case "an empty variable is unset" `Quick
        getenv_empty_is_unset;
      Alcotest.test_case "executable is a runnable file" `Quick
        executable_is_a_runnable_file;
      Alcotest.test_case "CHROME and NODE name the binary" `Quick
        env_names_the_binary;
      Alcotest.test_case "the version is read off the banner" `Quick
        version_from_the_banner;
    ] )
