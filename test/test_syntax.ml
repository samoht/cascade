open Cascade

let check_true name actual = Alcotest.(check bool) name true actual
let check_false name actual = Alcotest.(check bool) name false actual

let is_ascii_ident_start () =
  check_true "letter" (Syntax.is_ascii_ident_start 'a');
  check_true "uppercase" (Syntax.is_ascii_ident_start 'Z');
  check_true "underscore" (Syntax.is_ascii_ident_start '_');
  check_true "hyphen" (Syntax.is_ascii_ident_start '-');
  check_false "digit" (Syntax.is_ascii_ident_start '0');
  check_false "space" (Syntax.is_ascii_ident_start ' ')

let is_ascii_ident_continue () =
  check_true "letter" (Syntax.is_ascii_ident_continue 'a');
  check_true "digit" (Syntax.is_ascii_ident_continue '5');
  check_true "hyphen" (Syntax.is_ascii_ident_continue '-');
  check_true "underscore" (Syntax.is_ascii_ident_continue '_');
  check_false "punctuation" (Syntax.is_ascii_ident_continue '!')

let is_ident () =
  (* CSS Syntax 3 sec. 4.3.11 for the opening code points, sec. 4.2 for the
     rest. *)
  check_true "letters" (Syntax.is_ident "foo");
  check_true "underscore start" (Syntax.is_ident "_x");
  check_true "digit and hyphen inside" (Syntax.is_ident "a-1");
  check_true "dash then ident-start" (Syntax.is_ident "-foo");
  check_true "dashed-ident" (Syntax.is_ident "--foo");
  (* A second [-] opens an ident, so the reserved custom-property name is one
     lexically even though it names no property. *)
  check_true "two dashes" (Syntax.is_ident "--");
  check_false "empty" (Syntax.is_ident "");
  (* A lone [-] is a delim, and [-] before a digit opens no ident, so both slip
     past a scan that reads [-] as ident-start. *)
  check_false "bare hyphen" (Syntax.is_ident "-");
  check_false "dash then digit" (Syntax.is_ident "-5px");
  check_false "leading digit" (Syntax.is_ident "5px");
  check_false "space" (Syntax.is_ident "a b");
  check_false "backslash is no ident code point" (Syntax.is_ident "a\\b")

let is_ident_non_ascii () =
  (* Sec. 4.2 lists the non-ASCII ident code points as ranges rather than
     admitting every code point [>= U+0080]. U+00B7, U+00F6 and U+200C are in
     the list. *)
  check_true "U+00B7" (Syntax.is_ident "\xc2\xb7mid");
  check_true "U+00F6" (Syntax.is_ident "f\xc3\xb6o");
  check_true "U+200C" (Syntax.is_ident "\xe2\x80\x8cz");
  (* U+00D7 falls in the gap between U+00C0..U+00D6 and U+00D8..U+00F6, and
     U+2197 between U+2070..U+218F and U+2C00..U+2FEF. *)
  check_false "U+00D7" (Syntax.is_ident "a\xc3\x97b");
  check_false "U+2197" (Syntax.is_ident "text-\xe2\x86\x97");
  (* A byte [>= 0x80] is not a code point: a lone continuation byte decodes to
     nothing an ident can hold. *)
  check_false "lone continuation byte" (Syntax.is_ident "a\x80b")

let is_hex () =
  check_true "digit" (Syntax.is_hex '0');
  check_true "lower a-f" (Syntax.is_hex 'a');
  check_true "upper A-F" (Syntax.is_hex 'F');
  check_false "outside" (Syntax.is_hex 'g');
  check_false "non-letter" (Syntax.is_hex '!')

let url_needs_quotes () =
  check_false "plain" (Syntax.url_needs_quotes "foo.css");
  check_true "space" (Syntax.url_needs_quotes "foo bar.css");
  check_true "quote" (Syntax.url_needs_quotes "foo\"bar");
  check_true "paren" (Syntax.url_needs_quotes "foo(bar)")

let strip_url_suffix () =
  (* WHATWG URL sec. 4.4 path state, as RFC 3986 sec. 3.3: the path ends at the
     first [?] or [#]. *)
  Alcotest.(check string)
    "no suffix" "foo.css"
    (Syntax.strip_url_suffix "foo.css");
  Alcotest.(check string)
    "query" "foo.css"
    (Syntax.strip_url_suffix "foo.css?v=1");
  Alcotest.(check string)
    "fragment" "foo.css"
    (Syntax.strip_url_suffix "foo.css#top");
  Alcotest.(check string)
    "both" "foo.css"
    (Syntax.strip_url_suffix "foo.css?v=1#top");
  Alcotest.(check string)
    "encoded delimiters stay in path" "foo%3Fbar%23baz.css"
    (Syntax.strip_url_suffix "foo%3Fbar%23baz.css?v=1#top");
  (* The path is kept as written. Percent-encoding it would turn the backslash
     of a Windows path into [%5C], which no later step can tell from a [%5C] the
     author wrote. *)
  Alcotest.(check string)
    "a space stays as written" "foo bar.css"
    (Syntax.strip_url_suffix "foo bar.css");
  Alcotest.(check string)
    "a backslash stays as written" "c:\\a\\b c.css"
    (Syntax.strip_url_suffix "c:\\a\\b c.css?v=1")

let check_scheme name expected url =
  Alcotest.(check (option string)) name expected (Syntax.url_scheme url)

let url_scheme () =
  (* WHATWG URL sec. 4.4 scheme start state and scheme state, as RFC 3986 sec.
     3.1: an ASCII alpha, then ASCII alphanumerics, [+], [-] or [.], ended by
     [:]. The scheme is lowercased. *)
  check_scheme "https" (Some "https") "HTTPS://example.test/a.css";
  check_scheme "data" (Some "data") "data:text/css,a{}";
  check_scheme "every scheme code point" (Some "a+b-c.d") "a+b-c.d:x";
  (* A one-letter scheme is a scheme: WHATWG URL reads [C:/a.css] as scheme [c]
     with path [/a.css]. *)
  check_scheme "one letter" (Some "c") "C:/a.css";
  (* Scheme start state: a code point other than an ASCII alpha means no
     scheme. *)
  check_scheme "a digit opens no scheme" None "1:/x";
  check_scheme "an empty scheme" None ":a";
  (* Scheme state: a code point outside the scheme set before [:] starts the
     input over with no scheme. *)
  check_scheme "an underscore" None "a_b:x";
  check_scheme "a pipe" None "C|/a.css";
  check_scheme "a slash before the colon" None "a/b:c";
  check_scheme "no colon" None "a.css"

let url_file_path () =
  Alcotest.(check string)
    "encoded path becomes filesystem path" "foo?bar#baz.css"
    (Syntax.url_file_path "foo%3Fbar%23baz.css?v=1#top");
  Alcotest.(check string)
    "encoded space becomes filesystem space" "foo bar.css"
    (Syntax.url_file_path "foo%20bar.css");
  (* A drive path is the Windows filesystem convention [--inline-imports]
     follows on every OS, not URL syntax: WHATWG URL reads [C:] as a scheme. *)
  Alcotest.(check string)
    "a drive path keeps its drive" "C:/a b/c.css"
    (Syntax.url_file_path "C:/a%20b/c.css?v=1#top");
  Alcotest.(check string)
    "a drive path keeps its backslashes" "c:\\a\\b.css"
    (Syntax.url_file_path "c:\\a\\b.css");
  Alcotest.(check string)
    "a drive path survives the suffix strip" "c:\\a\\b c.css"
    (Syntax.url_file_path (Syntax.strip_url_suffix "c:\\a\\b c.css?v=1"));
  (* Windows resolves a drive-relative path against the current directory of
     that drive, so the drive stays. *)
  Alcotest.(check string)
    "a drive-relative path keeps its drive" "C:a.css"
    (Syntax.url_file_path "C:a.css");
  Alcotest.(check string)
    "a UNC path" "\\\\host\\share\\a.css"
    (Syntax.url_file_path "\\\\host\\share\\a.css");
  Alcotest.(check string)
    "a UNC path written with slashes keeps its host" "//host/share/a.css"
    (Syntax.url_file_path "//host/share/a.css");
  (* WHATWG URL sec. 4.4 scheme start state: a digit opens no scheme, so the
     colon is part of a relative path. *)
  Alcotest.(check string)
    "a digit before a colon is a path" "1:/x"
    (Syntax.url_file_path "1:/x");
  (* [C|] is a Windows drive letter (sec. 4.2) only in a file URL, or in a
     reference parsed against a file URL base, where the path state rewrites it
     to [C:]. An import resolves against the importing file's path, which is no
     URL, so [C|/a.css] is a relative path holding [|]. *)
  Alcotest.(check string)
    "a pipe after a letter is a path" "C|/a.css"
    (Syntax.url_file_path "C|/a.css");
  (* A file URL names a local file. Sec. 4.4 file state and path state keep a
     Windows drive letter as the first path segment, normalized to [C:], and the
     file host state reads [localhost] as the empty host. *)
  Alcotest.(check string)
    "a file URL keeps its drive" "C:/a.css"
    (Syntax.url_file_path "file:///C:/a.css?v=1#top");
  Alcotest.(check string)
    "a file URL without slashes" "C:/a.css"
    (Syntax.url_file_path "file:C:/a.css");
  Alcotest.(check string)
    "a file URL normalizes the drive letter" "C:/a b.css"
    (Syntax.url_file_path "file:///C|/a%20b.css");
  Alcotest.(check string)
    "a file URL on localhost" "C:/a.css"
    (Syntax.url_file_path "file://localhost/C:/a.css");
  Alcotest.(check string)
    "a bare drive in a file URL" "C:"
    (Syntax.url_file_path "file:///C:");
  (* "Starts with a Windows drive letter" (sec. 4.2) needs [/], [\], [?] or [#]
     after the letter and colon, so [C:x.css] is an ordinary segment. *)
  Alcotest.(check string)
    "a file URL segment that is no drive" "/C:x.css"
    (Syntax.url_file_path "file:///C:x.css");
  Alcotest.(check string)
    "a POSIX file URL" "/home/a.css"
    (Syntax.url_file_path "FILE:///home/a.css");
  (* A file URL host names the server of a UNC path. *)
  Alcotest.(check string)
    "a file URL host" "//host/share/a.css"
    (Syntax.url_file_path "file://host/share/a.css")

let remote_url () =
  (* Remote: a URL scheme other than [file] (WHATWG URL sec. 4.4 scheme state),
     or an authority (RFC 3986 sec. 4.2 network-path reference). *)
  check_true "https" (Syntax.is_remote_url "HTTPS://example.test/a.css");
  check_true "data" (Syntax.is_remote_url "data:text/css,a{}");
  check_true "two-letter scheme" (Syntax.is_remote_url "ab:/x");
  check_true "authority" (Syntax.is_remote_url "//example.test/a.css");
  check_false "relative" (Syntax.is_remote_url "a.css");
  (* Sec. 4.4 scheme start state: a digit opens no scheme. *)
  check_false "a digit before a colon" (Syntax.is_remote_url "1:/x");
  (* [|] ends no scheme, and [C|] is a drive letter only in a file URL. *)
  check_false "a pipe after a letter" (Syntax.is_remote_url "C|/a.css");
  (* WHATWG URL and RFC 3986 sec. 3.1 both read [C:] as a scheme; a drive path
     is local by the Windows filesystem convention [--inline-imports]
     follows. *)
  check_false "drive path" (Syntax.is_remote_url "C:/a/b.css");
  check_false "drive path with backslashes"
    (Syntax.is_remote_url "c:\\a\\b.css");
  check_false "drive-relative path" (Syntax.is_remote_url "C:a.css");
  check_false "UNC path" (Syntax.is_remote_url "\\\\host\\share\\a.css");
  check_false "file URL" (Syntax.is_remote_url "file:///C:/a.css");
  check_false "file URL without slashes" (Syntax.is_remote_url "file:C:/a.css");
  check_false "file URL with a host"
    (Syntax.is_remote_url "FILE://host/share/a.css")

let suite =
  ( "syntax",
    [
      Alcotest.test_case "is_ascii_ident_start" `Quick is_ascii_ident_start;
      Alcotest.test_case "is_ascii_ident_continue" `Quick
        is_ascii_ident_continue;
      Alcotest.test_case "is_ident" `Quick is_ident;
      Alcotest.test_case "is_ident_non_ascii" `Quick is_ident_non_ascii;
      Alcotest.test_case "is_hex" `Quick is_hex;
      Alcotest.test_case "url_needs_quotes" `Quick url_needs_quotes;
      Alcotest.test_case "strip_url_suffix" `Quick strip_url_suffix;
      Alcotest.test_case "url_scheme" `Quick url_scheme;
      Alcotest.test_case "url_file_path" `Quick url_file_path;
      Alcotest.test_case "remote_url" `Quick remote_url;
    ] )
