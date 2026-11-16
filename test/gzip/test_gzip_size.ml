(** Tests for the DEFLATE transfer-size estimator. *)

open Cascade

(* Real DEFLATE at the level a server ships, so every claim below is scored
   against the thing the estimator models rather than against a threshold
   someone guessed. Each case asserts the property of [gzip] first and of
   [Gzip_size.estimate] second: the first line says the claim is true of the
   format, the second says the estimator answers it the same way, and a run that
   fails on the first line is telling you the case is wrong rather than the
   estimator. *)
let gzip s =
  let r = Bytesrw.Bytes.Reader.of_string s in
  let r =
    Bytesrw_zlib.Gzip.compress_reads ~level:Bytesrw_zlib.best_compression () r
  in
  String.length (Bytesrw.Bytes.Reader.to_string r)

(* Deterministic byte stream with no useful LZ structure (xorshift). *)
let noise n =
  let state = ref 0x2545F491 in
  String.init n (fun _ ->
      let x = !state in
      let x = x lxor (x lsl 13) in
      let x = x lxor (x lsr 17) in
      let x = x lxor (x lsl 5) in
      state := x land 0x3FFFFFFF;
      Char.chr (x land 0xff))

let repeat n s = String.concat "" (List.init n (fun _ -> s))

let both name p input =
  Alcotest.(check bool)
    (String.concat "" [ "gzip: "; name ])
    true
    (p (gzip input));
  Alcotest.(check bool)
    (String.concat "" [ "estimate: "; name ])
    true
    (p (Gzip_size.estimate input))

let test_empty () =
  both "empty input costs only the wrapper" (fun n -> n < 32) ""

let test_repetition_is_cheap () =
  let sheet = repeat 200 ".card{color:red;margin:0;padding:4px}" in
  both "200 identical rules cost under a tenth of raw size"
    (fun n -> n * 10 < String.length sheet)
    sheet

let test_noise_is_incompressible () =
  let s = noise 8192 in
  both "random bytes cost near raw size"
    (fun n -> n * 10 > String.length s * 9)
    s

let test_monotone_in_content () =
  let a = noise 4096 in
  let b = String.concat "" [ a; noise 4096 ] in
  Alcotest.(check bool)
    "gzip: more content never costs less" true
    (gzip a <= gzip b);
  Alcotest.(check bool)
    "estimate: more content never costs less" true
    (Gzip_size.estimate a <= Gzip_size.estimate b)

let test_repeat_beats_distinct () =
  (* One declaration block repeated across rules against distinct declarations
     per rule. The repeated form is the LONGER of the two raw and still the
     cheaper compressed, which is the whole reason the factoring transfer gate
     asks about compressed size instead of raw. [100 + i] keeps every index
     three digits wide so neither variant wins on digit count. *)
  let sel i = String.concat "" [ ".c"; string_of_int (100 + i) ] in
  let repeated =
    String.concat ""
      (List.init 100 (fun i ->
           String.concat "" [ sel i; "{margin:0;color:red}" ]))
  in
  let distinct =
    String.concat ""
      (List.init 100 (fun i ->
           String.concat ""
             [ sel i; "{margin:"; string_of_int (100 + i); "px 40em}" ]))
  in
  Alcotest.(check bool)
    "the repeated form is the longer one raw" true
    (String.length repeated > String.length distinct);
  Alcotest.(check bool)
    "gzip: and the cheaper one compressed" true
    (gzip repeated < gzip distinct);
  Alcotest.(check bool)
    "estimate: ranked the same way" true
    (Gzip_size.estimate repeated < Gzip_size.estimate distinct)

let test_window_bound () =
  (* A repeat farther back than 32 KiB cannot be referenced, so two copies of an
     incompressible block cost about twice one copy. *)
  let block = noise 40000 in
  Alcotest.(check bool)
    "gzip: a distant repeat pays full price" true
    (gzip (String.concat "" [ block; block ]) * 10 > gzip block * 19);
  Alcotest.(check bool)
    "estimate: a distant repeat pays full price" true
    (Gzip_size.estimate (String.concat "" [ block; block ]) * 10
    > Gzip_size.estimate block * 19)

let suite =
  ( "gzip_size",
    [
      Alcotest.test_case "empty" `Quick test_empty;
      Alcotest.test_case "repetition is cheap" `Quick test_repetition_is_cheap;
      Alcotest.test_case "noise is incompressible" `Quick
        test_noise_is_incompressible;
      Alcotest.test_case "monotone in content" `Quick test_monotone_in_content;
      Alcotest.test_case "repeat beats distinct" `Quick
        test_repeat_beats_distinct;
      Alcotest.test_case "window bound" `Quick test_window_bound;
    ] )
