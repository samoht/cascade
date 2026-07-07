(** Tests for the DEFLATE transfer-size estimator. *)

open Cascade

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

let test_empty () =
  Alcotest.(check bool)
    "empty input costs only the wrapper" true
    (Gzip_size.estimate "" < 32)

let test_repetition_is_cheap () =
  let block = ".card{color:red;margin:0;padding:4px}" in
  let sheet = repeat 200 block in
  let estimate = Gzip_size.estimate sheet in
  Alcotest.(check bool)
    "200 identical rules estimate under a tenth of raw size" true
    (estimate * 10 < String.length sheet)

let test_noise_is_incompressible () =
  let s = noise 8192 in
  Alcotest.(check bool)
    "random bytes estimate near raw size" true
    (Gzip_size.estimate s * 10 > String.length s * 9)

let test_monotone_in_content () =
  let a = noise 4096 in
  let b = String.concat "" [ a; noise 4096 ] in
  Alcotest.(check bool)
    "more content never estimates smaller" true
    (Gzip_size.estimate a <= Gzip_size.estimate b)

let test_repeat_beats_distinct () =
  (* Same raw length: one declaration block repeated across rules vs distinct
     declarations per rule; the repeated form must estimate smaller, the
     property the factoring transfer gate relies on. [100 + i] keeps every index
     three digits wide so both variants have equal raw length. *)
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
    "same-length repeated declarations estimate smaller" true
    (Gzip_size.estimate repeated < Gzip_size.estimate distinct)

let test_window_bound () =
  (* A repeat farther back than 32 KiB cannot be referenced, so two copies of an
     incompressible block estimate about twice one copy. *)
  let block = noise 40000 in
  let one = Gzip_size.estimate block in
  let two = Gzip_size.estimate (String.concat "" [ block; block ]) in
  Alcotest.(check bool)
    "distant repeat pays full price" true
    (two * 10 > one * 19)

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
