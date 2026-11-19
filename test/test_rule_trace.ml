(* Rule_trace.settle on runs read straight from CSS text. A pair the cascade
   cannot tell apart must settle to one form and a pair it can must not; the
   expectations come from what an element of both classes computes, not from the
   form the code prints. *)

open Cascade

let statements css =
  match Css.of_string ~strict:false css with
  | Ok { stylesheet; _ } -> Css.statements stylesheet
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let render stmts = Pp.to_string ~minify:true Stylesheet.pp_stylesheet stmts
let ctx = Ctx.of_scope ~enforce_spec:false None
let settled css = render (Rule_trace.settle ~ctx (statements css))

let same msg a b =
  Alcotest.(check string) (Fmt.str "%s: %s" msg a) (settled a) (settled b)

let apart msg a b =
  Alcotest.(check bool)
    (Fmt.str "%s: %s against %s" msg a b)
    true
    (not (String.equal (settled a) (settled b)))

let empty_run () =
  Alcotest.(check string)
    "nothing settles to nothing" ""
    (render (Rule_trace.settle ~ctx []))

(* The interface prints a rule's independent declarations in text order. *)
let independent_declarations_sort () =
  Alcotest.(check string)
    "text order" ".a{color:red;width:1px}"
    (settled ".a{width:1px;color:red}")

(* [margin] writes [margin-top] after it on every element [.a] matches, so the
   longhand never wins (CSS Cascade 5 sec. 6.1, order of appearance). *)
let a_covered_longhand_goes () =
  Alcotest.(check string)
    "the shorthand after it covers it" ".a{margin:0}"
    (settled ".a{margin-top:1px;margin:0}")

(* A browser reads a fallback pair alike in one rule or in two, so where the
   rules begin and end decides nothing; nor do two identical writes. *)
let rule_boundaries_are_not_read () =
  same "a fallback pair split or joined"
    ".a{display:-webkit-box}.a{display:flex}"
    ".a{display:-webkit-box;display:flex}";
  same "an identical write repeated" ".a{color:red}.a{color:red}"
    ".a{color:red;color:red}";
  same "two writes split around an independent rule"
    ".a{color:red}.b{margin:0}.a{width:1px}"
    ".b{margin:0}.a{color:red;width:1px}"

(* On [class="a b"] each order ends at [flex] in an engine that reads it and at
   [-webkit-box] in one that does not: the later twin of each in the other's
   rule writes over both. *)
let a_later_twin_frees_a_pair () =
  same "two rules writing one fallback pair"
    ".a{display:-webkit-box;display:flex}.b{display:-webkit-box;display:flex}"
    ".b{display:-webkit-box;display:flex}.a{display:-webkit-box;display:flex}";
  apart "without the twin, the order decides"
    ".a{display:-webkit-box;display:flex}.b{display:-webkit-box}"
    ".b{display:-webkit-box}.a{display:-webkit-box;display:flex}"

(* An important declaration beats a normal one whatever their order (CSS Cascade
   5 sec. 6.1); two normal ones of one property do not. *)
let importance_frees_a_pair () =
  same "important against normal" ".a{color:red!important}.b{color:blue}"
    ".b{color:blue}.a{color:red!important}";
  apart "normal against normal" ".a{color:red}.b{color:blue}"
    ".b{color:blue}.a{color:red}"

(* A query and its negation never hold together (Media Queries 4 sec. 3.4), so
   no element sees both writes. *)
let exclusive_conditions_free_a_pair () =
  same "a query and its negation"
    "@media (min-width:1px){.a{color:red}}@media not \
     (min-width:1px){.a{color:blue}}"
    "@media not (min-width:1px){.a{color:blue}}@media \
     (min-width:1px){.a{color:red}}";
  apart "a query and another"
    "@media (min-width:1px){.a{color:red}}@media \
     (min-width:2px){.a{color:blue}}"
    "@media (min-width:2px){.a{color:blue}}@media \
     (min-width:1px){.a{color:red}}"

(* The interface prints consecutive rules under one prelude in one block. *)
let one_prelude_one_block () =
  Alcotest.(check int)
    "two print blocks become one" 1
    (List.length
       (Rule_trace.settle ~ctx
          (statements "@media print{.a{color:red}}@media print{.b{margin:0}}")))

(* A slot a later write of the group rewrites stops ordering the entry against
   another selector; a slot it keeps still does. On [class="a b"] the first pair
   ends at [margin-top:1px] and the second at [margin-left:2px] whatever the
   order of [.b]. *)
let a_rewritten_slot_frees_a_pair () =
  same "the rewritten slot" ".a{margin:0}.b{margin-top:5px}.a{margin-top:1px}"
    ".b{margin-top:5px}.a{margin:0}.a{margin-top:1px}";
  apart "a slot it keeps" ".a{margin:0}.b{margin-left:5px}.a{margin-top:1px}"
    ".b{margin-left:5px}.a{margin:0}.a{margin-top:1px}"

let settling_twice_changes_nothing () =
  List.iter
    (fun css ->
      let once = settled css in
      Alcotest.(check string) css once (settled once))
    [
      ".t{color:red;width:20px}.l{padding-top:0}.v{width:30px}.t{padding-top:10px}.l{color:blue}";
      "@media print{.a{color:red}}.a{color:green}@media print{.a{color:blue}}";
      ".a{display:-webkit-box;display:flex}.b{display:-webkit-box}.a{speak:none}";
    ]

let suite =
  ( "rule_trace",
    [
      Alcotest.test_case "empty run" `Quick empty_run;
      Alcotest.test_case "independent declarations sort" `Quick
        independent_declarations_sort;
      Alcotest.test_case "a covered longhand goes" `Quick
        a_covered_longhand_goes;
      Alcotest.test_case "rule boundaries are not read" `Quick
        rule_boundaries_are_not_read;
      Alcotest.test_case "a later twin frees a pair" `Quick
        a_later_twin_frees_a_pair;
      Alcotest.test_case "importance frees a pair" `Quick
        importance_frees_a_pair;
      Alcotest.test_case "exclusive conditions free a pair" `Quick
        exclusive_conditions_free_a_pair;
      Alcotest.test_case "one prelude, one block" `Quick one_prelude_one_block;
      Alcotest.test_case "a rewritten slot frees a pair" `Quick
        a_rewritten_slot_frees_a_pair;
      Alcotest.test_case "settling twice changes nothing" `Quick
        settling_twice_changes_nothing;
    ] )
