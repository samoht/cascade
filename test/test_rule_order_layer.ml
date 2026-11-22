(* CSS Cascade 5 sec. 6.4.3: cascade layers are sorted by the order in which
   they are first declared. Each pass here must keep that order, and only that
   order decides what it may drop or move. *)

open Cascade

let statements css =
  match Css.of_string ~strict:false css with
  | Ok { stylesheet; _ } -> Css.statements stylesheet
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let render stmts = Pp.to_string ~minify:true Stylesheet.pp_stylesheet stmts
let through f css = render (f (statements css))

let fold_drops_the_pin_a_block_repeats () =
  Alcotest.(check string)
    "the block declares what the pin did" "@layer a{x{color:red}}"
    (through Rule_order_layer.fold_layer_pins "@layer a;@layer a{x{color:red}}")

let fold_keeps_the_pin_that_orders () =
  (* [b]'s block comes first, so only the first pin puts [a] before it; the
     second pin then repeats what [b]'s block declares. *)
  Alcotest.(check string)
    "the ordering pin stays, the repeat goes"
    "@layer a;@layer b{x{color:red}}@layer a{y{color:blue}}"
    (through Rule_order_layer.fold_layer_pins
       "@layer a;@layer b;@layer b{x{color:red}}@layer a{y{color:blue}}")

let sort_puts_blocks_in_rank_order () =
  (* Once the blocks stand in the order the pin gives, the pin declares nothing
     they do not. *)
  Alcotest.(check string)
    "blocks follow the pinned order"
    "@layer a{y{color:blue}}@layer b{x{color:red}}"
    (through Rule_order_layer.sort_layer_blocks
       "@layer a,b;@layer b{x{color:red}}@layer a{y{color:blue}}")

let declarations_state_the_order_once () =
  Alcotest.(check string)
    "one statement names the run's order"
    "@layer a,b;@layer a{y{color:blue}}@layer b{x{color:red}}"
    (through Rule_order_layer.canonical_layer_declarations
       "@layer a;@layer b;@layer b{x{color:red}}@layer a{y{color:blue}}")

let suite =
  ( "rule_order_layer",
    [
      Alcotest.test_case "fold drops the pin a block repeats" `Quick
        fold_drops_the_pin_a_block_repeats;
      Alcotest.test_case "fold keeps the pin that orders" `Quick
        fold_keeps_the_pin_that_orders;
      Alcotest.test_case "sort puts blocks in rank order" `Quick
        sort_puts_blocks_in_rank_order;
      Alcotest.test_case "declarations state the order once" `Quick
        declarations_state_the_order_once;
    ] )
