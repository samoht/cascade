type row = {
  property : string;
  positives : string list;
  negatives : string list;
}

(* Filter Effects 1 section 6.1: drop-shadow alone requires arguments. *)
let omitted_filters =
  [
    "blur()";
    "brightness()";
    "contrast()";
    "grayscale()";
    "hue-rotate()";
    "invert()";
    "opacity()";
    "saturate()";
    "sepia()";
  ]

(* Filter Effects 1 section 6.1 and CSS Values 4 section 10: only lengths;
   calculated negative results clamp instead of becoming invalid literals. *)
let blur_lengths =
  [ "blur(calc(-1px))"; "blur(min(-1px, 2px))"; "blur(max(0px, 1em))" ]

let invalid_blur_lengths =
  [
    "blur(0%)";
    "blur(10%)";
    "blur(-1px)";
    "blur(auto)";
    "blur(calc(0%))";
    "blur(calc(1px + 0%))";
    "blur(calc(10% - 10%))";
    "blur(min(1px, 2%))";
    "blur(min(0, 1px))";
    "blur(clamp(0px, 1%, 2px))";
    "blur(minmax(1px, 2px))";
    "blur(fit-content(1px))";
  ]

let rows_for properties positives negatives =
  List.map (fun property -> { property; positives; negatives }) properties
