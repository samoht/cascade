(* Which productions the browser in front of these harnesses implements.

   The tables of hand-written entries this module used to carry are gone. They
   answered a question Cascade.Support answers from the web-features dataset,
   and answered it in prose that nothing could invalidate: an entry stayed true
   until a person reread it, where a lookup goes stale the day the browser ships
   the grammar. What is left is the derivation of the BCD compat key from the
   property and the value, which is the only part a dataset lookup cannot do for
   itself. *)

let unimplemented =
  [
    "caret";
    "nav-up";
    "nav-down";
    "nav-left";
    "nav-right";
    "initial-letter-align";
    "initial-letter-wrap";
    "inline-sizing";
    "line-fit-edge";
    "line-height-step";
    "margin-trim";
    "mask-border";
    "min-intrinsic-sizing";
    "ruby-merge";
    "text-decoration-skip";
    "text-decoration-skip-box";
    "text-decoration-skip-inset";
    "text-decoration-skip-self";
    "text-decoration-skip-spaces";
    "text-emphasis-skip";
    "glyph-orientation-vertical";
    "image-resolution";
    "font-synthesis-position";
    "-moz-appearance";
    "-moz-osx-font-smoothing";
    "-ms-filter";
    "-o-transition";
    "-o-transform";
    (* Chrome has never carried Gecko's prefixed names, and dropped the
       Microsoft ones with the Trident engine. *)
    "-moz-animation";
    "-moz-animation-delay";
    "-moz-animation-direction";
    "-moz-animation-duration";
    "-moz-animation-fill-mode";
    "-moz-animation-iteration-count";
    "-moz-animation-name";
    "-moz-animation-play-state";
    "-moz-animation-timing-function";
    "-moz-border-radius";
    "-moz-box-shadow";
    "-moz-box-sizing";
    "-moz-orient";
    "-moz-transform";
    "-moz-transition";
    "-moz-transition-delay";
    "-moz-transition-duration";
    "-moz-transition-property";
    "-moz-transition-timing-function";
    "-moz-user-select";
    "-ms-transform";
    "-ms-user-select";
    (* A descriptor of @font-face, not a property: setProperty and CSS.supports
       both take a property name, so neither can be asked about it. *)
    "src";
    "-webkit-backdrop-filter";
    "-webkit-hyphens";
    "-webkit-mask-source-type";
    "-webkit-text-decoration";
    "-webkit-text-decoration-color";
  ]

let unimplemented_property name = List.exists (String.equal name) unimplemented

(* ===== Naming a production the way BCD names it ===== *)

let split_ws s =
  List.filter
    (fun w -> not (String.equal w ""))
    (String.split_on_char ' ' (String.trim s))

let underscored s = String.map (function '-' -> '_' | c -> c) s

let is_quoted p =
  String.length p >= 2
  && (Char.equal p.[0] '"' || Char.equal p.[0] '\'')
  && Char.equal p.[String.length p - 1] p.[0]

(* The function name of [f(...)], when the value is one call. *)
let call_name s =
  let s = String.trim s in
  match String.index_opt s '(' with
  | Some i when i > 0 && String.length s > i && s.[String.length s - 1] = ')' ->
      let name = String.sub s 0 i in
      if String.for_all (fun c -> (c >= 'a' && c <= 'z') || c = '-') name then
        Some name
      else None
  | Some _ | None -> None

(* BCD writes a keyword arm as css.properties.<property>.<value>, some of them
   with [_] where CSS writes [-]; a multi-slot value under the slot that carries
   it, so each word is a candidate; a <string> arm as [.string]; and a value
   type under css.types. Every spelling is a candidate and the dataset decides
   which one exists, so a production BCD names in a way this does not reach is a
   measurement in the library rather than a table entry here. *)
(* A vendor-prefixed property is the same production under another name, and
   BCD files it under the unprefixed one. *)
let unprefixed property =
  List.find_map
    (fun prefix ->
      if String.starts_with ~prefix property then
        Some
          (String.sub property (String.length prefix)
             (String.length property - String.length prefix))
      else None)
    [ "-webkit-"; "-moz-"; "-ms-"; "-o-" ]

let keys_for ?(prefix = "css.properties") ~property ~value () =
  let value = String.trim value in
  let properties = property :: Option.to_list (unprefixed property) in
  let prop k =
    List.map (fun p -> String.concat "" [ prefix; "."; p; "."; k ]) properties
  in
  let words = split_ws value in
  let whole = prop value @ prop (underscored value) in
  let joined =
    match words with
    | [] | [ _ ] -> []
    | ws -> prop (String.concat "_" (List.map underscored ws))
  in
  (* BCD names an arm a property takes only in its multi-value form after the
     shape rather than the value: two slots are [two_value_syntax]. *)
  let arity =
    match words with [ _; _ ] -> prop "two_value_syntax" | _ -> []
  in
  let per_word =
    match words with
    | [] | [ _ ] -> []
    | ws -> List.concat_map (fun w -> prop w @ prop (underscored w)) ws
  in
  (* A value that is one entry of a comma-separated list is the same keyword
     doing a different job, and BCD names the job. *)
  let in_a_list =
    if String.contains value ',' then
      List.concat_map
        (fun entry ->
          List.concat_map
            (fun w -> prop (String.concat "" [ underscored w; "_in_a_list" ]))
            (split_ws entry))
        (String.split_on_char ',' value)
    else []
  in
  let strings = if is_quoted value then prop "string" else [] in
  (* Productions web-features does not model still have to be named, and a name
     nothing derives is a name only a table can reach. These are the shapes a
     value has rather than the value itself, so they are one name each however
     the generator spells the member it drew. *)
  let is_number w =
    w <> ""
    && String.for_all
         (fun c -> (c >= '0' && c <= '9') || c = '.' || c = '-' || c = '+')
         w
    && String.exists (fun c -> c >= '0' && c <= '9') w
  in
  let shapes =
    (match words with [ w ] when is_number w -> prop "number" | _ -> [])
    @ (if String.length value > 0 && value.[String.length value - 1] = ',' then
         prop "trailing_comma"
       else [])
    @ (if String.contains value ',' then prop "comma" else [])
    @
    (* A negative of any dimension, not only a bare number: the sign is what the
       range excludes, whatever unit follows it. *)
    match words with
    | [ w ]
      when String.length w > 1 && w.[0] = '-' && w.[1] >= '0' && w.[1] <= '9' ->
        prop "negative"
    | _ -> []
  in
  (* A function is filed under the property BCD happened to pick, and named the
     same way wherever that is, so the whole dataset is searched by name. *)
  let calls =
    match call_name value with
    | None -> []
    | Some name ->
        prop (String.concat "" [ underscored name; "_function" ])
        @ [
            String.concat "" [ "css.types."; name ];
            String.concat "" [ "css.types.image."; name ];
            String.concat "" [ "css.types.color."; name ];
          ]
        @ Cascade.Support.keys_named (String.concat "" [ name; "_function" ])
        @ Cascade.Support.keys_named
            (String.concat "" [ underscored name; "_function" ])
  in
  (* BCD names a slot the shorthand accepts [<longhand>_included], and which
     slot a value fills is grammar this harness does not have, so every slot the
     dataset records for the property is a candidate. *)
  let included =
    List.concat_map
      (fun p ->
        List.filter
          (fun k -> String.ends_with ~suffix:"_included" k)
          (Cascade.Support.keys_under
             (String.concat "" [ prefix; "."; p; "." ])))
      properties
  in
  let bare =
    List.map (fun p -> String.concat "" [ prefix; "."; p ]) properties
  in
  List.fold_left
    (fun acc k -> if List.exists (String.equal k) acc then acc else acc @ [ k ])
    []
    (whole @ joined @ in_a_list @ arity @ strings @ shapes @ calls @ per_word
   @ included @ bare)

(* ===== What the dataset says about a disagreement ===== *)

type explanation = Not_shipped of string | Shipped_beyond_spec of string

let explanation_key = function
  | Not_shipped key | Shipped_beyond_spec key -> key

let explains_rejection ?prefix ~chrome ~property ~value () =
  List.find_map
    (fun key ->
      match
        Cascade.Support.engine_implements Cascade.Support.Chrome chrome key
      with
      | Some false -> Some (Not_shipped key)
      | Some true | None -> None)
    (keys_for ?prefix ~property ~value ())

(* Only a measured key answers here: the generated table records what browsers
   ship, which cannot say whether a specification grants it. That judgement is a
   measurement, and it lives in the library beside the specification text that
   decided it. *)
let explains_acceptance ?prefix ~chrome ~property ~value () =
  List.find_map
    (fun key ->
      if
        Cascade.Support.self_measured key
        && Cascade.Support.engine_implements Cascade.Support.Chrome chrome key
           = Some true
      then Some (Shipped_beyond_spec key)
      else None)
    (keys_for ?prefix ~property ~value ())

(* ===== Reporting which side is wrong ===== *)

type verdict = Browser_behind | Cascade_wrong | Needs_measurement

let verdict_of targets key =
  match Cascade.Support.implemented targets key with
  | None -> Needs_measurement
  | Some true -> Cascade_wrong
  | Some false -> Browser_behind

let verdict_name = function
  | Browser_behind -> "cascade right, the browser has not shipped it"
  | Cascade_wrong -> "every target ships it, so cascade is wrong"
  | Needs_measurement -> "not modelled, needs measuring"
