(* Generated stylesheets for one property, drawn from Value_gen.

   Value_gen writes one declaration at a time, so nothing it draws reaches a
   pass that decides about a declaration by looking at another one. This draws
   those shapes: a second write of the same property, a write of a property
   sharing its grammar, the value behind a custom property, two rules, and the
   conditional groups whose contents the optimizer treats as one run.

   A generated sheet carries the simpler sheets it is built from, so an oracle
   that already reported a part does not report the composite as a second
   finding. *)

type sheet = { css : string; parts : string list }

(* ===== A seeded stream ===== *)

(* Its own stream rather than Value_gen's, keyed on the property and a salt, so
   adding a shape here does not move the values Value_gen draws. *)

let mask = 0x3FFFFFFF

let seed_of ~seed name =
  let h = ref (((seed * 0x27220A95) + 0x165667B1) land mask) in
  String.iter (fun c -> h := ((!h * 33) + Char.code c) land mask) name;
  !h

type stream = { mutable state : int }

let stream n = { state = n land mask lor 1 }

let next s =
  s.state <- ((s.state * 1103515245) + 12345) land mask;
  s.state

let sample s n values =
  let a = Array.of_list values in
  let len = Array.length a in
  for i = len - 1 downto 1 do
    let j = next s mod (i + 1) in
    let t = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- t
  done;
  Array.to_list (Array.sub a 0 (min n len))

(* ===== Which properties are worth writing together ===== *)

(* CSS names a longhand by extending its shorthand with a hyphen -
   [margin]/[margin-top], [transition]/[transition-duration] - and names a
   vendor's early spelling of a property by prefixing it. Two properties in
   either relation can write a common cascade slot, which is what the
   contraction, elimination and comparison passes decide about.

   Read from the manifest and from the naming conventions, never from the
   library: a population the library computes is a population the library can
   shrink by being wrong about it. A pair no reader models is a sheet the reader
   rejects, and a rejected sheet is not this harness's question. *)

let names = Property_grammar.property_names
let prefixes = [ "-webkit-"; "-moz-"; "-ms-"; "-o-" ]

let extends short long =
  let n = String.length short in
  String.length long > n + 1
  && String.equal (String.sub long 0 n) short
  && Char.equal long.[n] '-'

let related =
  let table = Hashtbl.create 512 in
  let add a b =
    let previous = try Hashtbl.find table a with Not_found -> [] in
    Hashtbl.replace table a (b :: previous)
  in
  List.iter
    (fun a ->
      List.iter
        (fun b ->
          if extends a b then (
            add a b;
            add b a))
        names)
    names;
  table

let partners name =
  let family = try Hashtbl.find related name with Not_found -> [] in
  let prefixed = List.map (fun p -> String.concat "" [ p; name ]) prefixes in
  List.sort_uniq String.compare (List.rev_append prefixed family)

(* ===== Writing a sheet ===== *)

let decl property value = String.concat "" [ property; ":"; value ]

let rule declarations =
  String.concat "" [ "a{"; String.concat ";" declarations; "}" ]

let plain property value = rule [ decl property value ]

(* The conditional groups whose contents the optimizer treats as one run. A
   wrapper whose body is a declaration list rather than a rule - [@font-face],
   [@keyframes], [@page] - belongs to the descriptor oracles, which ask a
   browser about a descriptor set rather than about a stylesheet. *)
let wrappers =
  [
    (fun body -> String.concat "" [ "@media screen{"; body; "}" ]);
    (fun body -> String.concat "" [ "@supports (color:red){"; body; "}" ]);
    (fun body -> String.concat "" [ "@layer l{"; body; "}" ]);
    (fun body -> String.concat "" [ "@container (width>0px){"; body; "}" ]);
  ]

(* One rule holding both writes in either order, the second write made
   important, the two writes split across rules, and the second write nested
   under the first. Each reaches the passes through a different reader or a
   different graph. *)
let shapes ~a ~b =
  [
    rule [ a; b ];
    rule [ b; a ];
    rule [ a; String.concat "" [ b; "!important" ] ];
    String.concat "" [ "a{"; a; "}b{"; b; "}" ];
    String.concat "" [ "a{"; a; ";&:hover{"; b; "}}" ];
  ]

(* ===== How much of each shape ===== *)

(* Each count multiplies the property inventory, and a pair shape multiplies it
   twice over, so they are held to what keeps the sweep inside half a minute. *)
let own_count = 4
let respelling_count = 5
let partner_count = 4
let partner_value_count = 2
let custom_count = 8
let wrapped_count = 2

(* A value and a spelling of that same value. CSS Syntax 3 sec. 4 and CSS Values
   4 sec. 4.1 make the two one value, so a pass that decides two declarations
   are the same write has to see through the spelling. Nothing reaches that
   question with one declaration, and no pair drawn independently reaches it
   often: the two draws have to land on one value. *)
let respellings ~seed name =
  List.filter_map
    (fun (v : Value_gen.vector) ->
      match v.respells with
      | None -> None
      | Some origin -> Some (origin, v.value))
    (Value_gen.vectors_for ~seed name)

let sheets_for ~seed name =
  let s = stream (seed_of ~seed name) in
  let values = Value_gen.values_for ~seed name in
  let singles = List.map (fun v -> { css = plain name v; parts = [] }) values in
  let own = sample s own_count values in
  let pair (p, a) (q, b) =
    List.map
      (fun css -> { css; parts = [ plain p a; plain q b ] })
      (shapes ~a:(decl p a) ~b:(decl q b))
  in
  (* The same property written twice, and written twice with one value spelled
     two ways. *)
  let repeated =
    List.concat_map
      (fun a -> List.concat_map (fun b -> pair (name, a) (name, b)) own)
      own
    @ List.concat_map
        (fun (origin, respelling) ->
          pair (name, origin) (name, respelling)
          @ pair (name, respelling) (name, origin))
        (sample s respelling_count (respellings ~seed name))
  in
  (* A property whose name says it shares a grammar, or spells this one for a
     vendor. Both directions of the respelling pair again: the shorthand written
     one way and the longhand the other is the shape a contraction pass that
     compares spellings gets wrong. *)
  let crossed =
    List.concat_map
      (fun partner ->
        let theirs =
          sample s partner_value_count (Value_gen.values_for ~seed partner)
        in
        List.concat_map
          (fun a ->
            List.concat_map (fun b -> pair (name, a) (partner, b)) theirs)
          own
        @ List.concat_map
            (fun (origin, respelling) ->
              pair (name, origin) (partner, respelling)
              @ pair (partner, origin) (name, respelling))
            (sample s respelling_count (respellings ~seed partner)))
      (sample s partner_count (partners name))
  in
  (* The value reached through a custom property. Both halves are parts: what
     the reader sees at this property is [var(--x)], and what it sees at the
     custom property is a token stream no grammar reads. *)
  let define v = String.concat "" [ "a{--x:"; v; "}" ] in
  let bound = sample s custom_count values in
  let custom = List.map (fun v -> { css = define v; parts = [] }) bound in
  let substituted =
    List.concat_map
      (fun v ->
        let reference = plain name "var(--x)" in
        List.map
          (fun css -> { css; parts = [ reference; define v ] })
          [
            String.concat "" [ "a{--x:"; v; ";"; decl name "var(--x)"; "}" ];
            String.concat "" [ "a{"; decl name "var(--x)"; ";--x:"; v; "}" ];
            String.concat ""
              [
                "@property --x{syntax:\"*\";inherits:false}a{--x:";
                v;
                ";";
                decl name "var(--x)";
                "}";
              ];
          ])
      bound
  in
  (* The pair shapes again inside each conditional group. The passes that act on
     a run of rules run per group, and a group is not the top level. *)
  let wrapped =
    List.concat_map
      (fun (sheet : sheet) ->
        List.map
          (fun wrap ->
            { css = wrap sheet.css; parts = sheet.css :: sheet.parts })
          wrappers)
      (sample s wrapped_count (repeated @ crossed))
  in
  List.concat [ singles; custom; repeated; crossed; substituted; wrapped ]
