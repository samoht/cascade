type version = int * int

type targets = {
  chrome : version;
  firefox : version;
  safari : version;
  ios_safari : version;
}

let evergreen =
  {
    chrome = (111, 0);
    firefox = (128, 0);
    safari = (16, 4);
    ios_safari = (16, 4);
  }

type measurement = {
  key : string;
  support : Baseline.support;
  why : string;
  measured : string;
}

(* Measured by hand where web-features records no key. Each is a production a
   specification grants and a browser has not shipped, so cascade reading it is
   right and the browser's answer says nothing about the value.

   This is library knowledge rather than a test fixture: it is a fact about CSS
   in the world, which is what cascade models. A harness carrying it would be
   excusing its own failures; one that asks {!implemented} is reading what the
   library knows.

   The bar for an entry is the bar for a generated row: the specification
   section that grants the grammar, and the build the disagreement was measured
   on. A key the dataset later carries should be deleted from here rather than
   left to drift. *)
let measured =
  [
    {
      key = "css.properties.background-blend-mode.plus-lighter";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "Compositing 2 sec. 3.4.3 spells background-blend-mode \
         <'mix-blend-mode'>#, and sec. 3.4.1 gives mix-blend-mode <blend-mode> \
         | plus-lighter, so the value is granted on both. web-features records \
         the mix-blend-mode key alone";
      measured = "Chrome 153";
    };
    {
      key = "css.properties.transition.none_in_a_list";
      support =
        {
          Baseline.chrome = None;
          firefox = None;
          safari = None;
          safari_ios = None;
        };
      why =
        "CSS Transitions 1 sec. 2.4 spells the shorthand <single-transition>#, \
         and sec. 2.3 gives <single-transition> a [ none | \
         <single-transition-property> ], so none is one entry of the list. \
         Chrome reads it alone and refuses every list holding one";
      measured = "Chrome 153";
    };
  ]

let measured_table =
  lazy
    (let t = Hashtbl.create (List.length measured) in
     List.iter (fun m -> Hashtbl.replace t m.key m.support) measured;
     t)

let table =
  lazy
    (let table = Hashtbl.create (List.length Baseline.support) in
     List.iter
       (fun (key, support) -> Hashtbl.replace table key support)
       Baseline.support;
     table)

let at_least (major, minor) (target_major, target_minor) =
  target_major > major || (target_major = major && target_minor >= minor)

(* An engine answers yes when the dataset names the version that shipped the key
   and the target is at or past it. A [None] shipped version is an engine that
   does not implement the key at all, which no target version satisfies. *)
let engine_has shipped target =
  match shipped with None -> false | Some shipped -> at_least shipped target

(* The generated table answers first; [measured] is the supplement for keys
   web-features gives none, so a fact leaves here of its own accord when the
   dataset catches up. *)
let implemented targets key =
  match
    match Hashtbl.find_opt (Lazy.force table) key with
    | Some s -> Some s
    | None -> Hashtbl.find_opt (Lazy.force measured_table) key
  with
  | None -> None
  | Some (support : Baseline.support) ->
      Some
        (engine_has support.chrome targets.chrome
        && engine_has support.firefox targets.firefox
        && engine_has support.safari targets.safari
        && engine_has support.safari_ios targets.ios_safari)

type engine = Chrome | Firefox | Safari | Ios_safari

let engine_implements engine version key =
  match Hashtbl.find_opt (Lazy.force table) key with
  | None -> None
  | Some (support : Baseline.support) ->
      let shipped =
        match engine with
        | Chrome -> support.chrome
        | Firefox -> support.firefox
        | Safari -> support.safari
        | Ios_safari -> support.safari_ios
      in
      Some (engine_has shipped version)

let unimplemented_by targets key =
  match implemented targets key with
  | Some false -> true
  | Some true | None -> false
