type t = node list

and node =
  | Element of element
  | Text of string
  | Comment of string
  | Doctype of Markup.doctype

and element = {
  ns : string;
  tag : string;
  mutable attributes : (string * string) list;
  mutable children : node list;
  mutable parent : element option;
}

(* markup.ml reports an attribute namespace as the prefix the source wrote
   ([("x-on", "click")] for [x-on:click]), and its HTML writer only knows how to
   put back the four it has URIs for. Joining the two here keeps every other
   prefix, which is most of what a page carries: Alpine, Vue, Angular. *)
let attribute_name (prefix, local) =
  if prefix = "" then local else String.concat ":" [ prefix; local ]

let parse html =
  Markup.string html |> Markup.parse_html |> Markup.signals
  |> Markup.trees
       ~text:(fun ss -> Text (String.concat "" ss))
       ~comment:(fun s -> Comment s)
       ~doctype:(fun d -> Doctype d)
       ~element:(fun (ns, tag) attributes children ->
         let attributes =
           List.map (fun (name, v) -> (attribute_name name, v)) attributes
         in
         let e = { ns; tag; attributes; children; parent = None } in
         List.iter
           (function Element c -> c.parent <- Some e | _ -> ())
           children;
         Element e)
  |> Markup.to_list

let to_string nodes =
  let signal = function
    | Element { ns; tag; attributes; children; _ } ->
        let attributes = List.map (fun (n, v) -> (("", n), v)) attributes in
        `Element ((ns, tag), attributes, children)
    | Text s -> `Text s
    | Comment s -> `Comment s
    | Doctype d -> `Doctype d
  in
  List.concat_map (fun n -> Markup.from_tree signal n |> Markup.to_list) nodes
  |> Markup.of_list |> Markup.write_html |> Markup.to_string

let pp ppf doc = Fmt.string ppf (to_string doc)

let roots nodes =
  List.filter_map (function Element e -> Some e | _ -> None) nodes

let element_children e = roots e.children

let rec find_all tag nodes =
  List.concat_map
    (function
      | Element e ->
          let here = if e.tag = tag then [ e ] else [] in
          here @ find_all tag e.children
      | _ -> [])
    nodes

let find tag nodes =
  match find_all tag nodes with [] -> None | e :: _ -> Some e

let rec text e =
  String.concat ""
    (List.map
       (function Element c -> text c | Text s -> s | _ -> "")
       e.children)

let attribute e name = List.assoc_opt name e.attributes

(* Setting an attribute puts it at the head of the list, so a page's own
   attribute order is what survives and the [style] this command writes reads
   first. *)
let set_attribute e name value =
  let others = List.filter (fun (n, _) -> n <> name) e.attributes in
  e.attributes <- (name, value) :: others

let is_ascii_whitespace = function
  | ' ' | '\t' | '\n' | '\012' | '\r' -> true
  | _ -> false

let classes e =
  match attribute e "class" with
  | None -> []
  | Some v ->
      String.split_on_char ' '
        (String.map (fun c -> if is_ascii_whitespace c then ' ' else c) v)
      |> List.filter (fun s -> s <> "")

let text_children e =
  List.filter_map (function Text s -> Some s | _ -> None) e.children

let clear e = e.children <- []

let element tag ~text =
  {
    ns = Markup.Ns.html;
    tag;
    attributes = [];
    children = (if text = "" then [] else [ Text text ]);
    parent = None;
  }

let append_child parent child =
  child.parent <- Some parent;
  parent.children <- parent.children @ [ Element child ]

let prepend_child parent child =
  child.parent <- Some parent;
  parent.children <- Element child :: parent.children
