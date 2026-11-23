(* Generated-content, counter and quotes printers and readers. *)

open Common
open Values
open Properties_intf
open Prop_common
open Prop_image

let pp_counter_style_arg ctx = function
  | Option.None -> ()
  | Some style ->
      Pp.comma ctx ();
      Prop_list.pp_list_style_type ctx style

let rec pp_content : content Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Normal -> Pp.string ctx "normal"
  | String s -> Pp.quoted_string ctx s
  | Quoted { value; quote; repr } -> (
      if Pp.minified ctx then Pp.quoted_string ctx value
      else
        match repr with
        | Some repr -> Pp.string ctx repr
        | None -> Pp.quoted_string ~quote ctx value)
  | Image image -> pp_background_image ctx image
  | Open_quote -> Pp.string ctx "open-quote"
  | Close_quote -> Pp.string ctx "close-quote"
  | No_open_quote -> Pp.string ctx "no-open-quote"
  | No_close_quote -> Pp.string ctx "no-close-quote"
  | Attr attr -> Pp.call "attr" (Values.pp_attr_call pp_content) ctx attr
  | Counter { name; style } ->
      Pp.string ctx "counter(";
      pp_ident ctx name;
      pp_counter_style_arg ctx style;
      Pp.char ctx ')'
  | String_ref name -> Pp.call "string" pp_ident ctx name
  | Counters { name; separator; style } ->
      Pp.string ctx "counters(";
      pp_ident ctx name;
      Pp.comma ctx ();
      Pp.quoted_string ctx separator;
      pp_counter_style_arg ctx style;
      Pp.char ctx ')'
  | Content_list items -> Pp.list ~sep:Pp.space pp_content ctx items
  | With_alt { content; alt } ->
      pp_content ctx content;
      Pp.sp ctx ();
      Pp.char ctx '/';
      Pp.sp ctx ();
      Pp.list ~sep:Pp.space pp_content ctx alt
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_content ctx v

(* CSS Values 4 sec. 6.7.2 gives a string one serialisation, and [--minify]
   writes that one whichever quote the author chose: the quote and the authored
   spelling survive only in the round-trip the printer keeps for unminified
   output. Fold onto [String] once that round-trip is gone, or [content:'x'] and
   [content:"x"] reach one minified text through two nodes, which anything keyed
   on the node reads as two values. *)
let rec normalize_content ~lossless ~enforce_spec (c : content) : content =
  match c with
  | Quoted { value; _ } -> String value
  (* CSS Lists 3 sec. 4.6: an omitted counter style is [decimal]. *)
  | Counter { name; style = Some Decimal } -> Counter { name; style = None }
  | Counters { name; separator; style = Some Decimal } ->
      Counters { name; separator; style = None }
  | Image i ->
      let i' = normalize_background_image ~lossless ~enforce_spec i in
      if i' == i then c else Image i'
  | Content_list items ->
      let items' = List.map (normalize_content ~lossless ~enforce_spec) items in
      if List.equal ( == ) items items' then c else Content_list items'
  | With_alt { content; alt } ->
      let content' = normalize_content ~lossless ~enforce_spec content in
      let alt' = List.map (normalize_content ~lossless ~enforce_spec) alt in
      if content' == content && List.equal ( == ) alt alt' then c
      else With_alt { content = content'; alt = alt' }
  | _ -> c

let pp_counter_item ctx { name; reversed; value } =
  if reversed then Pp.call "reversed" pp_ident ctx name else pp_ident ctx name;
  match value with
  | None -> ()
  | Some n ->
      Pp.space ctx ();
      Values.pp_number ctx n

let rec pp_counter_set : counter_set Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Counters items -> Pp.list ~sep:Pp.space pp_counter_item ctx items
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_counter_set ctx v

let rec pp_quotes : quotes Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Pairs pairs ->
      List.iter
        (fun (open_q, close_q) ->
          Pp.quoted_string ctx open_q;
          Pp.quoted_string ctx close_q)
        pairs
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_quotes ctx v

let read_content_string t =
  match Cursor.string_repr_with_quote_opt t with
  | Some (value, quote, repr) -> Quoted { value; quote; repr }
  | None -> Cursor.err_expected t "string"

let read_counter_style_arg inner =
  Cursor.ws inner;
  if Cursor.comma_opt inner then (
    Cursor.ws inner;
    let style = Prop_list.read_counter_style inner in
    Cursor.ws inner;
    Some style)
  else None

let read_content_counter t =
  Cursor.call "counter" t (fun inner ->
      Cursor.ws inner;
      let name = Cursor.ident inner in
      let style = read_counter_style_arg inner in
      Cursor.expect_eof inner;
      (Counter { name; style } : content))

let read_content_string_ref t =
  Cursor.call "string" t (fun inner ->
      Cursor.ws inner;
      let name = Cursor.ident inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      String_ref name)

let read_content_counters t =
  Cursor.call "counters" t (fun inner ->
      Cursor.ws inner;
      let name = Cursor.ident inner in
      Cursor.ws inner;
      Cursor.comma inner;
      Cursor.ws inner;
      let separator = Cursor.string inner in
      let style = read_counter_style_arg inner in
      Cursor.expect_eof inner;
      (Counters { name; separator; style } : content))

let rec read_content_attr t =
  Cursor.call "attr" t (fun inner ->
      Cursor.ws inner;
      let name = Cursor.ident ~keep_case:true inner in
      let type_ : Values.attr_type option =
        Cursor.ws inner;
        if Cursor.is_done inner || Cursor.peek_comma inner then None
        else Some (Values.read_attr_type inner)
      in
      Cursor.ws inner;
      let fallback : content Values.attr_fallback =
        if Cursor.comma_opt inner then (
          Cursor.ws inner;
          if Cursor.is_done inner then Empty_fallback
          else Attr_fallback (read_content_items inner))
        else No_fallback
      in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      Attr { name; type_; fallback })

and read_content_single t =
  let read_var t : content = Var (read_var read_content t) in
  Cursor.enum_or_calls "content"
    [
      ("none", (None : content));
      ("normal", Normal);
      ("open-quote", Open_quote);
      ("close-quote", Close_quote);
      ("no-open-quote", No_open_quote);
      ("no-close-quote", No_close_quote);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:
      [
        ("var", read_var);
        ("attr", read_content_attr);
        ("counter", read_content_counter);
        ("string", read_content_string_ref);
        ("counters", read_content_counters);
      ]
    ~default:
      (Cursor.one_of
         [ read_content_string; (fun t -> (Image (read_bg_image t) : content)) ])
    t

(* CSS Generated Content 3 sec. 1.1: the alternative text is [[ <string> |
   <counter> | attr() ]+]. *)
and read_content_alt_item t : content =
  Cursor.enum_or_calls "content alternative text" []
    ~calls:
      [
        ("var", fun t -> (Var (read_var read_content t) : content));
        ("attr", read_content_attr);
        ("counter", read_content_counter);
        ("counters", read_content_counters);
      ]
    ~default:read_content_string t

and read_content t : content =
  let content = read_content_items t in
  Cursor.ws t;
  if Cursor.slash_opt t then (
    (match content with
    | None | Normal ->
        Cursor.err_invalid t "none and normal take no alternative text"
    | _ -> ());
    Cursor.ws t;
    let alt = Cursor.list ~sep:Cursor.ws ~at_least:1 read_content_alt_item t in
    With_alt { content; alt })
  else content

and read_content_items t : content =
  let items = Cursor.list ~sep:Cursor.ws ~at_least:1 read_content_single t in
  match items with
  | [ item ] -> item
  | _ ->
      if
        List.exists
          (fun (item : content) ->
            match item with None | Normal -> true | _ -> false)
          items
      then Cursor.err_invalid t "none/normal cannot be combined in content";
      Content_list items

(* CSS Lists 3 sec. 2.1 spells a counter name [<custom-ident>] and excludes
   [none] from it. *)
let read_counter_name t =
  Cursor.custom_ident ~reserved:[ "none" ] "counter name" t

(* CSS Lists 3 sec. 4.1: [counter-reset] takes a [reversed( <counter-name> )]
   item, and sec. 4.2 gives [counter-increment] none. *)
let read_counter_item ~reversed t =
  let reversed_name =
    if reversed then
      Cursor.function_call "reversed"
        (fun t ->
          Cursor.ws t;
          let name = read_counter_name t in
          Cursor.ws t;
          name)
        t
    else None
  in
  let name, reversed =
    match reversed_name with
    | Some name -> (name, true)
    | None -> (read_counter_name t, false)
  in
  Cursor.ws t;
  let value = Cursor.option Values.read_integer_slot t in
  { name; reversed; value }

let rec read_counter_set ~reversed t : counter_set =
  Cursor.enum_or_var "counter"
    [
      ("none", (None : counter_set));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var (read_counter_set ~reversed) t))
    ~default:(fun t ->
      let items =
        Cursor.list ~sep:Cursor.ws ~at_least:1 (read_counter_item ~reversed) t
      in
      Counters items)
    t

let rec read_quotes t : quotes =
  let read_var' t : quotes = Var (read_var read_quotes t) in
  (* Read pairs of strings for quotes property *)
  let read_pairs t =
    let rec read_quotes_pairs acc =
      Cursor.ws t;
      match Cursor.string_opt t with
      | Some open_q ->
          Cursor.ws t;
          let close_q = Cursor.string t in
          read_quotes_pairs ((open_q, close_q) :: acc)
      | None -> List.rev acc
    in
    Pairs (read_quotes_pairs [])
  in
  Cursor.enum_or_calls "quotes"
    [
      ("auto", (Auto : quotes));
      ("none", None);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert-layer", Revert_layer);
      ("revert", Revert);
    ]
    ~calls:[ ("var", read_var') ]
    ~default:read_pairs t
