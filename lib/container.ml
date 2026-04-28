(** CSS container query condition types *)

type t =
  | Min_width_rem of float
  | Min_width_px of int
  | Named of string * t
  | Custom of Media.t
  | Raw of string

(* Format float without trailing period (24. -> 24, 24.5 -> 24.5) *)
let format_rem f =
  let s = string_of_float f in
  if String.ends_with ~suffix:"." s then String.sub s 0 (String.length s - 1)
  else s

let rec to_string = function
  | Min_width_rem rem -> "(min-width:" ^ format_rem rem ^ "rem)"
  | Min_width_px px -> "(min-width:" ^ Int.to_string px ^ "px)"
  | Named (name, cond) -> name ^ " " ^ to_string cond
  | Custom cond -> Media.to_string cond
  | Raw s -> s

let pp = to_string

let rec compare t1 t2 =
  match (t1, t2) with
  | Min_width_rem r1, Min_width_rem r2 -> Float.compare r1 r2
  | Min_width_px p1, Min_width_px p2 -> Int.compare p1 p2
  | Named (n1, c1), Named (n2, c2) ->
      let name_cmp = String.compare n1 n2 in
      if name_cmp <> 0 then name_cmp else compare c1 c2
  | Custom c1, Custom c2 -> Media.compare c1 c2
  | Raw s1, Raw s2 -> String.compare s1 s2
  (* Order: Min_width_rem < Min_width_px < Named < Custom *)
  | Min_width_rem _, _ -> -1
  | _, Min_width_rem _ -> 1
  | Min_width_px _, _ -> -1
  | _, Min_width_px _ -> 1
  | Named _, (Custom _ | Raw _) -> -1
  | (Custom _ | Raw _), Named _ -> 1
  | Custom _, Raw _ -> -1
  | Raw _, Custom _ -> 1

type kind = Kind_min_width | Kind_other

let rec kind = function
  | Min_width_rem _ | Min_width_px _ -> Kind_min_width
  | Named (_, cond) -> kind cond
  | Custom _ | Raw _ -> Kind_other

let of_string s =
  let is_ident_start c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || c = '-'
  in
  let is_ident_cont c = is_ident_start c || (c >= '0' && c <= '9') in
  let split_named s =
    let s = String.trim s in
    let len = String.length s in
    let rec ident_end i =
      if i < len && is_ident_cont s.[i] then ident_end (i + 1) else i
    in
    if len = 0 || not (is_ident_start s.[0]) then None
    else
      let stop = ident_end 0 in
      let rec first_non_ws i =
        if i >= len then None
        else
          match s.[i] with
          | ' ' | '\t' | '\n' | '\r' | '\012' -> first_non_ws (i + 1)
          | c -> Some (i, c)
      in
      match first_non_ws stop with
      | Some (i, ('(' | 's')) when stop > 0 && i > stop ->
          Some (String.sub s 0 stop, String.sub s i (len - i))
      | _ -> None
  in
  let balanced s =
    let len = String.length s in
    let rec loop depth i =
      if i >= len then depth = 0
      else
        match s.[i] with
        | '(' -> loop (depth + 1) (i + 1)
        | ')' -> depth > 0 && loop (depth - 1) (i + 1)
        | _ -> loop depth (i + 1)
    in
    loop 0 0
  in
  let starts_with ~prefix s =
    let n = String.length prefix in
    String.length s >= n && String.sub s 0 n = prefix
  in
  let validate_raw raw =
    let raw = String.trim raw in
    if raw = "" then failwith "empty container query";
    if not (balanced raw) then failwith "unmatched container query parentheses";
    if starts_with ~prefix:"style(" raw then (
      if not (String.ends_with ~suffix:")" raw) then
        failwith "unmatched style() container query";
      let body = String.sub raw 6 (String.length raw - 7) |> String.trim in
      if body = "" then failwith "empty style() container query";
      if String.contains body ':' then
        let parts = String.split_on_char ':' body in
        match parts with
        | [ name; value ] when String.trim name <> "" && String.trim value <> ""
          ->
            ()
        | _ -> failwith "invalid style() container query"
      else if body = "color" || body = "stuck" || body = "snapped" then
        failwith "invalid style() container query")
    else if starts_with ~prefix:"scroll-state(" raw then (
      if not (String.ends_with ~suffix:")" raw) then
        failwith "unmatched scroll-state() container query";
      let body = String.sub raw 13 (String.length raw - 14) |> String.trim in
      match String.split_on_char ':' body with
      | [ name; value ] -> (
          match (String.trim name, String.trim value) with
          | "stuck", ("top" | "right" | "bottom" | "left")
          | "snapped", ("block" | "inline" | "both") ->
              ()
          | _ -> failwith "invalid scroll-state() container query")
      | _ -> failwith "invalid scroll-state() container query")
    else ignore (Media.of_string raw : Media.t);
    raw
  in
  let parse_unnamed s =
    match Media.of_string s with
    | Media.Min_width_rem rem -> Min_width_rem rem
    | Media.Min_width px when Float.is_integer px ->
        Min_width_px (int_of_float px)
    | media -> Custom media
    | exception Failure _ -> Raw (validate_raw s)
  in
  match split_named s with
  | Some (name, raw) -> Named (name, parse_unnamed raw)
  | None -> parse_unnamed s
