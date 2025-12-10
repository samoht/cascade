(** Font-face descriptor types for type-safe [\@font-face] construction. *)

(** {1 Metric Override Types} *)

(** Metric override value - either "normal" or a percentage. Used for
    ascent-override, descent-override, line-gap-override. *)
type metric_override = Normal | Percent of float

let metric_override_to_string = function
  | Normal -> "normal"
  | Percent p -> Printf.sprintf "%g%%" p

(** {1 Size Adjust} *)

type size_adjust = float
(** Size adjustment percentage. *)

let size_adjust_to_string p = Printf.sprintf "%g%%" p

(** {1 Font Source} *)

(** A single font source entry. *)
type src_entry =
  | Url of { url : string; format : string option; tech : string option }
  | Local of string
  | Raw of string  (** Escape hatch for unparsed sources *)

type src = src_entry list
(** Font source list. *)

let src_entry_to_string = function
  | Url { url; format; tech } -> (
      let base = Printf.sprintf "url(\"%s\")" url in
      let with_format =
        match format with
        | Some f -> base ^ Printf.sprintf " format(\"%s\")" f
        | None -> base
      in
      match tech with
      | Some t -> with_format ^ Printf.sprintf " tech(%s)" t
      | None -> with_format)
  | Local name -> Printf.sprintf "local(\"%s\")" name
  | Raw s -> s

let src_to_string entries =
  String.concat ", " (List.map src_entry_to_string entries)

(** {1 Parsing} *)

(** Parse a metric override string like "normal" or "90%". *)
let metric_override_of_string s =
  let s = String.trim s in
  if String.equal s "normal" then Normal
  else if String.length s > 0 && s.[String.length s - 1] = '%' then
    try Percent (float_of_string (String.sub s 0 (String.length s - 1)))
    with _ -> Normal (* fallback *)
  else Normal (* fallback *)

(** Parse a size-adjust percentage like "90%". *)
let size_adjust_of_string s =
  let s = String.trim s in
  if String.length s > 0 && s.[String.length s - 1] = '%' then
    try float_of_string (String.sub s 0 (String.length s - 1))
    with _ -> 100. (* fallback to 100% *)
  else 100. (* fallback *)

(** Parse a src string into a list of entries. Falls back to Raw for complex
    values. *)
let src_of_string s = [ Raw s ]
