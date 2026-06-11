(** WAI-ARIA states and properties. *)

type t =
  | Active_descendant
  | Atomic
  | Autocomplete
  | Braillelabel
  | Brailleroledescription
  | Busy
  | Checked
  | Colcount
  | Colindex
  | Colindextext
  | Colspan
  | Controls
  | Current
  | Describedby
  | Description
  | Details
  | Disabled
  | Dropeffect
  | Errormessage
  | Expanded
  | Flowto
  | Grabbed
  | Haspopup
  | Hidden
  | Invalid
  | Keyshortcuts
  | Label
  | Labelledby
  | Level
  | Live
  | Modal
  | Multiline
  | Multiselectable
  | Orientation
  | Owns
  | Placeholder
  | Posinset
  | Pressed
  | Readonly
  | Relevant
  | Required
  | Roledescription
  | Rowcount
  | Rowindex
  | Rowindextext
  | Rowspan
  | Selected
  | Setsize
  | Sort
  | Valuemax
  | Valuemin
  | Valuenow
  | Valuetext

val suffix : t -> string
(** [suffix attr] is the attribute name without the [aria-] prefix. *)

val to_string : t -> string
(** [to_string attr] is the full [aria-*] attribute name. *)

val of_string : string -> t
(** [of_string name] parses a full [aria-*] attribute name. *)

val pp : t Pp.t
(** [pp] prints the full [aria-*] attribute name. *)
