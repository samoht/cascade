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

let suffix = function
  | Active_descendant -> "activedescendant"
  | Atomic -> "atomic"
  | Autocomplete -> "autocomplete"
  | Braillelabel -> "braillelabel"
  | Brailleroledescription -> "brailleroledescription"
  | Busy -> "busy"
  | Checked -> "checked"
  | Colcount -> "colcount"
  | Colindex -> "colindex"
  | Colindextext -> "colindextext"
  | Colspan -> "colspan"
  | Controls -> "controls"
  | Current -> "current"
  | Describedby -> "describedby"
  | Description -> "description"
  | Details -> "details"
  | Disabled -> "disabled"
  | Dropeffect -> "dropeffect"
  | Errormessage -> "errormessage"
  | Expanded -> "expanded"
  | Flowto -> "flowto"
  | Grabbed -> "grabbed"
  | Haspopup -> "haspopup"
  | Hidden -> "hidden"
  | Invalid -> "invalid"
  | Keyshortcuts -> "keyshortcuts"
  | Label -> "label"
  | Labelledby -> "labelledby"
  | Level -> "level"
  | Live -> "live"
  | Modal -> "modal"
  | Multiline -> "multiline"
  | Multiselectable -> "multiselectable"
  | Orientation -> "orientation"
  | Owns -> "owns"
  | Placeholder -> "placeholder"
  | Posinset -> "posinset"
  | Pressed -> "pressed"
  | Readonly -> "readonly"
  | Relevant -> "relevant"
  | Required -> "required"
  | Roledescription -> "roledescription"
  | Rowcount -> "rowcount"
  | Rowindex -> "rowindex"
  | Rowindextext -> "rowindextext"
  | Rowspan -> "rowspan"
  | Selected -> "selected"
  | Setsize -> "setsize"
  | Sort -> "sort"
  | Valuemax -> "valuemax"
  | Valuemin -> "valuemin"
  | Valuenow -> "valuenow"
  | Valuetext -> "valuetext"

let pp ctx attr =
  Pp.string ctx "aria-";
  Pp.string ctx (suffix attr)

let to_string attr = Pp.to_string pp attr

let of_string = function
  | "aria-activedescendant" -> Some Active_descendant
  | "aria-atomic" -> Some Atomic
  | "aria-autocomplete" -> Some Autocomplete
  | "aria-braillelabel" -> Some Braillelabel
  | "aria-brailleroledescription" -> Some Brailleroledescription
  | "aria-busy" -> Some Busy
  | "aria-checked" -> Some Checked
  | "aria-colcount" -> Some Colcount
  | "aria-colindex" -> Some Colindex
  | "aria-colindextext" -> Some Colindextext
  | "aria-colspan" -> Some Colspan
  | "aria-controls" -> Some Controls
  | "aria-current" -> Some Current
  | "aria-describedby" -> Some Describedby
  | "aria-description" -> Some Description
  | "aria-details" -> Some Details
  | "aria-disabled" -> Some Disabled
  | "aria-dropeffect" -> Some Dropeffect
  | "aria-errormessage" -> Some Errormessage
  | "aria-expanded" -> Some Expanded
  | "aria-flowto" -> Some Flowto
  | "aria-grabbed" -> Some Grabbed
  | "aria-haspopup" -> Some Haspopup
  | "aria-hidden" -> Some Hidden
  | "aria-invalid" -> Some Invalid
  | "aria-keyshortcuts" -> Some Keyshortcuts
  | "aria-label" -> Some Label
  | "aria-labelledby" -> Some Labelledby
  | "aria-level" -> Some Level
  | "aria-live" -> Some Live
  | "aria-modal" -> Some Modal
  | "aria-multiline" -> Some Multiline
  | "aria-multiselectable" -> Some Multiselectable
  | "aria-orientation" -> Some Orientation
  | "aria-owns" -> Some Owns
  | "aria-placeholder" -> Some Placeholder
  | "aria-posinset" -> Some Posinset
  | "aria-pressed" -> Some Pressed
  | "aria-readonly" -> Some Readonly
  | "aria-relevant" -> Some Relevant
  | "aria-required" -> Some Required
  | "aria-roledescription" -> Some Roledescription
  | "aria-rowcount" -> Some Rowcount
  | "aria-rowindex" -> Some Rowindex
  | "aria-rowindextext" -> Some Rowindextext
  | "aria-rowspan" -> Some Rowspan
  | "aria-selected" -> Some Selected
  | "aria-setsize" -> Some Setsize
  | "aria-sort" -> Some Sort
  | "aria-valuemax" -> Some Valuemax
  | "aria-valuemin" -> Some Valuemin
  | "aria-valuenow" -> Some Valuenow
  | "aria-valuetext" -> Some Valuetext
  | _ -> None
