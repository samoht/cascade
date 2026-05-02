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
  | "aria-activedescendant" -> Active_descendant
  | "aria-atomic" -> Atomic
  | "aria-autocomplete" -> Autocomplete
  | "aria-braillelabel" -> Braillelabel
  | "aria-brailleroledescription" -> Brailleroledescription
  | "aria-busy" -> Busy
  | "aria-checked" -> Checked
  | "aria-colcount" -> Colcount
  | "aria-colindex" -> Colindex
  | "aria-colindextext" -> Colindextext
  | "aria-colspan" -> Colspan
  | "aria-controls" -> Controls
  | "aria-current" -> Current
  | "aria-describedby" -> Describedby
  | "aria-description" -> Description
  | "aria-details" -> Details
  | "aria-disabled" -> Disabled
  | "aria-dropeffect" -> Dropeffect
  | "aria-errormessage" -> Errormessage
  | "aria-expanded" -> Expanded
  | "aria-flowto" -> Flowto
  | "aria-grabbed" -> Grabbed
  | "aria-haspopup" -> Haspopup
  | "aria-hidden" -> Hidden
  | "aria-invalid" -> Invalid
  | "aria-keyshortcuts" -> Keyshortcuts
  | "aria-label" -> Label
  | "aria-labelledby" -> Labelledby
  | "aria-level" -> Level
  | "aria-live" -> Live
  | "aria-modal" -> Modal
  | "aria-multiline" -> Multiline
  | "aria-multiselectable" -> Multiselectable
  | "aria-orientation" -> Orientation
  | "aria-owns" -> Owns
  | "aria-placeholder" -> Placeholder
  | "aria-posinset" -> Posinset
  | "aria-pressed" -> Pressed
  | "aria-readonly" -> Readonly
  | "aria-relevant" -> Relevant
  | "aria-required" -> Required
  | "aria-roledescription" -> Roledescription
  | "aria-rowcount" -> Rowcount
  | "aria-rowindex" -> Rowindex
  | "aria-rowindextext" -> Rowindextext
  | "aria-rowspan" -> Rowspan
  | "aria-selected" -> Selected
  | "aria-setsize" -> Setsize
  | "aria-sort" -> Sort
  | "aria-valuemax" -> Valuemax
  | "aria-valuemin" -> Valuemin
  | "aria-valuenow" -> Valuenow
  | "aria-valuetext" -> Valuetext
  | name -> invalid_arg ("not a supported aria attribute: " ^ name)
