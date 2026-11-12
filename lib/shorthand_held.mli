(** Shorthand slots that a contraction would reset: the slot bits of the
    [transition], [animation], [text-decoration] and [border-image] families,
    the {!type-held} summary of what neighbouring rules hold, and the reset
    hazard a composer checks before leaving a slot out. *)

(** A slot of one [transition] layer. *)
type tr_slot = Property | Duration | Timing | Delay | Behavior

val tr_slot_bit : tr_slot -> int
(** [tr_slot_bit s] is the bit standing for [s]. *)

val all_tr_bits : int
(** The bits of every [transition] slot. *)

(** A slot of one [animation] layer. *)
type an_slot =
  | Name
  | Duration
  | Timing
  | Delay
  | Iteration
  | Direction
  | Fill
  | Play
  | Timeline
  | Range_start
  | Range_end

val an_slot_bit : an_slot -> int
(** [an_slot_bit s] is the bit standing for [s], disjoint from the [transition]
    bits. *)

val all_an_bits : int
(** The bits of every [animation] slot. *)

val td_thickness_bit : int
(** The bit of the [text-decoration-thickness] slot. *)

val border_image_bit : int
(** The bits of every [border-image] longhand slot. *)

val an_overwritten_slots : Declaration.declaration -> int
(** [an_overwritten_slots d] is the [animation] slots [d] sets at all. *)

type held
(** Slots a set of declarations leaves holding something other than the slot
    initial, split by importance. *)

val held_none : held
(** Nothing held. *)

val held_add : held -> Declaration.declaration list -> held
(** [held_add held decls] extends [held] with the slots [decls] hold. *)

val held_outside : held -> Declaration.declaration list -> held
(** [held_outside held decls] is [held] without the slots [decls] hold
    themselves. *)

val reset_hazard :
  slots_of:(Declaration.declaration -> int) ->
  Rule_index.t ->
  int ->
  held:held ->
  missing:int ->
  important:bool ->
  bool
(** [reset_hazard ~slots_of idx i ~held ~missing ~important] is [true] when
    [held], or a declaration of [idx] before [i] read through [slots_of], holds
    one of the [missing] slots a run at [i] of importance [important] would
    reset. *)

val tr_written_slots : Declaration.declaration -> int
(** [tr_written_slots d] is the [transition] slots [d] writes, initial values
    included. *)

val an_written_slots : Declaration.declaration -> int
(** [an_written_slots d] is the [animation] slots [d] writes, initial values
    included. *)

val tr_reset_hazard :
  Rule_index.t -> int -> held:held -> missing:int -> important:bool -> bool
(** [tr_reset_hazard] is {!val-reset_hazard} over the [transition] slots. *)

val td_reset_hazard :
  Rule_index.t -> int -> held:held -> missing:int -> important:bool -> bool
(** [td_reset_hazard] is {!val-reset_hazard} over the [text-decoration]
    thickness slot. *)

val bi_reset_hazard :
  Rule_index.t -> int -> held:held -> missing:int -> important:bool -> bool
(** [bi_reset_hazard] is {!val-reset_hazard} over the [border-image] slots. *)
