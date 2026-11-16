(** Length canonicalisation (CSS Values 4 sec. 6): every unit converts to
    pixels, absolute units through the fixed 1in = 96px table, font- and
    viewport-relative units against the base sizes the context supplies. *)

open Context_intf

module Length : sig
  type ctx = {
    root_font_size : float option;
    parent_font_size : float option;
    viewport_width : float option;
    viewport_height : float option;
    container_width : float option;
    container_height : float option;
  }
  (** The base sizes, in pixels, that relative units resolve against. *)

  val to_px : ctx -> Values.length -> float option
  (** [to_px ctx l] is [l] in pixels. [None] when [l] depends on a size [ctx]
      lacks, or on glyph metrics or a containing block. *)

  val media_to_px : Values.length -> float option
  (** [media_to_px l] is {!to_px} against the 16px font sizes that relative
      units in [@media] and [@container] resolve against. *)

  val normalize_zero : Values.length -> Values.length
  (** [normalize_zero l] is [0] when [l] is [0px], [l] otherwise. *)

  val of_t : t -> ctx
  (** [of_t ctx] is the pixel base sizes [ctx] states. *)

  val simplify :
    ?preserve_authored_calc:bool ->
    ?layer_order:string list ->
    ?layer:string ->
    t ->
    ctx ->
    Values.length ->
    Values.length
  (** [simplify cascade ctx l] is [l] with its references resolved against
      [cascade] and its [calc()] folded against [ctx]. [preserve_authored_calc]
      (default [true]) skips the pixel folding of an authored [calc()] that
      holds a reference. *)
end
