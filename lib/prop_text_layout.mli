(** CSS Text 4 properties: text alignment, indent, transform, overflow and
    wrapping, white-space, word-break, overflow-wrap, hyphens, line-break,
    tab-size and text-size-adjust. *)

open Properties_intf

val read_text_align : Cursor.t -> text_align
(** [read_text_align t] reads a [text_align] from [t]. *)

val read_text_indent_value : Cursor.t -> text_indent_value
(** [read_text_indent_value t] reads a [text_indent_value] from [t]. *)

val read_text_transform : Cursor.t -> text_transform
(** [read_text_transform t] reads a [text_transform] from [t]. *)

val read_text_transform_case : Cursor.t -> text_transform_case
(** [read_text_transform_case t] reads a [text_transform_case] from [t]. *)

val pp_line_break : line_break Pp.t
(** [pp_line_break] prints a [line_break]. *)

val read_line_break : Cursor.t -> line_break
(** [read_line_break t] reads a [line_break] from [t]. *)

val normalize_text_indent : text_indent_value -> text_indent_value
(** [normalize_text_indent v] is [v] in its canonical spelling. *)

val normalize_hyphenate_limit_chars :
  ctx:Values.calc_ctx -> hyphenate_limit_chars -> hyphenate_limit_chars
(** [normalize_hyphenate_limit_chars ~ctx v] is [v] in its canonical spelling,
    static math folded under [ctx]. *)

val normalize_tab_size : ctx:Values.calc_ctx -> tab_size -> tab_size
(** [normalize_tab_size ~ctx v] is [v] in its canonical spelling, static math
    folded under [ctx]. *)

val pp_tab_size : tab_size Pp.t
(** [pp_tab_size] prints a [tab_size]. *)

val pp_text_align : text_align Pp.t
(** [pp_text_align] prints a [text_align]. *)

val pp_text_indent_value : text_indent_value Pp.t
(** [pp_text_indent_value] prints a [text_indent_value]. *)

val pp_text_transform_case : Pp.ctx -> text_transform_case -> unit
(** [pp_text_transform_case] prints a [text_transform_case]. *)

val pp_text_transform : text_transform Pp.t
(** [pp_text_transform] prints a [text_transform]. *)

val pp_text_overflow : text_overflow Pp.t
(** [pp_text_overflow] prints a [text_overflow]. *)

val pp_text_wrap : text_wrap Pp.t
(** [pp_text_wrap] prints a [text_wrap]. *)

val pp_text_wrap_mode : text_wrap_mode Pp.t
(** [pp_text_wrap_mode] prints a [text_wrap_mode]. *)

val pp_text_wrap_style : text_wrap_style Pp.t
(** [pp_text_wrap_style] prints a [text_wrap_style]. *)

val pp_text_spacing_trim : text_spacing_trim Pp.t
(** [pp_text_spacing_trim] prints a [text_spacing_trim]. *)

val pp_hyphenate_limit_chars_item : hyphenate_limit_chars_item Pp.t
(** [pp_hyphenate_limit_chars_item] prints a [hyphenate_limit_chars_item]. *)

val pp_hyphenate_limit_chars : hyphenate_limit_chars Pp.t
(** [pp_hyphenate_limit_chars] prints a [hyphenate_limit_chars]. *)

val pp_white_space : white_space Pp.t
(** [pp_white_space] prints a [white_space]. *)

val pp_word_break : word_break Pp.t
(** [pp_word_break] prints a [word_break]. *)

val pp_overflow_wrap : overflow_wrap Pp.t
(** [pp_overflow_wrap] prints an [overflow_wrap]. *)

val pp_hyphens : hyphens Pp.t
(** [pp_hyphens] prints a [hyphens]. *)

val pp_text_size_adjust : text_size_adjust Pp.t
(** [pp_text_size_adjust] prints a [text_size_adjust]. *)

val read_text_overflow : Cursor.t -> text_overflow
(** [read_text_overflow t] reads a [text_overflow] from [t]. *)

val read_text_wrap : Cursor.t -> text_wrap
(** [read_text_wrap t] reads a [text_wrap] from [t]. *)

val read_text_wrap_mode : Cursor.t -> text_wrap_mode
(** [read_text_wrap_mode t] reads a [text_wrap_mode] from [t]. *)

val read_text_wrap_style : Cursor.t -> text_wrap_style
(** [read_text_wrap_style t] reads a [text_wrap_style] from [t]. *)

val read_text_spacing_trim : Cursor.t -> text_spacing_trim
(** [read_text_spacing_trim t] reads a [text_spacing_trim] from [t]. *)

val read_hyphenate_limit_chars_item : Cursor.t -> hyphenate_limit_chars_item
(** [read_hyphenate_limit_chars_item t] reads a [hyphenate_limit_chars_item]
    from [t]. *)

val read_hyphenate_limit_chars : Cursor.t -> hyphenate_limit_chars
(** [read_hyphenate_limit_chars t] reads a [hyphenate_limit_chars] from [t]. *)

val read_white_space : Cursor.t -> white_space
(** [read_white_space t] reads a [white_space] from [t]. *)

val pp_white_space_collapse : white_space_collapse Pp.t
(** [pp_white_space_collapse] prints a [white_space_collapse]. *)

val read_white_space_collapse : Cursor.t -> white_space_collapse
(** [read_white_space_collapse t] reads a [white_space_collapse] from [t]. *)

val read_word_break : Cursor.t -> word_break
(** [read_word_break t] reads a [word_break] from [t]. *)

val read_overflow_wrap : Cursor.t -> overflow_wrap
(** [read_overflow_wrap t] reads an [overflow_wrap] from [t]. *)

val read_hyphens : Cursor.t -> hyphens
(** [read_hyphens t] reads a [hyphens] from [t]. *)

val read_text_size_adjust : Cursor.t -> text_size_adjust
(** [read_text_size_adjust t] reads a [text_size_adjust] from [t]. *)

val read_tab_size : Cursor.t -> tab_size
(** [read_tab_size t] reads a [tab_size] from [t]. *)

val normalize_text_size_adjust : text_size_adjust -> text_size_adjust
(** [normalize_text_size_adjust v] is [v] in its canonical spelling. *)
