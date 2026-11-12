(** CSS Inline 3 and CSS Ruby 1 properties: vertical-align, initial-letter,
    text-box, line-fit-edge and the ruby properties. *)

open Properties_intf

val pp_text_box_trim : text_box_trim Pp.t
(** [pp_text_box_trim] prints a [text_box_trim]. *)

val pp_text_box_edge_keyword : text_box_edge_keyword Pp.t
(** [pp_text_box_edge_keyword] prints a [text_box_edge_keyword]. *)

val pp_text_box_edge : text_box_edge Pp.t
(** [pp_text_box_edge] prints a [text_box_edge]. *)

val pp_text_box : text_box Pp.t
(** [pp_text_box] prints a [text_box]. *)

val pp_line_fit_edge_keyword : line_fit_edge_keyword Pp.t
(** [pp_line_fit_edge_keyword] prints a [line_fit_edge_keyword]. *)

val pp_line_fit_edge : line_fit_edge Pp.t
(** [pp_line_fit_edge] prints a [line_fit_edge]. *)

val pp_initial_letter : initial_letter Pp.t
(** [pp_initial_letter] prints an [initial_letter]. *)

val pp_initial_letter_align_keyword : initial_letter_align_keyword Pp.t
(** [pp_initial_letter_align_keyword] prints an [initial_letter_align_keyword].
*)

val pp_initial_letter_align : initial_letter_align Pp.t
(** [pp_initial_letter_align] prints an [initial_letter_align]. *)

val normalize_initial_letter_wrap : initial_letter_wrap -> initial_letter_wrap
(** [normalize_initial_letter_wrap v] is [v] in its canonical spelling. *)

val pp_initial_letter_wrap : initial_letter_wrap Pp.t
(** [pp_initial_letter_wrap] prints an [initial_letter_wrap]. *)

val pp_ruby_merge : ruby_merge Pp.t
(** [pp_ruby_merge] prints a [ruby_merge]. *)

val pp_ruby_align : ruby_align Pp.t
(** [pp_ruby_align] prints a [ruby_align]. *)

val pp_ruby_overhang : ruby_overhang Pp.t
(** [pp_ruby_overhang] prints a [ruby_overhang]. *)

val pp_ruby_position_keyword : ruby_position_keyword Pp.t
(** [pp_ruby_position_keyword] prints a [ruby_position_keyword]. *)

val pp_ruby_position : ruby_position Pp.t
(** [pp_ruby_position] prints a [ruby_position]. *)

val pp_vertical_align : vertical_align Pp.t
(** [pp_vertical_align] prints a [vertical_align]. *)

val read_text_box_trim : Cursor.t -> text_box_trim
(** [read_text_box_trim t] reads a [text_box_trim] from [t]. *)

val read_text_box_edge_keyword : Cursor.t -> text_box_edge_keyword
(** [read_text_box_edge_keyword t] reads a [text_box_edge_keyword] from [t]. *)

val read_text_box_edge : ?global:bool -> Cursor.t -> text_box_edge
(** [read_text_box_edge ?global t] reads a [text_box_edge] from [t]; [global]
    (default [true]) also accepts the CSS-wide keywords and [var()]. *)

val read_text_box : Cursor.t -> text_box
(** [read_text_box t] reads a [text_box] from [t]. *)

val read_line_fit_edge_keyword : Cursor.t -> line_fit_edge_keyword
(** [read_line_fit_edge_keyword t] reads a [line_fit_edge_keyword] from [t]. *)

val read_line_fit_edge : Cursor.t -> line_fit_edge
(** [read_line_fit_edge t] reads a [line_fit_edge] from [t]. *)

val read_ruby_merge : Cursor.t -> ruby_merge
(** [read_ruby_merge t] reads a [ruby_merge] from [t]. *)

val read_ruby_align : Cursor.t -> ruby_align
(** [read_ruby_align t] reads a [ruby_align] from [t]. *)

val read_ruby_overhang : Cursor.t -> ruby_overhang
(** [read_ruby_overhang t] reads a [ruby_overhang] from [t]. *)

val read_ruby_position_keyword : Cursor.t -> ruby_position_keyword
(** [read_ruby_position_keyword t] reads a [ruby_position_keyword] from [t]. *)

val read_ruby_position : Cursor.t -> ruby_position
(** [read_ruby_position t] reads a [ruby_position] from [t]. *)

val read_vertical_align : Cursor.t -> vertical_align
(** [read_vertical_align t] reads a [vertical_align] from [t]. *)

val normalize_vertical_align : vertical_align -> vertical_align
(** [normalize_vertical_align v] is [v] in its canonical spelling. *)

val normalize_initial_letter : initial_letter -> initial_letter
(** [normalize_initial_letter v] is [v] in its canonical spelling. *)

val read_initial_letter_align_keyword : Cursor.t -> initial_letter_align_keyword
(** [read_initial_letter_align_keyword t] reads an
    [initial_letter_align_keyword] from [t]. *)

val read_initial_letter : Cursor.t -> initial_letter
(** [read_initial_letter t] reads an [initial_letter] from [t]. *)

val read_initial_letter_align : Cursor.t -> initial_letter_align
(** [read_initial_letter_align t] reads an [initial_letter_align] from [t]. *)

val read_initial_letter_wrap : Cursor.t -> initial_letter_wrap
(** [read_initial_letter_wrap t] reads an [initial_letter_wrap] from [t]. *)
