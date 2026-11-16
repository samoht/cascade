(* Property identity: a dense tag per constructor, the order it induces and the
   type-equality proof between two properties. *)

open Common
open Properties_intf

(* A dense integer per property constructor, in the order [Properties_intf]
   declares them. It exists so an ordered container keyed on a property
   identity compares two integers rather than walking the runtime
   representation through [caml_compare]; ordering an identity never has to
   relate the two payload types a [Declaration] pack hides, so a tag is all it
   takes.

   The match carries no wildcard, so a new constructor fails this build, and
   [scripts/check_properties.ml] pins that no two constructors take the same
   tag - a silent collision would make an ordered container hold one entry for
   two different properties. *)
(* PROPERTY_TAG_START - Used by scripts/check_properties.ml *)
let property_tag : type a. a property -> int = function
  | Custom_property _ -> 0
  | Unknown_property _ -> 1
  | All -> 2
  | Background_color -> 3
  | Color -> 4
  | Border_color -> 5
  | Border_style -> 6
  | Border_top_style -> 7
  | Border_right_style -> 8
  | Border_bottom_style -> 9
  | Border_left_style -> 10
  | Border_inline_start_style -> 11
  | Border_inline_end_style -> 12
  | Border_block_start_style -> 13
  | Border_block_end_style -> 14
  | Padding -> 15
  | Padding_left -> 16
  | Padding_right -> 17
  | Padding_bottom -> 18
  | Padding_top -> 19
  | Padding_inline -> 20
  | Padding_inline_start -> 21
  | Padding_inline_end -> 22
  | Padding_block -> 23
  | Padding_block_start -> 24
  | Padding_block_end -> 25
  | Margin -> 26
  | Margin_inline_end -> 27
  | Margin_inline_start -> 28
  | Margin_left -> 29
  | Margin_right -> 30
  | Margin_top -> 31
  | Margin_bottom -> 32
  | Margin_inline -> 33
  | Margin_block -> 34
  | Margin_block_start -> 35
  | Margin_block_end -> 36
  | Gap -> 37
  | Column_gap -> 38
  | Row_gap -> 39
  | Width -> 40
  | Height -> 41
  | Min_width -> 42
  | Min_height -> 43
  | Max_width -> 44
  | Max_height -> 45
  | Inline_size -> 46
  | Min_inline_size -> 47
  | Max_inline_size -> 48
  | Block_size -> 49
  | Min_block_size -> 50
  | Max_block_size -> 51
  | Font_size -> 52
  | Line_height -> 53
  | Font_weight -> 54
  | Font_style -> 55
  | Text_align -> 56
  | Text_decoration -> 57
  | Text_decoration_line -> 58
  | Text_decoration_style -> 59
  | Text_decoration_color -> 60
  | Text_underline_offset -> 61
  | Text_decoration_skip -> 62
  | Text_decoration_skip_self -> 63
  | Text_decoration_skip_box -> 64
  | Text_decoration_skip_inset -> 65
  | Text_decoration_skip_spaces -> 66
  | Text_emphasis -> 67
  | Text_emphasis_style -> 68
  | Text_emphasis_color -> 69
  | Text_emphasis_position -> 70
  | Text_emphasis_skip -> 71
  | Text_orientation -> 72
  | Text_transform -> 73
  | Letter_spacing -> 74
  | List_style_type -> 75
  | List_style_position -> 76
  | List_style_image -> 77
  | Display -> 78
  | Position -> 79
  | Visibility -> 80
  | Baseline_source -> 81
  | Alignment_baseline -> 82
  | Baseline_shift -> 83
  | Flex_direction -> 84
  | Flex_wrap -> 85
  | Flex_flow -> 86
  | Flex -> 87
  | Flex_grow -> 88
  | Flex_shrink -> 89
  | Flex_basis -> 90
  | Order -> 91
  | Align_items -> 92
  | Justify_content -> 93
  | Justify_items -> 94
  | Justify_self -> 95
  | Align_content -> 96
  | Align_self -> 97
  | Place_content -> 98
  | Place_items -> 99
  | Place_self -> 100
  | Grid_template_columns -> 101
  | Grid_template_rows -> 102
  | Grid_template_areas -> 103
  | Grid_template -> 104
  | Grid -> 105
  | Grid_area -> 106
  | Grid_auto_flow -> 107
  | Grid_auto_columns -> 108
  | Grid_auto_rows -> 109
  | Grid_column -> 110
  | Grid_row -> 111
  | Grid_column_start -> 112
  | Grid_column_end -> 113
  | Grid_row_start -> 114
  | Grid_row_end -> 115
  | Border_width -> 116
  | Border_top_width -> 117
  | Border_right_width -> 118
  | Border_bottom_width -> 119
  | Border_left_width -> 120
  | Border_inline_start_width -> 121
  | Border_inline_end_width -> 122
  | Border_block_start_width -> 123
  | Border_block_end_width -> 124
  | Border_inline_width -> 125
  | Border_block_width -> 126
  | Border_image -> 127
  | Border_image_source -> 128
  | Border_image_slice -> 129
  | Border_image_repeat -> 130
  | Border_image_width -> 131
  | Border_image_outset -> 132
  | Border_radius -> 133
  | Border_top_left_radius -> 134
  | Border_top_right_radius -> 135
  | Border_bottom_left_radius -> 136
  | Border_bottom_right_radius -> 137
  | Border_top_color -> 138
  | Border_right_color -> 139
  | Border_bottom_color -> 140
  | Border_left_color -> 141
  | Border_inline_start_color -> 142
  | Border_inline_end_color -> 143
  | Border_block_start_color -> 144
  | Border_block_end_color -> 145
  | Border_inline_color -> 146
  | Border_block_color -> 147
  | Border_inline_style -> 148
  | Border_block_style -> 149
  | Border_start_start_radius -> 150
  | Border_start_end_radius -> 151
  | Border_end_start_radius -> 152
  | Border_end_end_radius -> 153
  | Opacity -> 154
  | Fill_opacity -> 155
  | Stroke_opacity -> 156
  | Stop_opacity -> 157
  | Flood_opacity -> 158
  | Mix_blend_mode -> 159
  | Transform -> 160
  | Translate -> 161
  | Cursor -> 162
  | Interactivity -> 163
  | Caret_animation -> 164
  | Caret_shape -> 165
  | Caret -> 166
  | Interest_delay -> 167
  | Interest_delay_start -> 168
  | Interest_delay_end -> 169
  | Nav_up -> 170
  | Nav_right -> 171
  | Nav_down -> 172
  | Nav_left -> 173
  | Table_layout -> 174
  | Border_collapse -> 175
  | Border_spacing -> 176
  | User_select -> 177
  | Pointer_events -> 178
  | Overflow -> 179
  | Inset -> 180
  | Inset_inline -> 181
  | Inset_inline_start -> 182
  | Inset_inline_end -> 183
  | Inset_block -> 184
  | Inset_block_start -> 185
  | Inset_block_end -> 186
  | Top -> 187
  | Right -> 188
  | Bottom -> 189
  | Left -> 190
  | Z_index -> 191
  | Outline -> 192
  | Outline_style -> 193
  | Outline_width -> 194
  | Outline_color -> 195
  | Outline_offset -> 196
  | Forced_color_adjust -> 197
  | Scroll_snap_type -> 198
  | White_space -> 199
  | White_space_collapse -> 541
  | Font_variant_alternates -> 548
  | Font_variant -> 549
  | Border -> 200
  | Border_block -> 201
  | Border_block_start -> 202
  | Border_block_end -> 203
  | Border_inline -> 204
  | Border_inline_start -> 205
  | Border_inline_end -> 206
  | Background -> 207
  | Tab_size -> 208
  | Zoom -> 209
  | Webkit_text_size_adjust -> 210
  | Font_feature_settings -> 211
  | Font_variation_settings -> 212
  | Webkit_tap_highlight_color -> 213
  | Webkit_user_select -> 214
  | Moz_user_select -> 215
  | Ms_user_select -> 216
  | Webkit_text_decoration -> 217
  | Webkit_text_decoration_color -> 218
  | Webkit_text_fill_color -> 219
  | Webkit_text_stroke_color -> 220
  | Webkit_text_stroke -> 542
  | Webkit_text_stroke_width -> 543
  | Text_indent -> 221
  | List_style -> 222
  | Font -> 223
  | Source -> 224
  | Webkit_appearance -> 225
  | Webkit_transform -> 226
  | Moz_transform -> 227
  | Ms_transform -> 228
  | O_transform -> 229
  | Webkit_transition -> 230
  | Webkit_transition_delay -> 231
  | Webkit_transition_duration -> 232
  | Webkit_transition_property -> 233
  | Webkit_transition_timing_function -> 234
  | Webkit_animation -> 235
  | Webkit_animation_delay -> 236
  | Webkit_animation_duration -> 237
  | Webkit_animation_direction -> 238
  | Webkit_animation_iteration_count -> 239
  | Webkit_animation_name -> 240
  | Webkit_animation_timing_function -> 241
  | Webkit_animation_fill_mode -> 242
  | Webkit_animation_play_state -> 243
  | Webkit_flex_direction -> 244
  | Webkit_flex_wrap -> 245
  | Webkit_flex_flow -> 246
  | Webkit_justify_content -> 247
  | Webkit_align_items -> 248
  | Webkit_align_content -> 249
  | Webkit_align_self -> 250
  | Webkit_border_radius -> 251
  | Webkit_box_sizing -> 252
  | Moz_box_sizing -> 253
  | Webkit_box_shadow -> 254
  | Webkit_background_size -> 255
  | Webkit_filter -> 256
  | Moz_appearance -> 257
  | Moz_animation -> 258
  | Moz_animation_delay -> 259
  | Moz_animation_duration -> 260
  | Moz_animation_direction -> 261
  | Moz_animation_iteration_count -> 262
  | Moz_animation_name -> 263
  | Moz_animation_timing_function -> 264
  | Moz_animation_fill_mode -> 265
  | Moz_animation_play_state -> 266
  | Moz_transition -> 267
  | Moz_transition_delay -> 268
  | Moz_transition_duration -> 269
  | Moz_transition_property -> 270
  | Moz_transition_timing_function -> 271
  | Moz_border_radius -> 272
  | Moz_box_shadow -> 273
  | Ms_filter -> 274
  | O_transition -> 275
  | Container_type -> 276
  | Container_name -> 277
  | Container -> 278
  | Anchor_name -> 279
  | Position_anchor -> 280
  | Position_try_fallbacks -> 281
  | Position_try_order -> 282
  | Position_try -> 283
  | Position_visibility -> 284
  | Position_area -> 285
  | Shape_outside -> 286
  | Shape_margin -> 287
  | Shape_image_threshold -> 288
  | Overflow_clip_margin -> 289
  | Overflow_anchor -> 290
  | Scrollbar_width -> 291
  | Scrollbar_color -> 292
  | Scrollbar_gutter -> 293
  | Line_height_step -> 294
  | Font_palette -> 295
  | Font_synthesis -> 296
  | Text_wrap_mode -> 297
  | Text_wrap_style -> 298
  | Text_box_trim -> 299
  | Text_underline_position -> 300
  | Text_box_edge -> 301
  | Text_box -> 302
  | Inline_sizing -> 303
  | Line_fit_edge -> 304
  | Interpolate_size -> 305
  | Min_intrinsic_sizing -> 306
  | Ruby_align -> 307
  | Ruby_merge -> 308
  | Ruby_overhang -> 309
  | Ruby_position -> 310
  | Glyph_orientation_vertical -> 311
  | Text_combine_upright -> 312
  | Animation_timeline -> 313
  | Animation_range -> 314
  | Animation_range_start -> 315
  | Animation_range_end -> 316
  | Scroll_timeline -> 317
  | Scroll_timeline_name -> 318
  | Scroll_timeline_axis -> 319
  | View_transition_name -> 320
  | View_transition_class -> 321
  | Image_orientation -> 322
  | Image_rendering -> 323
  | Image_resolution -> 324
  | Contain_intrinsic_size -> 325
  | Contain_intrinsic_width -> 326
  | Contain_intrinsic_height -> 327
  | Contain_intrinsic_block_size -> 328
  | Contain_intrinsic_inline_size -> 329
  | Margin_trim -> 330
  | Offset_path -> 331
  | Offset_distance -> 332
  | Offset_rotate -> 333
  | Font_size_adjust -> 334
  | Font_variant_emoji -> 335
  | Text_spacing_trim -> 336
  | Hyphenate_limit_chars -> 337
  | Initial_letter -> 338
  | Initial_letter_align -> 339
  | Initial_letter_wrap -> 340
  | Dominant_baseline -> 341
  | View_timeline_name -> 342
  | View_timeline_axis -> 343
  | View_timeline_inset -> 344
  | View_timeline -> 345
  | Timeline_scope -> 346
  | Perspective -> 347
  | Perspective_origin -> 348
  | Transform_style -> 349
  | Backface_visibility -> 350
  | Object_position -> 351
  | Rotate -> 352
  | Transition_duration -> 353
  | Transition_timing_function -> 354
  | Transition_delay -> 355
  | Transition_property -> 356
  | Transition_behavior -> 357
  | Overlay -> 358
  | Will_change -> 359
  | Contain -> 360
  | Isolation -> 361
  | Break_before -> 362
  | Break_after -> 363
  | Break_inside -> 364
  | Page_break_before -> 365
  | Page_break_after -> 366
  | Page_break_inside -> 367
  | Page_size -> 368
  | Columns -> 369
  | Column_width -> 370
  | Column_height -> 544
  | Column_wrap -> 545
  | Column_count -> 371
  | Column_rule -> 372
  | Column_rule_color -> 373
  | Column_rule_width -> 539
  | Column_rule_style -> 540
  | Column_span -> 374
  | Word_spacing -> 375
  | Background_attachment -> 376
  | Border_top -> 377
  | Border_right -> 378
  | Border_bottom -> 379
  | Border_left -> 380
  | Transform_origin -> 381
  | Transform_box -> 382
  | Text_shadow -> 383
  | Clip_path -> 384
  | Mask -> 385
  | Mask_border -> 386
  | Content_visibility -> 387
  | Filter -> 388
  | Background_image -> 389
  | Background_origin -> 390
  | Background_clip -> 391
  | Webkit_background_clip -> 392
  | Animation -> 393
  | Aspect_ratio -> 394
  | Overflow_x -> 395
  | Overflow_y -> 396
  | Overflow_block -> 397
  | Overflow_inline -> 398
  | Vertical_align -> 399
  | Font_family -> 400
  | Background_position -> 401
  | Background_position_x -> 537
  | Background_position_y -> 538
  | Webkit_mask_position_x -> 546
  | Webkit_mask_position_y -> 547
  | Background_repeat -> 402
  | Background_size -> 403
  | Webkit_font_smoothing -> 404
  | Moz_osx_font_smoothing -> 405
  | Webkit_line_clamp -> 406
  | Webkit_box_orient -> 407
  | Moz_orient -> 408
  | Text_overflow -> 409
  | Text_wrap -> 410
  | Word_break -> 411
  | Overflow_wrap -> 412
  | Line_break -> 413
  | Hyphens -> 414
  | Webkit_hyphens -> 415
  | Font_stretch -> 416
  | Font_optical_sizing -> 417
  | Font_kerning -> 418
  | Font_language_override -> 419
  | Font_synthesis_style -> 420
  | Font_synthesis_weight -> 421
  | Font_synthesis_small_caps -> 422
  | Font_synthesis_position -> 423
  | Font_variant_ligatures -> 424
  | Caps -> 425
  | Numeric -> 426
  | Font_variant_position -> 427
  | East_asian -> 428
  | Backdrop_filter -> 429
  | Webkit_backdrop_filter -> 430
  | Webkit_mask_image -> 431
  | Webkit_mask_composite -> 432
  | Webkit_mask_source_type -> 433
  | Webkit_mask_size -> 434
  | Webkit_mask_position -> 435
  | Webkit_mask_repeat -> 436
  | Webkit_mask_clip -> 437
  | Webkit_mask_origin -> 438
  | Mask_image -> 439
  | Mask_composite -> 440
  | Mask_mode -> 441
  | Mask_size -> 442
  | Mask_position -> 443
  | Mask_repeat -> 444
  | Mask_clip -> 445
  | Mask_origin -> 446
  | Mask_type -> 447
  | Scroll_snap_align -> 448
  | Scroll_snap_stop -> 449
  | Scroll_behavior -> 450
  | Box_sizing -> 451
  | Field_sizing -> 452
  | Caption_side -> 453
  | Resize -> 454
  | Object_fit -> 455
  | Object_view_box -> 456
  | Appearance -> 457
  | Color_scheme -> 458
  | Print_color_adjust -> 459
  | Webkit_print_color_adjust -> 460
  | Box_decoration_break -> 461
  | Webkit_box_decoration_break -> 462
  | Content -> 463
  | Counter_reset -> 464
  | Counter_increment -> 465
  | Quotes -> 466
  | Text_decoration_thickness -> 467
  | Text_size_adjust -> 468
  | Touch_action -> 469
  | Clip -> 470
  | Clear -> 471
  | Float -> 472
  | Scale -> 473
  | Transition -> 474
  | Box_shadow -> 475
  | Fill -> 476
  | Stroke -> 477
  | Stroke_width -> 478
  | Fill_rule -> 479
  | Clip_rule -> 480
  | Stroke_linecap -> 481
  | Stroke_linejoin -> 482
  | Stroke_miterlimit -> 483
  | Stroke_dashoffset -> 484
  | Stroke_dasharray -> 485
  | Paint_order -> 486
  | Vector_effect -> 487
  | Stop_color -> 488
  | Flood_color -> 489
  | Lighting_color -> 490
  | Direction -> 491
  | Unicode_bidi -> 492
  | Writing_mode -> 493
  | Text_decoration_skip_ink -> 494
  | Animation_name -> 495
  | Animation_duration -> 496
  | Animation_timing_function -> 497
  | Animation_delay -> 498
  | Animation_iteration_count -> 499
  | Animation_direction -> 500
  | Animation_fill_mode -> 501
  | Animation_play_state -> 502
  | Animation_composition -> 503
  | Background_blend_mode -> 504
  | Scroll_margin -> 505
  | Scroll_margin_top -> 506
  | Scroll_margin_right -> 507
  | Scroll_margin_bottom -> 508
  | Scroll_margin_left -> 509
  | Scroll_margin_inline -> 510
  | Scroll_margin_inline_start -> 511
  | Scroll_margin_inline_end -> 512
  | Scroll_margin_block -> 513
  | Scroll_margin_block_start -> 514
  | Scroll_margin_block_end -> 515
  | Scroll_padding -> 516
  | Scroll_padding_top -> 517
  | Scroll_padding_right -> 518
  | Scroll_padding_bottom -> 519
  | Scroll_padding_left -> 520
  | Scroll_padding_inline -> 521
  | Scroll_padding_inline_start -> 522
  | Scroll_padding_inline_end -> 523
  | Scroll_padding_block -> 524
  | Scroll_padding_block_start -> 525
  | Scroll_padding_block_end -> 526
  | Overscroll_behavior -> 527
  | Overscroll_behavior_x -> 528
  | Overscroll_behavior_y -> 529
  | Overscroll_behavior_block -> 530
  | Overscroll_behavior_inline -> 531
  | Accent_color -> 532
  | Caret_color -> 533
  | Offset_anchor -> 534
  | Offset_position -> 535
  | Offset -> 536
(* PROPERTY_TAG_END *)

(* Two property identities order by tag, and the two payload-carrying
   constructors by name within their own tag, so [0] means the same property
   exactly as [Declaration.equal_prop_key] does. *)
let compare_property : type a b. a property -> b property -> int =
 fun a b ->
  match (a, b) with
  | Custom_property x, Custom_property y -> String.compare x y
  | Unknown_property x, Unknown_property y -> String.compare x y
  | _ -> Int.compare (property_tag a) (property_tag b)

(* A property identity carries the type its value has, so two identities that
   agree prove their value types equal. [compare_property] answers the same
   question without the proof, and the two must agree: [Some Equal] exactly
   where the comparison is [0].

   [scripts/check_properties.ml] pins that the table below names every
   constructor. The trailing wildcard is what the type checker cannot see past:
   without the pin a property added later would silently read back as [None]. *)
(* PROPERTY_EQ_START - Used by scripts/check_properties.ml *)
let eq_property : type a b. a property -> b property -> (a, b) Type.eq option =
 fun a b ->
  match (a, b) with
  | Custom_property x, Custom_property y ->
      if String.equal x y then Some Equal else None
  | Unknown_property x, Unknown_property y ->
      if String.equal x y then Some Equal else None
  | All, All -> Some Equal
  | Background_color, Background_color -> Some Equal
  | Color, Color -> Some Equal
  | Border_color, Border_color -> Some Equal
  | Border_style, Border_style -> Some Equal
  | Border_top_style, Border_top_style -> Some Equal
  | Border_right_style, Border_right_style -> Some Equal
  | Border_bottom_style, Border_bottom_style -> Some Equal
  | Border_left_style, Border_left_style -> Some Equal
  | Border_inline_start_style, Border_inline_start_style -> Some Equal
  | Border_inline_end_style, Border_inline_end_style -> Some Equal
  | Border_block_start_style, Border_block_start_style -> Some Equal
  | Border_block_end_style, Border_block_end_style -> Some Equal
  | Padding, Padding -> Some Equal
  | Padding_left, Padding_left -> Some Equal
  | Padding_right, Padding_right -> Some Equal
  | Padding_bottom, Padding_bottom -> Some Equal
  | Padding_top, Padding_top -> Some Equal
  | Padding_inline, Padding_inline -> Some Equal
  | Padding_inline_start, Padding_inline_start -> Some Equal
  | Padding_inline_end, Padding_inline_end -> Some Equal
  | Padding_block, Padding_block -> Some Equal
  | Padding_block_start, Padding_block_start -> Some Equal
  | Padding_block_end, Padding_block_end -> Some Equal
  | Margin, Margin -> Some Equal
  | Margin_inline_end, Margin_inline_end -> Some Equal
  | Margin_inline_start, Margin_inline_start -> Some Equal
  | Margin_left, Margin_left -> Some Equal
  | Margin_right, Margin_right -> Some Equal
  | Margin_top, Margin_top -> Some Equal
  | Margin_bottom, Margin_bottom -> Some Equal
  | Margin_inline, Margin_inline -> Some Equal
  | Margin_block, Margin_block -> Some Equal
  | Margin_block_start, Margin_block_start -> Some Equal
  | Margin_block_end, Margin_block_end -> Some Equal
  | Gap, Gap -> Some Equal
  | Column_gap, Column_gap -> Some Equal
  | Row_gap, Row_gap -> Some Equal
  | Width, Width -> Some Equal
  | Height, Height -> Some Equal
  | Min_width, Min_width -> Some Equal
  | Min_height, Min_height -> Some Equal
  | Max_width, Max_width -> Some Equal
  | Max_height, Max_height -> Some Equal
  | Inline_size, Inline_size -> Some Equal
  | Min_inline_size, Min_inline_size -> Some Equal
  | Max_inline_size, Max_inline_size -> Some Equal
  | Block_size, Block_size -> Some Equal
  | Min_block_size, Min_block_size -> Some Equal
  | Max_block_size, Max_block_size -> Some Equal
  | Font_size, Font_size -> Some Equal
  | Line_height, Line_height -> Some Equal
  | Font_weight, Font_weight -> Some Equal
  | Font_style, Font_style -> Some Equal
  | Text_align, Text_align -> Some Equal
  | Text_decoration, Text_decoration -> Some Equal
  | Text_decoration_line, Text_decoration_line -> Some Equal
  | Text_decoration_style, Text_decoration_style -> Some Equal
  | Text_decoration_color, Text_decoration_color -> Some Equal
  | Text_underline_offset, Text_underline_offset -> Some Equal
  | Text_decoration_skip, Text_decoration_skip -> Some Equal
  | Text_decoration_skip_self, Text_decoration_skip_self -> Some Equal
  | Text_decoration_skip_box, Text_decoration_skip_box -> Some Equal
  | Text_decoration_skip_inset, Text_decoration_skip_inset -> Some Equal
  | Text_decoration_skip_spaces, Text_decoration_skip_spaces -> Some Equal
  | Text_emphasis, Text_emphasis -> Some Equal
  | Text_emphasis_style, Text_emphasis_style -> Some Equal
  | Text_emphasis_color, Text_emphasis_color -> Some Equal
  | Text_emphasis_position, Text_emphasis_position -> Some Equal
  | Text_emphasis_skip, Text_emphasis_skip -> Some Equal
  | Text_orientation, Text_orientation -> Some Equal
  | Text_transform, Text_transform -> Some Equal
  | Letter_spacing, Letter_spacing -> Some Equal
  | List_style_type, List_style_type -> Some Equal
  | List_style_position, List_style_position -> Some Equal
  | List_style_image, List_style_image -> Some Equal
  | Display, Display -> Some Equal
  | Position, Position -> Some Equal
  | Visibility, Visibility -> Some Equal
  | Baseline_source, Baseline_source -> Some Equal
  | Alignment_baseline, Alignment_baseline -> Some Equal
  | Baseline_shift, Baseline_shift -> Some Equal
  | Flex_direction, Flex_direction -> Some Equal
  | Flex_wrap, Flex_wrap -> Some Equal
  | Flex_flow, Flex_flow -> Some Equal
  | Flex, Flex -> Some Equal
  | Flex_grow, Flex_grow -> Some Equal
  | Flex_shrink, Flex_shrink -> Some Equal
  | Flex_basis, Flex_basis -> Some Equal
  | Order, Order -> Some Equal
  | Align_items, Align_items -> Some Equal
  | Justify_content, Justify_content -> Some Equal
  | Justify_items, Justify_items -> Some Equal
  | Justify_self, Justify_self -> Some Equal
  | Align_content, Align_content -> Some Equal
  | Align_self, Align_self -> Some Equal
  | Place_content, Place_content -> Some Equal
  | Place_items, Place_items -> Some Equal
  | Place_self, Place_self -> Some Equal
  | Grid_template_columns, Grid_template_columns -> Some Equal
  | Grid_template_rows, Grid_template_rows -> Some Equal
  | Grid_template_areas, Grid_template_areas -> Some Equal
  | Grid_template, Grid_template -> Some Equal
  | Grid, Grid -> Some Equal
  | Grid_area, Grid_area -> Some Equal
  | Grid_auto_flow, Grid_auto_flow -> Some Equal
  | Grid_auto_columns, Grid_auto_columns -> Some Equal
  | Grid_auto_rows, Grid_auto_rows -> Some Equal
  | Grid_column, Grid_column -> Some Equal
  | Grid_row, Grid_row -> Some Equal
  | Grid_column_start, Grid_column_start -> Some Equal
  | Grid_column_end, Grid_column_end -> Some Equal
  | Grid_row_start, Grid_row_start -> Some Equal
  | Grid_row_end, Grid_row_end -> Some Equal
  | Border_width, Border_width -> Some Equal
  | Border_top_width, Border_top_width -> Some Equal
  | Border_right_width, Border_right_width -> Some Equal
  | Border_bottom_width, Border_bottom_width -> Some Equal
  | Border_left_width, Border_left_width -> Some Equal
  | Border_inline_start_width, Border_inline_start_width -> Some Equal
  | Border_inline_end_width, Border_inline_end_width -> Some Equal
  | Border_block_start_width, Border_block_start_width -> Some Equal
  | Border_block_end_width, Border_block_end_width -> Some Equal
  | Border_inline_width, Border_inline_width -> Some Equal
  | Border_block_width, Border_block_width -> Some Equal
  | Border_image, Border_image -> Some Equal
  | Border_image_source, Border_image_source -> Some Equal
  | Border_image_slice, Border_image_slice -> Some Equal
  | Border_image_repeat, Border_image_repeat -> Some Equal
  | Border_image_width, Border_image_width -> Some Equal
  | Border_image_outset, Border_image_outset -> Some Equal
  | Border_radius, Border_radius -> Some Equal
  | Border_top_left_radius, Border_top_left_radius -> Some Equal
  | Border_top_right_radius, Border_top_right_radius -> Some Equal
  | Border_bottom_left_radius, Border_bottom_left_radius -> Some Equal
  | Border_bottom_right_radius, Border_bottom_right_radius -> Some Equal
  | Border_top_color, Border_top_color -> Some Equal
  | Border_right_color, Border_right_color -> Some Equal
  | Border_bottom_color, Border_bottom_color -> Some Equal
  | Border_left_color, Border_left_color -> Some Equal
  | Border_inline_start_color, Border_inline_start_color -> Some Equal
  | Border_inline_end_color, Border_inline_end_color -> Some Equal
  | Border_block_start_color, Border_block_start_color -> Some Equal
  | Border_block_end_color, Border_block_end_color -> Some Equal
  | Border_inline_color, Border_inline_color -> Some Equal
  | Border_block_color, Border_block_color -> Some Equal
  | Border_inline_style, Border_inline_style -> Some Equal
  | Border_block_style, Border_block_style -> Some Equal
  | Border_start_start_radius, Border_start_start_radius -> Some Equal
  | Border_start_end_radius, Border_start_end_radius -> Some Equal
  | Border_end_start_radius, Border_end_start_radius -> Some Equal
  | Border_end_end_radius, Border_end_end_radius -> Some Equal
  | Opacity, Opacity -> Some Equal
  | Fill_opacity, Fill_opacity -> Some Equal
  | Stroke_opacity, Stroke_opacity -> Some Equal
  | Stop_opacity, Stop_opacity -> Some Equal
  | Flood_opacity, Flood_opacity -> Some Equal
  | Mix_blend_mode, Mix_blend_mode -> Some Equal
  | Transform, Transform -> Some Equal
  | Translate, Translate -> Some Equal
  | Cursor, Cursor -> Some Equal
  | Interactivity, Interactivity -> Some Equal
  | Caret_animation, Caret_animation -> Some Equal
  | Caret_shape, Caret_shape -> Some Equal
  | Caret, Caret -> Some Equal
  | Interest_delay, Interest_delay -> Some Equal
  | Interest_delay_start, Interest_delay_start -> Some Equal
  | Interest_delay_end, Interest_delay_end -> Some Equal
  | Nav_up, Nav_up -> Some Equal
  | Nav_right, Nav_right -> Some Equal
  | Nav_down, Nav_down -> Some Equal
  | Nav_left, Nav_left -> Some Equal
  | Table_layout, Table_layout -> Some Equal
  | Border_collapse, Border_collapse -> Some Equal
  | Border_spacing, Border_spacing -> Some Equal
  | User_select, User_select -> Some Equal
  | Pointer_events, Pointer_events -> Some Equal
  | Overflow, Overflow -> Some Equal
  | Inset, Inset -> Some Equal
  | Inset_inline, Inset_inline -> Some Equal
  | Inset_inline_start, Inset_inline_start -> Some Equal
  | Inset_inline_end, Inset_inline_end -> Some Equal
  | Inset_block, Inset_block -> Some Equal
  | Inset_block_start, Inset_block_start -> Some Equal
  | Inset_block_end, Inset_block_end -> Some Equal
  | Top, Top -> Some Equal
  | Right, Right -> Some Equal
  | Bottom, Bottom -> Some Equal
  | Left, Left -> Some Equal
  | Z_index, Z_index -> Some Equal
  | Outline, Outline -> Some Equal
  | Outline_style, Outline_style -> Some Equal
  | Outline_width, Outline_width -> Some Equal
  | Outline_color, Outline_color -> Some Equal
  | Outline_offset, Outline_offset -> Some Equal
  | Forced_color_adjust, Forced_color_adjust -> Some Equal
  | Scroll_snap_type, Scroll_snap_type -> Some Equal
  | White_space, White_space -> Some Equal
  | White_space_collapse, White_space_collapse -> Some Equal
  | Font_variant_alternates, Font_variant_alternates -> Some Equal
  | Font_variant, Font_variant -> Some Equal
  | Border, Border -> Some Equal
  | Border_block, Border_block -> Some Equal
  | Border_block_start, Border_block_start -> Some Equal
  | Border_block_end, Border_block_end -> Some Equal
  | Border_inline, Border_inline -> Some Equal
  | Border_inline_start, Border_inline_start -> Some Equal
  | Border_inline_end, Border_inline_end -> Some Equal
  | Background, Background -> Some Equal
  | Tab_size, Tab_size -> Some Equal
  | Zoom, Zoom -> Some Equal
  | Webkit_text_size_adjust, Webkit_text_size_adjust -> Some Equal
  | Font_feature_settings, Font_feature_settings -> Some Equal
  | Font_variation_settings, Font_variation_settings -> Some Equal
  | Webkit_tap_highlight_color, Webkit_tap_highlight_color -> Some Equal
  | Webkit_user_select, Webkit_user_select -> Some Equal
  | Moz_user_select, Moz_user_select -> Some Equal
  | Ms_user_select, Ms_user_select -> Some Equal
  | Webkit_text_decoration, Webkit_text_decoration -> Some Equal
  | Webkit_text_decoration_color, Webkit_text_decoration_color -> Some Equal
  | Webkit_text_fill_color, Webkit_text_fill_color -> Some Equal
  | Webkit_text_stroke_color, Webkit_text_stroke_color -> Some Equal
  | Webkit_text_stroke, Webkit_text_stroke -> Some Equal
  | Webkit_text_stroke_width, Webkit_text_stroke_width -> Some Equal
  | Text_indent, Text_indent -> Some Equal
  | List_style, List_style -> Some Equal
  | Font, Font -> Some Equal
  | Source, Source -> Some Equal
  | Webkit_appearance, Webkit_appearance -> Some Equal
  | Webkit_transform, Webkit_transform -> Some Equal
  | Moz_transform, Moz_transform -> Some Equal
  | Ms_transform, Ms_transform -> Some Equal
  | O_transform, O_transform -> Some Equal
  | Webkit_transition, Webkit_transition -> Some Equal
  | Webkit_transition_delay, Webkit_transition_delay -> Some Equal
  | Webkit_transition_duration, Webkit_transition_duration -> Some Equal
  | Webkit_transition_property, Webkit_transition_property -> Some Equal
  | Webkit_transition_timing_function, Webkit_transition_timing_function ->
      Some Equal
  | Webkit_animation, Webkit_animation -> Some Equal
  | Webkit_animation_delay, Webkit_animation_delay -> Some Equal
  | Webkit_animation_duration, Webkit_animation_duration -> Some Equal
  | Webkit_animation_direction, Webkit_animation_direction -> Some Equal
  | Webkit_animation_iteration_count, Webkit_animation_iteration_count ->
      Some Equal
  | Webkit_animation_name, Webkit_animation_name -> Some Equal
  | Webkit_animation_timing_function, Webkit_animation_timing_function ->
      Some Equal
  | Webkit_animation_fill_mode, Webkit_animation_fill_mode -> Some Equal
  | Webkit_animation_play_state, Webkit_animation_play_state -> Some Equal
  | Webkit_flex_direction, Webkit_flex_direction -> Some Equal
  | Webkit_flex_wrap, Webkit_flex_wrap -> Some Equal
  | Webkit_flex_flow, Webkit_flex_flow -> Some Equal
  | Webkit_justify_content, Webkit_justify_content -> Some Equal
  | Webkit_align_items, Webkit_align_items -> Some Equal
  | Webkit_align_content, Webkit_align_content -> Some Equal
  | Webkit_align_self, Webkit_align_self -> Some Equal
  | Webkit_border_radius, Webkit_border_radius -> Some Equal
  | Webkit_box_sizing, Webkit_box_sizing -> Some Equal
  | Moz_box_sizing, Moz_box_sizing -> Some Equal
  | Webkit_box_shadow, Webkit_box_shadow -> Some Equal
  | Webkit_background_size, Webkit_background_size -> Some Equal
  | Webkit_filter, Webkit_filter -> Some Equal
  | Moz_appearance, Moz_appearance -> Some Equal
  | Moz_animation, Moz_animation -> Some Equal
  | Moz_animation_delay, Moz_animation_delay -> Some Equal
  | Moz_animation_duration, Moz_animation_duration -> Some Equal
  | Moz_animation_direction, Moz_animation_direction -> Some Equal
  | Moz_animation_iteration_count, Moz_animation_iteration_count -> Some Equal
  | Moz_animation_name, Moz_animation_name -> Some Equal
  | Moz_animation_timing_function, Moz_animation_timing_function -> Some Equal
  | Moz_animation_fill_mode, Moz_animation_fill_mode -> Some Equal
  | Moz_animation_play_state, Moz_animation_play_state -> Some Equal
  | Moz_transition, Moz_transition -> Some Equal
  | Moz_transition_delay, Moz_transition_delay -> Some Equal
  | Moz_transition_duration, Moz_transition_duration -> Some Equal
  | Moz_transition_property, Moz_transition_property -> Some Equal
  | Moz_transition_timing_function, Moz_transition_timing_function -> Some Equal
  | Moz_border_radius, Moz_border_radius -> Some Equal
  | Moz_box_shadow, Moz_box_shadow -> Some Equal
  | Ms_filter, Ms_filter -> Some Equal
  | O_transition, O_transition -> Some Equal
  | Container_type, Container_type -> Some Equal
  | Container_name, Container_name -> Some Equal
  | Container, Container -> Some Equal
  | Anchor_name, Anchor_name -> Some Equal
  | Position_anchor, Position_anchor -> Some Equal
  | Position_try_fallbacks, Position_try_fallbacks -> Some Equal
  | Position_try_order, Position_try_order -> Some Equal
  | Position_try, Position_try -> Some Equal
  | Position_visibility, Position_visibility -> Some Equal
  | Position_area, Position_area -> Some Equal
  | Shape_outside, Shape_outside -> Some Equal
  | Shape_margin, Shape_margin -> Some Equal
  | Shape_image_threshold, Shape_image_threshold -> Some Equal
  | Overflow_clip_margin, Overflow_clip_margin -> Some Equal
  | Overflow_anchor, Overflow_anchor -> Some Equal
  | Scrollbar_width, Scrollbar_width -> Some Equal
  | Scrollbar_color, Scrollbar_color -> Some Equal
  | Scrollbar_gutter, Scrollbar_gutter -> Some Equal
  | Line_height_step, Line_height_step -> Some Equal
  | Font_palette, Font_palette -> Some Equal
  | Font_synthesis, Font_synthesis -> Some Equal
  | Text_wrap_mode, Text_wrap_mode -> Some Equal
  | Text_wrap_style, Text_wrap_style -> Some Equal
  | Text_box_trim, Text_box_trim -> Some Equal
  | Text_underline_position, Text_underline_position -> Some Equal
  | Text_box_edge, Text_box_edge -> Some Equal
  | Text_box, Text_box -> Some Equal
  | Inline_sizing, Inline_sizing -> Some Equal
  | Line_fit_edge, Line_fit_edge -> Some Equal
  | Interpolate_size, Interpolate_size -> Some Equal
  | Min_intrinsic_sizing, Min_intrinsic_sizing -> Some Equal
  | Ruby_align, Ruby_align -> Some Equal
  | Ruby_merge, Ruby_merge -> Some Equal
  | Ruby_overhang, Ruby_overhang -> Some Equal
  | Ruby_position, Ruby_position -> Some Equal
  | Glyph_orientation_vertical, Glyph_orientation_vertical -> Some Equal
  | Text_combine_upright, Text_combine_upright -> Some Equal
  | Animation_timeline, Animation_timeline -> Some Equal
  | Animation_range, Animation_range -> Some Equal
  | Animation_range_start, Animation_range_start -> Some Equal
  | Animation_range_end, Animation_range_end -> Some Equal
  | Scroll_timeline, Scroll_timeline -> Some Equal
  | Scroll_timeline_name, Scroll_timeline_name -> Some Equal
  | Scroll_timeline_axis, Scroll_timeline_axis -> Some Equal
  | View_transition_name, View_transition_name -> Some Equal
  | View_transition_class, View_transition_class -> Some Equal
  | Image_orientation, Image_orientation -> Some Equal
  | Image_rendering, Image_rendering -> Some Equal
  | Image_resolution, Image_resolution -> Some Equal
  | Contain_intrinsic_size, Contain_intrinsic_size -> Some Equal
  | Contain_intrinsic_width, Contain_intrinsic_width -> Some Equal
  | Contain_intrinsic_height, Contain_intrinsic_height -> Some Equal
  | Contain_intrinsic_block_size, Contain_intrinsic_block_size -> Some Equal
  | Contain_intrinsic_inline_size, Contain_intrinsic_inline_size -> Some Equal
  | Margin_trim, Margin_trim -> Some Equal
  | Offset_path, Offset_path -> Some Equal
  | Offset, Offset -> Some Equal
  | Offset_anchor, Offset_anchor -> Some Equal
  | Offset_position, Offset_position -> Some Equal
  | Offset_distance, Offset_distance -> Some Equal
  | Offset_rotate, Offset_rotate -> Some Equal
  | Font_size_adjust, Font_size_adjust -> Some Equal
  | Font_variant_emoji, Font_variant_emoji -> Some Equal
  | Text_spacing_trim, Text_spacing_trim -> Some Equal
  | Hyphenate_limit_chars, Hyphenate_limit_chars -> Some Equal
  | Initial_letter, Initial_letter -> Some Equal
  | Initial_letter_align, Initial_letter_align -> Some Equal
  | Initial_letter_wrap, Initial_letter_wrap -> Some Equal
  | Dominant_baseline, Dominant_baseline -> Some Equal
  | View_timeline_name, View_timeline_name -> Some Equal
  | View_timeline_axis, View_timeline_axis -> Some Equal
  | View_timeline_inset, View_timeline_inset -> Some Equal
  | View_timeline, View_timeline -> Some Equal
  | Timeline_scope, Timeline_scope -> Some Equal
  | Perspective, Perspective -> Some Equal
  | Perspective_origin, Perspective_origin -> Some Equal
  | Transform_style, Transform_style -> Some Equal
  | Backface_visibility, Backface_visibility -> Some Equal
  | Object_position, Object_position -> Some Equal
  | Rotate, Rotate -> Some Equal
  | Transition_duration, Transition_duration -> Some Equal
  | Transition_timing_function, Transition_timing_function -> Some Equal
  | Transition_delay, Transition_delay -> Some Equal
  | Transition_property, Transition_property -> Some Equal
  | Transition_behavior, Transition_behavior -> Some Equal
  | Overlay, Overlay -> Some Equal
  | Will_change, Will_change -> Some Equal
  | Contain, Contain -> Some Equal
  | Isolation, Isolation -> Some Equal
  | Break_before, Break_before -> Some Equal
  | Break_after, Break_after -> Some Equal
  | Break_inside, Break_inside -> Some Equal
  | Page_break_before, Page_break_before -> Some Equal
  | Page_break_after, Page_break_after -> Some Equal
  | Page_break_inside, Page_break_inside -> Some Equal
  | Page_size, Page_size -> Some Equal
  | Columns, Columns -> Some Equal
  | Column_width, Column_width -> Some Equal
  | Column_height, Column_height -> Some Equal
  | Column_wrap, Column_wrap -> Some Equal
  | Column_count, Column_count -> Some Equal
  | Column_rule, Column_rule -> Some Equal
  | Column_rule_color, Column_rule_color -> Some Equal
  | Column_rule_width, Column_rule_width -> Some Equal
  | Column_rule_style, Column_rule_style -> Some Equal
  | Column_span, Column_span -> Some Equal
  | Word_spacing, Word_spacing -> Some Equal
  | Background_attachment, Background_attachment -> Some Equal
  | Border_top, Border_top -> Some Equal
  | Border_right, Border_right -> Some Equal
  | Border_bottom, Border_bottom -> Some Equal
  | Border_left, Border_left -> Some Equal
  | Transform_origin, Transform_origin -> Some Equal
  | Transform_box, Transform_box -> Some Equal
  | Text_shadow, Text_shadow -> Some Equal
  | Clip_path, Clip_path -> Some Equal
  | Mask, Mask -> Some Equal
  | Mask_border, Mask_border -> Some Equal
  | Content_visibility, Content_visibility -> Some Equal
  | Filter, Filter -> Some Equal
  | Background_image, Background_image -> Some Equal
  | Background_origin, Background_origin -> Some Equal
  | Background_clip, Background_clip -> Some Equal
  | Webkit_background_clip, Webkit_background_clip -> Some Equal
  | Animation, Animation -> Some Equal
  | Aspect_ratio, Aspect_ratio -> Some Equal
  | Overflow_x, Overflow_x -> Some Equal
  | Overflow_y, Overflow_y -> Some Equal
  | Overflow_block, Overflow_block -> Some Equal
  | Overflow_inline, Overflow_inline -> Some Equal
  | Vertical_align, Vertical_align -> Some Equal
  | Font_family, Font_family -> Some Equal
  | Background_position, Background_position -> Some Equal
  | Background_position_x, Background_position_x -> Some Equal
  | Background_position_y, Background_position_y -> Some Equal
  | Webkit_mask_position_x, Webkit_mask_position_x -> Some Equal
  | Webkit_mask_position_y, Webkit_mask_position_y -> Some Equal
  | Background_repeat, Background_repeat -> Some Equal
  | Background_size, Background_size -> Some Equal
  | Webkit_font_smoothing, Webkit_font_smoothing -> Some Equal
  | Moz_osx_font_smoothing, Moz_osx_font_smoothing -> Some Equal
  | Webkit_line_clamp, Webkit_line_clamp -> Some Equal
  | Webkit_box_orient, Webkit_box_orient -> Some Equal
  | Moz_orient, Moz_orient -> Some Equal
  | Text_overflow, Text_overflow -> Some Equal
  | Text_wrap, Text_wrap -> Some Equal
  | Word_break, Word_break -> Some Equal
  | Overflow_wrap, Overflow_wrap -> Some Equal
  | Line_break, Line_break -> Some Equal
  | Hyphens, Hyphens -> Some Equal
  | Webkit_hyphens, Webkit_hyphens -> Some Equal
  | Font_stretch, Font_stretch -> Some Equal
  | Font_optical_sizing, Font_optical_sizing -> Some Equal
  | Font_kerning, Font_kerning -> Some Equal
  | Font_language_override, Font_language_override -> Some Equal
  | Font_synthesis_style, Font_synthesis_style -> Some Equal
  | Font_synthesis_weight, Font_synthesis_weight -> Some Equal
  | Font_synthesis_small_caps, Font_synthesis_small_caps -> Some Equal
  | Font_synthesis_position, Font_synthesis_position -> Some Equal
  | Font_variant_ligatures, Font_variant_ligatures -> Some Equal
  | Caps, Caps -> Some Equal
  | Numeric, Numeric -> Some Equal
  | Font_variant_position, Font_variant_position -> Some Equal
  | East_asian, East_asian -> Some Equal
  | Backdrop_filter, Backdrop_filter -> Some Equal
  | Webkit_backdrop_filter, Webkit_backdrop_filter -> Some Equal
  | Webkit_mask_image, Webkit_mask_image -> Some Equal
  | Webkit_mask_composite, Webkit_mask_composite -> Some Equal
  | Webkit_mask_source_type, Webkit_mask_source_type -> Some Equal
  | Webkit_mask_size, Webkit_mask_size -> Some Equal
  | Webkit_mask_position, Webkit_mask_position -> Some Equal
  | Webkit_mask_repeat, Webkit_mask_repeat -> Some Equal
  | Webkit_mask_clip, Webkit_mask_clip -> Some Equal
  | Webkit_mask_origin, Webkit_mask_origin -> Some Equal
  | Mask_image, Mask_image -> Some Equal
  | Mask_composite, Mask_composite -> Some Equal
  | Mask_mode, Mask_mode -> Some Equal
  | Mask_size, Mask_size -> Some Equal
  | Mask_position, Mask_position -> Some Equal
  | Mask_repeat, Mask_repeat -> Some Equal
  | Mask_clip, Mask_clip -> Some Equal
  | Mask_origin, Mask_origin -> Some Equal
  | Mask_type, Mask_type -> Some Equal
  | Scroll_snap_align, Scroll_snap_align -> Some Equal
  | Scroll_snap_stop, Scroll_snap_stop -> Some Equal
  | Scroll_behavior, Scroll_behavior -> Some Equal
  | Box_sizing, Box_sizing -> Some Equal
  | Field_sizing, Field_sizing -> Some Equal
  | Caption_side, Caption_side -> Some Equal
  | Resize, Resize -> Some Equal
  | Object_fit, Object_fit -> Some Equal
  | Object_view_box, Object_view_box -> Some Equal
  | Appearance, Appearance -> Some Equal
  | Color_scheme, Color_scheme -> Some Equal
  | Print_color_adjust, Print_color_adjust -> Some Equal
  | Webkit_print_color_adjust, Webkit_print_color_adjust -> Some Equal
  | Box_decoration_break, Box_decoration_break -> Some Equal
  | Webkit_box_decoration_break, Webkit_box_decoration_break -> Some Equal
  | Content, Content -> Some Equal
  | Counter_reset, Counter_reset -> Some Equal
  | Counter_increment, Counter_increment -> Some Equal
  | Quotes, Quotes -> Some Equal
  | Text_decoration_thickness, Text_decoration_thickness -> Some Equal
  | Text_size_adjust, Text_size_adjust -> Some Equal
  | Touch_action, Touch_action -> Some Equal
  | Clip, Clip -> Some Equal
  | Clear, Clear -> Some Equal
  | Float, Float -> Some Equal
  | Scale, Scale -> Some Equal
  | Transition, Transition -> Some Equal
  | Box_shadow, Box_shadow -> Some Equal
  | Fill, Fill -> Some Equal
  | Stroke, Stroke -> Some Equal
  | Stroke_width, Stroke_width -> Some Equal
  | Fill_rule, Fill_rule -> Some Equal
  | Clip_rule, Clip_rule -> Some Equal
  | Stroke_linecap, Stroke_linecap -> Some Equal
  | Stroke_linejoin, Stroke_linejoin -> Some Equal
  | Stroke_miterlimit, Stroke_miterlimit -> Some Equal
  | Stroke_dashoffset, Stroke_dashoffset -> Some Equal
  | Stroke_dasharray, Stroke_dasharray -> Some Equal
  | Paint_order, Paint_order -> Some Equal
  | Vector_effect, Vector_effect -> Some Equal
  | Stop_color, Stop_color -> Some Equal
  | Flood_color, Flood_color -> Some Equal
  | Lighting_color, Lighting_color -> Some Equal
  | Direction, Direction -> Some Equal
  | Unicode_bidi, Unicode_bidi -> Some Equal
  | Writing_mode, Writing_mode -> Some Equal
  | Text_decoration_skip_ink, Text_decoration_skip_ink -> Some Equal
  | Animation_name, Animation_name -> Some Equal
  | Animation_duration, Animation_duration -> Some Equal
  | Animation_timing_function, Animation_timing_function -> Some Equal
  | Animation_delay, Animation_delay -> Some Equal
  | Animation_iteration_count, Animation_iteration_count -> Some Equal
  | Animation_direction, Animation_direction -> Some Equal
  | Animation_fill_mode, Animation_fill_mode -> Some Equal
  | Animation_play_state, Animation_play_state -> Some Equal
  | Animation_composition, Animation_composition -> Some Equal
  | Background_blend_mode, Background_blend_mode -> Some Equal
  | Scroll_margin, Scroll_margin -> Some Equal
  | Scroll_margin_top, Scroll_margin_top -> Some Equal
  | Scroll_margin_right, Scroll_margin_right -> Some Equal
  | Scroll_margin_bottom, Scroll_margin_bottom -> Some Equal
  | Scroll_margin_left, Scroll_margin_left -> Some Equal
  | Scroll_margin_inline, Scroll_margin_inline -> Some Equal
  | Scroll_margin_inline_start, Scroll_margin_inline_start -> Some Equal
  | Scroll_margin_inline_end, Scroll_margin_inline_end -> Some Equal
  | Scroll_margin_block, Scroll_margin_block -> Some Equal
  | Scroll_margin_block_start, Scroll_margin_block_start -> Some Equal
  | Scroll_margin_block_end, Scroll_margin_block_end -> Some Equal
  | Scroll_padding, Scroll_padding -> Some Equal
  | Scroll_padding_top, Scroll_padding_top -> Some Equal
  | Scroll_padding_right, Scroll_padding_right -> Some Equal
  | Scroll_padding_bottom, Scroll_padding_bottom -> Some Equal
  | Scroll_padding_left, Scroll_padding_left -> Some Equal
  | Scroll_padding_inline, Scroll_padding_inline -> Some Equal
  | Scroll_padding_inline_start, Scroll_padding_inline_start -> Some Equal
  | Scroll_padding_inline_end, Scroll_padding_inline_end -> Some Equal
  | Scroll_padding_block, Scroll_padding_block -> Some Equal
  | Scroll_padding_block_start, Scroll_padding_block_start -> Some Equal
  | Scroll_padding_block_end, Scroll_padding_block_end -> Some Equal
  | Overscroll_behavior, Overscroll_behavior -> Some Equal
  | Overscroll_behavior_x, Overscroll_behavior_x -> Some Equal
  | Overscroll_behavior_y, Overscroll_behavior_y -> Some Equal
  | Overscroll_behavior_block, Overscroll_behavior_block -> Some Equal
  | Overscroll_behavior_inline, Overscroll_behavior_inline -> Some Equal
  | Accent_color, Accent_color -> Some Equal
  | Caret_color, Caret_color -> Some Equal
  | _ -> None
(* PROPERTY_EQ_END *)
