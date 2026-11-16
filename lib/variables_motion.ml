open Properties
open Variables_intf
open Variables_value
open Variables_visual

let vars_of_rotate_value (value : Properties.rotate_value) : any_var list =
  match value with
  | Angle a | X a | Y a | Z a -> vars_of_angle a
  | Axis (_, _, _, a) -> vars_of_angle a
  | Var v -> [ V v ]
  | None | Inherit | Initial | Unset | Revert | Revert_layer -> []

let vars_of_transform (value : Properties.transform) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_transform_list (value : Properties.transform list) : any_var list =
  List.concat_map vars_of_transform value

let vars_of_scale (value : Properties.scale) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_translate_value (value : Properties.translate_value) : any_var list
    =
  match value with
  | Var v -> [ V v ]
  | X len -> vars_of_length len
  | XY (len1, len2) -> vars_of_length len1 @ vars_of_length len2
  | XYZ (len1, len2, len3) ->
      vars_of_length len1 @ vars_of_length len2 @ vars_of_length len3
  | None | Inherit | Initial | Unset | Revert | Revert_layer -> []

let vars_of_animation (value : Properties.animation) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_transition_property_value
    (value : Properties.transition_property_value) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_transition_property_list (value : Properties.transition_property) :
    any_var list =
  List.concat_map vars_of_transition_property_value value

let vars_of_transition (value : Properties.transition) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let rec vars_of_animation_name (value : Properties.animation_name) :
    any_var list =
  match value with
  | Var v -> [ V v ]
  | Names names -> List.concat_map vars_of_animation_name names
  | _ -> []

let vars_of_transform_origin (value : Properties.transform_origin) :
    any_var list =
  match value with
  | Var v -> [ V v ]
  | Position position -> vars_of_position_value position
  | X l -> vars_of_length l
  | XY (l1, l2) -> vars_of_length l1 @ vars_of_length l2
  | XYZ (l1, l2, l3) ->
      vars_of_length l1 @ vars_of_length l2 @ vars_of_length l3
  | Position_z (position, z) ->
      vars_of_position_value position @ vars_of_length z
  | _ -> []

let vars_of_will_change (value : Properties.will_change) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let rec vars_of_animation_timeline (value : Properties.animation_timeline) =
  match value with
  | Var v -> [ V v ]
  | Timelines timelines -> List.concat_map vars_of_animation_timeline timelines
  | _ -> []

let rec vars_of_animation_range_item (value : Properties.animation_range_item) =
  match value with
  | Var v -> [ V v ]
  | Normal -> []
  | Items items -> List.concat_map vars_of_animation_range_item items
  | Offset lp | Named (_, Some lp) -> vars_of_length_percentage lp
  | Named (_, None) -> []
  | Initial | Inherit | Unset | Revert | Revert_layer -> []

let rec vars_of_animation_range (value : Properties.animation_range) =
  match value with
  | Var v -> [ V v ]
  | Ranges ranges -> List.concat_map vars_of_animation_range ranges
  | Range (first, second) ->
      vars_of_animation_range_item first
      @ Option.fold ~none:[] ~some:vars_of_animation_range_item second
  | Initial | Inherit | Unset | Revert | Revert_layer -> []

let vars_of_view_transition_name (value : Properties.view_transition_name) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_view_transition_class (value : Properties.view_transition_class) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_ray (value : Properties.ray) =
  vars_of_angle value.angle
  @ Option.fold ~none:[] ~some:vars_of_position_value value.position

let vars_of_offset_path (value : Properties.offset_path) =
  match value with
  | Var v -> [ V v ]
  | Ray ray -> vars_of_ray ray
  | Shape shape -> vars_of_clip_path shape
  | None | Url _ | Path _ | Initial | Inherit | Unset | Revert | Revert_layer ->
      []

let vars_of_offset_anchor (value : Properties.offset_anchor) =
  match value with
  | Var var -> [ V var ]
  | Position position -> vars_of_position_value position
  | Auto | Initial | Inherit | Unset | Revert | Revert_layer -> []

let vars_of_offset_position (value : Properties.offset_position) =
  match value with
  | Var var -> [ V var ]
  | Position position -> vars_of_position_value position
  | Normal | Auto | Initial | Inherit | Unset | Revert | Revert_layer -> []

let vars_of_offset_rotate (value : Properties.offset_rotate) =
  match value with
  | Var v -> [ V v ]
  | Angle angle | With_angle (_, angle) -> vars_of_angle angle
  | Auto | Reverse | Initial | Inherit | Unset | Revert | Revert_layer -> []

let vars_of_offset_target (value : Properties.offset_target) =
  match value with
  | Position_only position -> vars_of_offset_position position
  | With_path { position; path; distance; rotate } ->
      List.concat
        [
          (match position with
          | Some position -> vars_of_offset_position position
          | None -> []);
          vars_of_offset_path path;
          (match distance with
          | Some distance -> vars_of_length_percentage distance
          | None -> []);
          (match rotate with
          | Some rotate -> vars_of_offset_rotate rotate
          | None -> []);
        ]

let vars_of_offset (value : Properties.offset) =
  match value with
  | Var var -> [ V var ]
  | Shorthand { target; anchor } ->
      List.append
        (vars_of_offset_target target)
        (match anchor with
        | Some anchor -> vars_of_offset_anchor anchor
        | None -> [])
  | Initial | Inherit | Unset | Revert | Revert_layer -> []

let vars_of_transform_style (value : Properties.transform_style) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_transform_box (value : Properties.transform_box) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_backface_visibility (value : Properties.backface_visibility) =
  match value with Var v -> [ V v ] | _ -> []

let rec vars_of_animation_direction (value : Properties.animation_direction) =
  match value with
  | Var v -> [ V v ]
  | Directions values -> List.concat_map vars_of_animation_direction values
  | _ -> []

let rec vars_of_animation_fill_mode (value : Properties.animation_fill_mode) =
  match value with
  | Var v -> [ V v ]
  | Fill_modes values -> List.concat_map vars_of_animation_fill_mode values
  | _ -> []

let rec vars_of_animation_play_state (value : Properties.animation_play_state) =
  match value with
  | Var v -> [ V v ]
  | States states -> List.concat_map vars_of_animation_play_state states
  | _ -> []

let rec vars_of_animation_iteration_count
    (value : Properties.animation_iteration_count) =
  match value with
  | Var v -> [ V v ]
  | Counts counts -> List.concat_map vars_of_animation_iteration_count counts
  | Count n -> vars_of_number_value n
  | _ -> []

let rec vars_of_transition_behavior (value : Properties.transition_behavior) =
  match value with
  | Var v -> [ V v ]
  | Behaviors l -> List.concat_map vars_of_transition_behavior l
  | _ -> []

let vars_of_perspective_origin (value : Properties.perspective_origin) =
  vars_of_position_value value

let vars_of_timeline_axis (value : Properties.timeline_axis) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_timeline_scope (value : Properties.timeline_scope) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_timeline_name (value : Properties.timeline_name) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_timeline_shorthand (value : Properties.timeline_shorthand) =
  match value with
  | Var v -> [ V v ]
  | (Timelines items : Properties.timeline_shorthand) ->
      List.concat_map
        (fun ({ Properties.axis; _ } : Properties.timeline_shorthand_item) ->
          Option.fold ~none:[] ~some:vars_of_timeline_axis axis)
        items
  | _ -> []

let vars_of_timeline_inset_item (value : Properties.timeline_inset_item) =
  match value with Auto -> [] | Length lp -> vars_of_length_percentage lp

let rec vars_of_timeline_inset (value : Properties.timeline_inset) =
  match value with
  | Var v -> [ V v ]
  | Inset (first, second) ->
      vars_of_timeline_inset_item first
      @ Option.value ~default:[] (Option.map vars_of_timeline_inset_item second)
  | Insets insets -> List.concat_map vars_of_timeline_inset insets
  | Initial | Inherit | Unset | Revert | Revert_layer -> []

let vars_of_view_timeline_shorthand (value : Properties.view_timeline_shorthand)
    =
  match value with
  | Var v -> [ V v ]
  | Timelines items ->
      List.concat_map
        (fun { Properties.axis; inset; _ } ->
          Option.fold ~none:[] ~some:vars_of_timeline_axis axis
          @ Option.fold ~none:[] ~some:vars_of_timeline_inset inset)
        items
  | _ -> []
