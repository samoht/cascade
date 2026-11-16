open Context_intf
open Context_core
open Context_length

(** {2 Media-feature value comparison (CSS Media Queries 4 sec. 3)}

    [query.media_features] stores typed [Media.feature]s parsed at construction;
    this module is just the numeric comparison glue used when matching range
    queries against those typed values. *)

module Media_value = struct
  let to_number (v : Media.value) : float option =
    match v with
    | Length l -> Length.media_to_px l
    | Integer i -> Some (float_of_int i)
    | Number n -> Some n
    | Ratio (a, b) when b <> 0 -> Some (float_of_int a /. float_of_int b)
    | Ratio _ -> None
    | Resolution_value (n, unit) -> (
        match String.lowercase_ascii unit with
        | "dppx" | "x" -> Some n
        | "dpi" -> Some (n /. 96.)
        | "dpcm" -> Some (n *. 2.54 /. 96.)
        | _ -> None)
    | Ident _ | Function _ -> None

  let cmp_op : Media.cmp -> float -> float -> bool = function
    | Lt -> ( < )
    | Le -> ( <= )
    | Eq -> ( = )
    | Gt -> ( > )
    | Ge -> ( >= )

  (* Compare two media values. [None] when the comparison cannot be decided
     (mixed kinds, missing length context). *)
  let compare_with op (lhs : Media.value) (rhs : Media.value) : bool option =
    match (lhs, rhs) with
    | Ident a, Ident b ->
        Some (op = Media.Eq && Media.string_of_ident a = Media.string_of_ident b)
    | _ -> (
        match (to_number lhs, to_number rhs) with
        | Some la, Some lb -> Some (cmp_op op la lb)
        | _ -> None)
end

(** {2 [@supports] evaluation (CSS Conditional 3 sec. 6)}

    Each [Supports.t] constructor has a dedicated evaluator. The structural
    cases ([Not]/[And]/[Or]) recurse into [eval]; the leaves ([Property]/[Func])
    consult the explicit declaration/function tables. *)

module Match_supports = struct
  (* Leaves match against [q.supports] via the typed [Supports.equal]. *)
  let rec eval q : Supports.t -> bool = function
    | (Property _ | Function _ | General_enclosed _) as leaf ->
        List.exists (Supports.equal leaf) q.supports
    | Not c -> not (eval q c)
    | And (a, b) -> eval q a && eval q b
    | Or (a, b) -> eval q a || eval q b
end

(** {2 [@media] evaluation (CSS Media Queries 4)}

    Each [Media] AST type has its own evaluator: [eval_feature] for
    [Media.feature], [eval_condition] for [Media.condition], [eval_query] for
    [Media.query], [eval_medium] for [Media.medium], and [eval] for the
    typed-shorthand wrapper [Media.t]. Lengths resolve against a 16px base per
    sec. 1.3; [min-]/[max-] feature prefixes desugar to range comparisons. *)

module Match_media = struct
  open Media

  type feature_table = Media.t list
  type truth = True | False | Unknown

  let of_bool = function true -> True | false -> False
  let of_option = function Some value -> of_bool value | None -> Unknown
  let negate = function True -> False | False -> True | Unknown -> Unknown

  let conjunction a b =
    match (a, b) with
    | False, _ | _, False -> False
    | True, True -> True
    | Unknown, _ | _, Unknown -> Unknown

  let disjunction a b =
    match (a, b) with
    | True, _ | _, True -> True
    | False, False -> False
    | Unknown, _ | _, Unknown -> Unknown

  let strip_min_max = function
    | Media.Min name -> Some (`Min, name)
    | Media.Max name -> Some (`Max, name)
    | _ -> None

  let lookup_value (table : feature_table) feature_name : Media.value option =
    List.find_map (media_feature_value feature_name) table

  let boolean_value : Media.value -> bool option = function
    | Ident (None | No_preference) -> Some false
    | Ident _ -> Some true
    | value -> Option.map (fun n -> n <> 0.) (Media_value.to_number value)

  let eval_feature q : Media.feature -> truth =
    let with_lookup name f =
      if List.exists (Media.equal_name name) q.media_inapplicable then False
      else
        match lookup_value q.media_features (Media.string_of_name name) with
        | None -> Unknown
        | Some actual -> of_option (f actual)
    in
    function
    (* MQ4 section 3.1: preserve unknown until the whole query is evaluated. *)
    | General_enclosed _ -> Unknown
    | Boolean name -> with_lookup name boolean_value
    | Plain (name, value) -> (
        match strip_min_max name with
        | Some (`Min, base) ->
            with_lookup base (fun a -> Media_value.compare_with Ge a value)
        | Some (`Max, base) ->
            with_lookup base (fun a -> Media_value.compare_with Le a value)
        | None ->
            with_lookup name (fun a -> Media_value.compare_with Eq a value))
    | Range (name, op, value) ->
        with_lookup name (fun a -> Media_value.compare_with op a value)
    | Range_rev (value, op, name) ->
        with_lookup name (fun a -> Media_value.compare_with op value a)
    | Interval (lo, lo_op, name, hi_op, hi) ->
        with_lookup name (fun a ->
            match
              ( Media_value.compare_with lo_op lo a,
                Media_value.compare_with hi_op a hi )
            with
            | Some x, Some y -> Some (x && y)
            | _ -> None)

  let eval_medium q : Media.medium -> bool = function
    | All -> true
    | Screen -> q.media_type = Some "screen"
    | Print -> q.media_type = Some "print"
    | Other s -> q.media_type = Some s

  let rec eval_condition q : Media.condition -> truth = function
    | Feature f -> eval_feature q f
    | Not c -> negate (eval_condition q c)
    | And (a, b) -> conjunction (eval_condition q a) (eval_condition q b)
    | Or (a, b) -> disjunction (eval_condition q a) (eval_condition q b)

  let rec eval_query q : Media.t -> truth = function
    | Cond c -> eval_condition q c
    | Type { prefix; type_; trailing } -> (
        let body =
          conjunction
            (of_bool (eval_medium q type_))
            (Option.fold ~none:True ~some:(eval_condition q) trailing)
        in
        match prefix with Some Media.Not -> negate body | _ -> body)
    | List [] -> True
    | List qs ->
        List.fold_left
          (fun acc query -> disjunction acc (eval_query q query))
          False qs

  let eval q query = eval_query q query = True
end

(** {2 Container-query evaluation (CSS Conditional 5 sec. 5.4)}

    Container queries reuse the media-feature evaluator applied to the container
    feature table; they additionally guard on the container name (when supplied)
    and accept [style()] / [scroll-state()] queries that the media evaluator
    does not know about. *)

module Match_container = struct
  let name_matches ?name q =
    match (name, q.container_name) with
    | None, _ -> true
    | Some n, Some actual -> String.equal n actual
    | Some _, None -> false

  (* Project [container_features] entries that look like size/range features
     into [Media.t]s so the [Match_media] evaluator can range-compare them.
     [Style _] / [Scroll_state _] are handled directly. *)
  let media_features_of q =
    List.filter_map
      (function Container.Feature_query f -> Some f | _ -> None)
      q.container_features

  let style_value = function
    | Container.Boolean name -> (name, None)
    | Container.Declaration { name; value } ->
        (name, Some (Cursor.string_of_components ~trim:true value))
    | Container.Range _ | Container.All _ | Container.Any _ | Container.Neg _ ->
        ("", None)

  let style_leaf_match q ~query =
    let prop, value = style_value query in
    List.exists
      (function
        | Container.Style { query = Range _ | All _ | Any _ | Neg _; _ } ->
            false
        | Container.Style { query; _ }
          when String.equal (fst (style_value query)) prop -> (
            match (value, snd (style_value query)) with
            | None, _ -> true (* any style(prop) match: present in any form *)
            | Some _, None -> false
            | Some asked, Some actual ->
                String.equal (String.trim asked) (String.trim actual))
        | _ -> false)
      q.container_features

  let rec style_match q ~query =
    match query with
    | Container.Boolean _ | Container.Declaration _ | Container.Range _ ->
        style_leaf_match q ~query
    | Container.All (a, b) -> style_match q ~query:a && style_match q ~query:b
    | Container.Any (a, b) -> style_match q ~query:a || style_match q ~query:b
    | Container.Neg query -> not (style_match q ~query)

  let rec eval_scroll_state_query q = function
    | Container.State { name = prop; value } ->
        List.exists
          (function
            | Container.Scroll_state
                { query = State { name = p; value = v }; _ } ->
                String.equal p prop && String.equal v value
            | _ -> false)
          q.container_features
    | Both (a, b) -> eval_scroll_state_query q a && eval_scroll_state_query q b
    | Either (a, b) ->
        eval_scroll_state_query q a || eval_scroll_state_query q b
    | Negated query -> not (eval_scroll_state_query q query)

  let eval_scroll_state q ~query =
    List.exists
      (function
        | Container.Scroll_state { query = actual; _ } ->
            Container.compare_scroll_state_query actual query = 0
        | _ -> false)
      q.container_features
    || eval_scroll_state_query q query

  let rec feature_available table = function
    | Media.Min name | Media.Max name -> feature_available table name
    | name ->
        Option.is_some
          (Match_media.lookup_value table (Media.string_of_name name))

  let rec media_available table : Media.condition -> bool = function
    | Feature (General_enclosed _) -> false
    | Feature
        ( Plain (name, _)
        | Boolean name
        | Range (name, _, _)
        | Range_rev (_, _, name)
        | Interval (_, _, name, _, _) ) ->
        feature_available table name
    | Not condition -> media_available table condition
    | And (a, b) | Or (a, b) ->
        media_available table a && media_available table b

  (* Conditional Rules 5 section 5.4: all features must select the same eligible
     container, even when an [or] branch would already be true. *)
  let rec eligible q table : Container.t -> bool = function
    | Min_width_rem _ | Min_width_px _ -> feature_available table Media.Width
    | Feature_query (Media.Cond condition) -> media_available table condition
    | Feature_query (Media.Type _ | Media.List _) -> false
    | Named (name, condition) ->
        name_matches ~name q && eligible q table condition
    | And (a, b) | Or (a, b) -> eligible q table a && eligible q table b
    | Not condition -> eligible q table condition
    | Style _ -> true
    | Scroll_state _ ->
        List.exists
          (function Container.Scroll_state _ -> true | _ -> false)
          q.container_features

  let rec eval_condition q : Container.t -> Match_media.truth =
    let media_q =
      { q with media_features = media_features_of q; media_inapplicable = [] }
    in
    fun cond ->
      let min_width l =
        Media.Cond
          (Media.Feature (Media.Plain (Media.Min Media.Width, Media.Length l)))
      in
      match cond with
      | Min_width_rem rem ->
          Match_media.eval_query media_q (min_width (Values.Rem rem))
      | Min_width_px px ->
          Match_media.eval_query media_q
            (min_width (Values.Px (float_of_int px)))
      | Named (_, inner) -> eval_condition q inner
      | Style { query; _ } -> Match_media.of_bool (style_match q ~query)
      | Scroll_state { query; _ } ->
          Match_media.of_bool (eval_scroll_state q ~query)
      | And (a, b) ->
          Match_media.conjunction (eval_condition q a) (eval_condition q b)
      | Or (a, b) ->
          Match_media.disjunction (eval_condition q a) (eval_condition q b)
      | Not c -> Match_media.negate (eval_condition q c)
      | Feature_query media -> Match_media.eval_query media_q media

  let eval q ?name condition =
    name_matches ?name q
    && eligible q (media_features_of q) condition
    && eval_condition q condition = Match_media.True
end

(** {2 Selector matching (CSS Selectors 4)}

    Browsers match selectors right-to-left because the rightmost compound fixes
    the candidate element. Without a tree the matcher reduces to that rightmost
    subject; combinators always succeed against the document so callers should
    only request selector matching when they have already decided which element
    they are testing. *)

module Match_selector = struct
  let attr_name : Selector.attr_name -> string = function
    | Aria a -> Aria.to_string a
    | Data s -> "data-" ^ s
    | Regular s -> s

  let value_match (matcher : Selector.attribute_match) ~flag actual =
    let normalize =
      match flag with
      | Some Selector.Insensitive -> String.lowercase_ascii
      | _ -> Fun.id
    in
    let actual = normalize actual in
    let words s =
      String.split_on_char ' ' s |> List.filter (fun s -> s <> "")
    in
    match matcher with
    | Presence -> true
    | Exact v | Exact_quoted (v, _) -> String.equal actual (normalize v)
    | Whitespace_list v | Whitespace_list_quoted (v, _) ->
        let v = normalize v in
        List.exists (String.equal v) (words actual)
    | Hyphen_list v | Hyphen_list_quoted (v, _) ->
        let v = normalize v in
        String.equal actual v
        || String.length actual > String.length v
           && String.sub actual 0 (String.length v) = v
           && actual.[String.length v] = '-'
    | Prefix v | Prefix_quoted (v, _) ->
        let v = normalize v in
        String.length actual >= String.length v
        && String.sub actual 0 (String.length v) = v
    | Suffix v | Suffix_quoted (v, _) ->
        let v = normalize v in
        let la = String.length actual and lv = String.length v in
        la >= lv && String.sub actual (la - lv) lv = v
    | Substring v | Substring_quoted (v, _) ->
        let v = normalize v in
        let la = String.length actual and lv = String.length v in
        let rec scan i =
          if i + lv > la then false
          else if String.sub actual i lv = v then true
          else scan (i + 1)
        in
        lv = 0 || scan 0

  let rec eval (doc : document) (sel : Selector.t) : bool =
    match sel with
    | Universal _ -> true
    | Element (_, name) -> doc.element = Some name
    | Class name -> List.exists (String.equal name) doc.classes
    | Id name -> List.exists (String.equal name) doc.ids
    | Attribute (_, name, matcher, flag) -> (
        match List.assoc_opt (attr_name name) doc.attributes with
        | None -> false
        | Some None -> matcher = Presence
        | Some (Some v) -> value_match matcher ~flag v)
    | Compound parts -> List.for_all (eval doc) parts
    | List alts -> List.exists (eval doc) alts
    | Is alts | Where alts -> List.exists (eval doc) alts
    | Not alts -> not (List.exists (eval doc) alts)
    | Combined (_, _, right) ->
        (* Combinators need a tree we do not have; collapse to the rightmost
           subject so the matcher answers questions about the [doc] element. *)
        eval doc right
    | Scope -> Option.is_some doc.scope
    | Root -> Option.is_some doc.root
    | Nesting -> true
    | Hover -> List.mem "hover" doc.pseudo_classes
    | Active -> List.mem "active" doc.pseudo_classes
    | Focus -> List.mem "focus" doc.pseudo_classes
    | Focus_visible -> List.mem "focus-visible" doc.pseudo_classes
    | Focus_within -> List.mem "focus-within" doc.pseudo_classes
    | Target -> List.mem "target" doc.pseudo_classes
    | Link -> List.mem "link" doc.pseudo_classes
    | Visited -> List.mem "visited" doc.pseudo_classes
    | Any_link -> List.mem "any-link" doc.pseudo_classes
    | Empty -> List.mem "empty" doc.pseudo_classes
    | First_child -> List.mem "first-child" doc.pseudo_classes
    | Last_child -> List.mem "last-child" doc.pseudo_classes
    | Only_child -> List.mem "only-child" doc.pseudo_classes
    | First_of_type -> List.mem "first-of-type" doc.pseudo_classes
    | Last_of_type -> List.mem "last-of-type" doc.pseudo_classes
    | Only_of_type -> List.mem "only-of-type" doc.pseudo_classes
    | Enabled -> List.mem "enabled" doc.pseudo_classes
    | Disabled -> List.mem "disabled" doc.pseudo_classes
    | Read_only -> List.mem "read-only" doc.pseudo_classes
    | Read_write -> List.mem "read-write" doc.pseudo_classes
    | Placeholder_shown -> List.mem "placeholder-shown" doc.pseudo_classes
    | Default -> List.mem "default" doc.pseudo_classes
    | Checked -> List.mem "checked" doc.pseudo_classes
    | Indeterminate -> List.mem "indeterminate" doc.pseudo_classes
    | Valid -> List.mem "valid" doc.pseudo_classes
    | Invalid -> List.mem "invalid" doc.pseudo_classes
    | In_range -> List.mem "in-range" doc.pseudo_classes
    | Out_of_range -> List.mem "out-of-range" doc.pseudo_classes
    | Required -> List.mem "required" doc.pseudo_classes
    | Optional -> List.mem "optional" doc.pseudo_classes
    | Open -> List.mem "open" doc.pseudo_classes
    | Popover_open -> List.mem "popover-open" doc.pseudo_classes
    | Before _ -> List.mem "before" doc.pseudo_elements
    | After _ -> List.mem "after" doc.pseudo_elements
    | First_letter _ -> List.mem "first-letter" doc.pseudo_elements
    | First_line _ -> List.mem "first-line" doc.pseudo_elements
    | Backdrop -> List.mem "backdrop" doc.pseudo_elements
    | Marker -> List.mem "marker" doc.pseudo_elements
    | Placeholder -> List.mem "placeholder" doc.pseudo_elements
    | Selection -> List.mem "selection" doc.pseudo_elements
    | _ -> false
end

(** {2 URL resolution (RFC 3986)} *)

module Url = struct
  (* A drive or UNC path names a file whatever the directory of a filesystem
     base. [Uri] reads [1:/x] as scheme [1], where WHATWG URL sec. 4.4 reads a
     relative path, so a [./] keeps its first segment a path. *)
  let resolve loader href =
    let local_base =
      match loader.base_url with
      | None -> true
      | Some base -> not (Syntax.is_remote_url base)
    in
    match (Syntax.local_absolute_path href, Syntax.url_scheme href) with
    | Some path, _ when local_base -> Ok path
    | _, Some _ -> Ok (Uri.to_string (Uri.of_string href))
    | _, None -> (
        match loader.base_url with
        | None -> Error ("no base URL to resolve " ^ href)
        | Some base ->
            let reference =
              match Uri.scheme (Uri.of_string href) with
              | Some _ -> Uri.of_string (String.concat "" [ "./"; href ])
              | None -> Uri.of_string href
            in
            Ok (Uri.resolve "" (Uri.of_string base) reference |> Uri.to_string))
end

(** {2 [@import] loader (CSS Cascade 5 sec. 2)}

    Resolves the import URL through {!Url}, looks up the body in
    [loader.imports], parses it, and applies any media/supports/layer guards on
    the rule. *)

module Import = struct
  let layer_known ~layer_order = function
    | None -> true
    | Some name -> List.mem (Stylesheet.string_of_layer_name name) layer_order

  let supports_ok ?query rule =
    match ((rule : Stylesheet.import_rule).supports, query) with
    | None, _ -> true
    | Some _, None -> false
    | Some cond, Some q -> Match_supports.eval q cond

  let media_ok ?query rule =
    match ((rule : Stylesheet.import_rule).media, query) with
    | None, _ -> true
    | Some _, None -> false
    | Some media, Some q -> Match_media.eval q media

  let wrap_in_layer rule statements =
    match (rule : Stylesheet.import_rule).layer with
    | None -> statements
    | Some name -> [ Stylesheet.Layer (Some name, statements) ]

  let parse_source rule source =
    try
      let cursor = Cursor.of_string source in
      let sheet = Stylesheet.read cursor in
      Ok (wrap_in_layer rule sheet)
    with
    | Failure msg -> Error msg
    | Cursor.Parse_error _ -> Error "stylesheet parse error"

  let load_resolved loader rule resolved =
    match List.assoc_opt resolved loader.imports with
    | None -> Error ("import not in loader table: " ^ resolved)
    | Some source -> parse_source rule source

  let guards_ok ?query ~layer_order rule =
    let layer_name = (rule : Stylesheet.import_rule).layer in
    if not (layer_known ~layer_order layer_name) then
      Error
        ("unknown layer "
        ^ Option.fold ~none:"" ~some:Stylesheet.string_of_layer_name layer_name
        )
    else if not (supports_ok ?query rule) then
      Error "supports() guard rejected the import"
    else if not (media_ok ?query rule) then
      Error "media guard rejected the import"
    else Ok ()

  let load ?query ?(layer_order = []) loader (rule : Stylesheet.import_rule) =
    match guards_ok ?query ~layer_order rule with
    | Error _ as e -> e
    | Ok () -> (
        match Url.resolve loader rule.url with
        | Error _ as e -> e
        | Ok resolved -> load_resolved loader rule resolved)
end
