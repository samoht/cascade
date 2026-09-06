(* Mutation testing for cascade: how much of the library can be broken without a
   test noticing.

   The tool rewrites one token of one source file, rebuilds, runs the suite and
   asks whether the result moved. A mutant the suite still accepts names a gap
   in the suite directly, rather than by inference from a coverage percentage.

   It never writes to the checkout it is built from. [--work] names a separate
   checkout (a [git worktree] of the same commit is the cheapest way to get one)
   and every edit lands there. The originals of every target file are read once
   at startup and written back on exit, on an exception and on a signal, so an
   interrupted run leaves that checkout as it found it. A run killed past its
   handlers cannot be cleaned up from inside it, so the next run refuses a
   [--work] whose target files differ from the index rather than reading a
   surviving mutant as the original.

   Mutants are enumerated from the source, not from a list of interesting edits:
   every position where an operator applies is a site, in file order and then
   byte order. The numbering is a position in that enumeration, so it holds for
   a given tree and a given [--file] set and means nothing across either.
   [--sample] draws from it with [--seed], and [--only] replays one site.

   The verdict is relative, because a suite with a known failure is still a
   useful oracle. The baseline run is captured once and a mutant counts as
   killed when its set of failing rules and test cases differs from the
   baseline's, not when the suite is red. That set is the granularity: a rule
   failing at baseline is opaque, and a mutant that only changes what it prints
   scores SURVIVED. The baseline is listed on every run for that reason.

   [--list] enumerates the sites and stops, [--sample N] with [--seed S] draws a
   campaign, and [--only ID] replays one site. scripts/dune spells the
   invocations out.

   Not wired to any alias: a full campaign is minutes per mutant and belongs to
   whoever asked for it. *)

let default_files =
  [
    "lib/shorthand.ml";
    "lib/optimize.ml";
    "lib/rule_candidate.ml";
    "lib/rule_order.ml";
  ]

(* Skipped when set, so the browser-backed oracles under test/render cannot make
   a verdict depend on a headless Chromium: they are slow, and a browser that
   fails to start would move every mutant's result at once. *)
let no_browser = "CASCADE_NO_BROWSER"

let ident_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false
let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let write_file path s =
  let oc = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> output_string oc s)

let matches src i s =
  let l = String.length s in
  i + l <= String.length src
  &&
  let rec go k =
    k >= l || (String.get src (i + k) = String.get s k && go (k + 1))
  in
  go 0

let contains hay needle =
  let n = String.length hay and m = String.length needle in
  let rec go i = i + m <= n && (matches hay i needle || go (i + 1)) in
  m = 0 || go 0

(* Lexical mask *)

let skip_string src i =
  let n = String.length src in
  let rec go j =
    if j >= n then n
    else
      match String.get src j with
      | '\\' -> go (j + 2)
      | '"' -> j + 1
      | _ -> go (j + 1)
  in
  go (i + 1)

(* [i] is at '{'. The delimiter of a {id|...|id} string when this opens one. *)
let quoted_open src i =
  let n = String.length src in
  let rec go j =
    if j >= n then None
    else
      match String.get src j with
      | '|' -> Some (String.sub src (i + 1) (j - i - 1))
      | 'a' .. 'z' | '_' -> go (j + 1)
      | _ -> None
  in
  go (i + 1)

let skip_quoted src i delim =
  let n = String.length src in
  let close = String.concat "" [ "|"; delim; "}" ] in
  let cl = String.length close in
  let rec go j =
    if j + cl > n then n else if matches src j close then j + cl else go (j + 1)
  in
  go (i + String.length delim + 2)

(* A ' opens a character literal only when a closing ' follows it; otherwise it
   introduces a type variable, which is code. *)
let char_literal_end src i =
  let n = String.length src in
  if
    i + 2 < n && String.get src (i + 1) <> '\\' && String.get src (i + 2) = '\''
  then Some (i + 3)
  else if i + 1 < n && String.get src (i + 1) = '\\' then
    let rec go j =
      if j >= n || j > i + 8 then None
      else if String.get src j = '\'' then Some (j + 1)
      else go (j + 1)
    in
    go (i + 2)
  else None

let code = '\001'
let text = '\000'

(* Marks every byte that is OCaml code rather than comment, string or character
   literal content, so an operator cannot rewrite fixture CSS or prose. *)
let code_mask src =
  let n = String.length src in
  let mask = Bytes.make n code in
  let blank i j = Bytes.fill mask i (j - i) text in
  let i = ref 0 and depth = ref 0 in
  while !i < n do
    let next = if !i + 1 < n then String.get src (!i + 1) else '\000' in
    match String.get src !i with
    | '(' when next = '*' ->
        blank !i (!i + 2);
        incr depth;
        i := !i + 2
    | '*' when next = ')' && !depth > 0 ->
        blank !i (!i + 2);
        decr depth;
        i := !i + 2
    | '"' ->
        let j = skip_string src !i in
        blank !i j;
        i := j
    | '{' when !depth = 0 -> (
        match quoted_open src !i with
        | Some d ->
            let j = skip_quoted src !i d in
            blank !i j;
            i := j
        | None -> incr i)
    | '\'' when !depth = 0 -> (
        match char_literal_end src !i with
        | Some j ->
            blank !i j;
            i := j
        | None -> incr i)
    | _ ->
        if !depth > 0 then blank !i (!i + 1);
        incr i
  done;
  mask

(* Operators *)

(* [Sym] wants whitespace on both sides, which is what keeps [->] out of the
   arithmetic operators and [||] out of an empty array literal. [Ident] wants a
   non-identifier byte on both sides. *)
type kind = Sym | Ident
type rule = { op : string; kind : kind; before : string; after : string }

let rule op kind before after = { op; kind; before; after }

let rules =
  [
    rule "bool" Ident "true" "false";
    rule "bool" Ident "false" "true";
    rule "logic" Sym "&&" "||";
    rule "logic" Sym "||" "&&";
    rule "cmp" Sym "<=" "<";
    rule "cmp" Sym ">=" ">";
    rule "cmp" Sym "<>" "=";
    rule "cmp" Sym "<" "<=";
    rule "cmp" Sym ">" ">=";
    rule "arith" Sym "+" "-";
    rule "arith" Sym "-" "+";
    rule "neg" Ident "not" "";
    rule "opt" Ident "Option.is_some" "Option.is_none";
    rule "opt" Ident "Option.is_none" "Option.is_some";
    rule "list" Ident "List.exists" "List.for_all";
    rule "list" Ident "List.for_all" "List.exists";
    rule "minmax" Ident "min" "max";
    rule "minmax" Ident "max" "min";
  ]

let sym_ok src i len =
  let n = String.length src in
  i > 0
  && is_space (String.get src (i - 1))
  && i + len < n
  && is_space (String.get src (i + len))

let ident_ok src i len =
  let n = String.length src in
  (i = 0 || not (ident_char (String.get src (i - 1))))
  && (i + len >= n || not (ident_char (String.get src (i + len))))

let rule_site src i r =
  let l = String.length r.before in
  if not (matches src i r.before) then None
  else
    let ok =
      match r.kind with Sym -> sym_ok src i l | Ident -> ident_ok src i l
    in
    if ok then Some (l, r.op, r.before, r.after) else None

(* A decimal integer literal, replaced by its successor. The neighbouring bytes
   rule out a float, a 0x/0b/0o body, a 1_000 group and an Int32/Int64 suffix,
   all of which change type or meaning rather than value. *)
let int_site src i =
  let n = String.length src in
  if not (is_digit (String.get src i)) then None
  else if
    i > 0
    && (ident_char (String.get src (i - 1)) || String.get src (i - 1) = '.')
  then None
  else
    let j = ref i in
    while !j < n && is_digit (String.get src !j) do
      incr j
    done;
    if !j < n && (ident_char (String.get src !j) || String.get src !j = '.')
    then None
    else
      let t = String.sub src i (!j - i) in
      if String.length t > 15 then None
      else Some (String.length t, "int", t, Int.to_string (int_of_string t + 1))

let word_at src i w = matches src i w && ident_ok src i (String.length w)

(* The [then] that closes the [if] starting just before [i], skipping the ones
   belonging to a nested conditional and to any bracketed subexpression. *)
let closing_then src mask i =
  let n = String.length src in
  let depth = ref 0
  and pending = ref 0
  and res = ref None
  and j = ref i
  and stop = ref false in
  while (not !stop) && !j < n do
    if Bytes.get mask !j = text then incr j
    else
      match String.get src !j with
      | '(' | '[' | '{' ->
          incr depth;
          incr j
      | ')' | ']' | '}' ->
          decr depth;
          if !depth < 0 then stop := true else incr j
      | _ when !depth = 0 && word_at src !j "if" ->
          incr pending;
          j := !j + 2
      | _ when !depth = 0 && word_at src !j "then" ->
          if !pending > 0 then (
            decr pending;
            j := !j + 4)
          else (
            res := Some !j;
            stop := true)
      | _ -> incr j
  done;
  !res

(* The condition of an [if], so it can be forced to one branch. *)
let cond_span src mask i =
  if not (word_at src i "if") then None
  else
    match closing_then src mask (i + 2) with
    | None -> None
    | Some t ->
        let s = ref (i + 2) and e = ref t in
        while !s < t && is_space (String.get src !s) do
          incr s
        done;
        while !e > !s && is_space (String.get src (!e - 1)) do
          decr e
        done;
        if !e <= !s then None else Some (!s, !e - !s)

(* Sites *)

type site = {
  id : int;
  file : string;
  line : int;
  offset : int;
  len : int;
  op : string;
  before : string;
  after : string;
}

let source_line src offset =
  let n = String.length src in
  let s = ref offset and e = ref offset in
  while !s > 0 && String.get src (!s - 1) <> '\n' do
    decr s
  done;
  while !e < n && String.get src !e <> '\n' do
    incr e
  done;
  String.trim (String.sub src !s (!e - !s))

let add acc file line offset (len, op, before, after) =
  if String.equal before after then ()
  else acc := { id = 0; file; line; offset; len; op; before; after } :: !acc

let cond_sites src mask i acc file line =
  match cond_span src mask i with
  | None -> ()
  | Some (s, l) ->
      let cond = String.sub src s l in
      List.iter
        (fun v -> add acc file line s (l, "cond", cond, v))
        [ "true"; "false" ]

let sites_in_file file src =
  let mask = code_mask src in
  let acc = ref [] in
  let line = ref 1 in
  for i = 0 to String.length src - 1 do
    if String.get src i = '\n' then incr line;
    if Bytes.get mask i = code then (
      List.iter
        (fun r -> Option.iter (add acc file !line i) (rule_site src i r))
        rules;
      Option.iter (add acc file !line i) (int_site src i);
      cond_sites src mask i acc file !line)
  done;
  List.rev !acc

let number_sites all = List.mapi (fun id s -> { s with id }) all

(* Running the gate *)

type outcome = Exited of int | Timed_out

let wait_with_timeout pid timeout =
  let deadline = Unix.gettimeofday () +. float_of_int timeout in
  let termed = ref false in
  let rec go () =
    match Unix.waitpid [ Unix.WNOHANG ] pid with
    | 0, _ ->
        if Unix.gettimeofday () > deadline then (
          (* SIGTERM first: dune tears its own children down on it, and a
             SIGKILL to the parent alone would leave a compiler behind. *)
          let sign = if !termed then Sys.sigkill else Sys.sigterm in
          (try Unix.kill pid sign with Unix.Unix_error _ -> ());
          if not !termed then (
            termed := true;
            Unix.sleepf 5.);
          Unix.sleepf 0.2;
          go ())
        else (
          Unix.sleepf 0.2;
          go ())
    | _, Unix.WEXITED c -> if !termed then Timed_out else Exited c
    | _, _ -> Timed_out
  in
  go ()

let run ~timeout ~log argv =
  let fd =
    Unix.openfile log [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644
  in
  let pid =
    Fun.protect
      ~finally:(fun () -> Unix.close fd)
      (fun () -> Unix.create_process (Array.get argv 0) argv Unix.stdin fd fd)
  in
  wait_with_timeout pid timeout

let log_path () =
  Filename.concat (Filename.get_temp_dir_name ()) "cascade-mutate.log"

let dune ~work ~jobs args =
  let tail = [ "--root"; work; "-j"; Int.to_string jobs ] in
  Array.of_list
    (("sh" :: Filename.concat work "scripts/with_switch.sh" :: "dune" :: args)
    @ tail)

let build_argv ~work ~jobs =
  dune ~work ~jobs [ "build"; "--cache=disabled"; "@check"; "@default" ]

let test_argv ~work ~jobs =
  dune ~work ~jobs
    [ "runtest"; "--cache=disabled"; "--error-reporting=deterministic" ]

(* The failing rules and test cases, as a sorted set. Everything else dune
   prints carries a run id, a timing or a temporary path, none of which say
   anything about the mutant. *)
let signature log =
  let ic = open_in log in
  let acc = ref [] in
  (try
     while true do
       let l = String.trim (input_line ic) in
       if String.starts_with ~prefix:"File \"" l || contains l "[FAIL]" then
         acc := l :: !acc
     done
   with End_of_file -> ());
  close_in ic;
  List.sort_uniq String.compare !acc

(* Campaign *)

type verdict = Killed of string list | Survived | No_compile | Timeout

let verdict_name = function
  | Killed _ -> "KILLED"
  | Survived -> "SURVIVED"
  | No_compile -> "nocompile"
  | Timeout -> "TIMEOUT"

let apply work site sources =
  let src = List.assoc site.file sources in
  let b = Buffer.create (String.length src) in
  Buffer.add_string b (String.sub src 0 site.offset);
  Buffer.add_string b site.after;
  let tail = site.offset + site.len in
  Buffer.add_string b (String.sub src tail (String.length src - tail));
  write_file (Filename.concat work site.file) (Buffer.contents b)

let restore work sources =
  List.iter (fun (f, src) -> write_file (Filename.concat work f) src) sources

let judge ~work ~jobs ~timeout ~log ~baseline =
  match run ~timeout ~log (build_argv ~work ~jobs) with
  | Timed_out -> Timeout
  | Exited c when c <> 0 -> No_compile
  | Exited _ -> (
      match run ~timeout ~log (test_argv ~work ~jobs) with
      | Timed_out -> Timeout
      | Exited _ ->
          let s = signature log in
          if List.equal String.equal s baseline then Survived
          else
            let added = List.filter (fun l -> not (List.mem l baseline)) s in
            let gone = List.filter (fun l -> not (List.mem l s)) baseline in
            Killed
              (added @ List.map (fun l -> String.concat "" [ "gone: "; l ]) gone)
      )

let run_site ~work ~jobs ~timeout ~log ~baseline sources site =
  apply work site sources;
  Fun.protect
    ~finally:(fun () -> restore work sources)
    (fun () -> judge ~work ~jobs ~timeout ~log ~baseline)

(* Reporting *)

let show_site s src =
  Fmt.pr "#%-5d %s:%d %-7s %S -> %S@." s.id s.file s.line s.op s.before s.after;
  Fmt.pr "        | %s@." (source_line src s.offset)

let show_result s v =
  Fmt.pr "#%-5d %s:%d %-7s %S -> %S : %s@." s.id s.file s.line s.op s.before
    s.after (verdict_name v);
  match v with Killed (l :: _) -> Fmt.pr "        by %s@." l | _ -> ()

let percent k n = if n = 0 then 0. else 100. *. float_of_int k /. float_of_int n

(* A mutant that did not compile says nothing about the suite, and neither does
   one whose run did not finish, so both stay out of the score. *)
let summary results =
  let count p = List.length (List.filter (fun (_, v) -> p v) results) in
  let killed = count (function Killed _ -> true | _ -> false) in
  let survived = count (function Survived -> true | _ -> false) in
  let nocompile = count (function No_compile -> true | _ -> false) in
  let timeout = count (function Timeout -> true | _ -> false) in
  Fmt.pr "@.run %d: killed %d, survived %d, nocompile %d, timeout %d@."
    (List.length results) killed survived nocompile timeout;
  Fmt.pr
    "mutation score %.1f%% (killed / (killed + survived); nocompile and \
     timeout excluded)@."
    (percent killed (killed + survived));
  Fmt.pr "scored %.1f%% of the sample@."
    (percent (killed + survived) (List.length results))

let show_survivors results sources =
  let alive = function Survived -> true | _ -> false in
  let survivors = List.filter (fun (_, v) -> alive v) results in
  if not (List.is_empty survivors) then (
    Fmt.pr "@.survivors:@.";
    List.iter (fun (s, _) -> show_site s (List.assoc s.file sources)) survivors)

(* Selection *)

let shuffle seed a =
  let st = Random.State.make [| seed |] in
  for i = Array.length a - 1 downto 1 do
    let j = Random.State.int st (i + 1) in
    let t = Array.get a i in
    Array.set a i (Array.get a j);
    Array.set a j t
  done

let sample ~seed ~n all =
  let a = Array.of_list all in
  shuffle seed a;
  let n = min n (Array.length a) in
  let picked = Array.to_list (Array.sub a 0 n) in
  List.sort (fun x y -> Int.compare x.id y.id) picked

(* Entry point *)

let usage () =
  Fmt.pr
    "usage: mutate --work DIR [--file PATH]... [--list] [--only ID,ID...]@.";
  Fmt.pr "              [--sample N] [--seed S] [--timeout SECS] [--jobs N]@.";
  exit 2

type conf = {
  mutable work : string;
  mutable files : string list;
  mutable list_only : bool;
  mutable only : int list;
  mutable n : int;
  mutable seed : int;
  mutable timeout : int;
  mutable jobs : int;
}

let parse_args () =
  let c =
    {
      work = "";
      files = [];
      list_only = false;
      only = [];
      n = 20;
      seed = 1;
      timeout = 900;
      jobs = 8;
    }
  in
  let rec go = function
    | [] -> ()
    | "--work" :: v :: r ->
        c.work <- v;
        go r
    | "--file" :: v :: r ->
        c.files <- c.files @ [ v ];
        go r
    | "--list" :: r ->
        c.list_only <- true;
        go r
    | "--only" :: v :: r ->
        c.only <- c.only @ List.map int_of_string (String.split_on_char ',' v);
        go r
    | "--sample" :: v :: r ->
        c.n <- int_of_string v;
        go r
    | "--seed" :: v :: r ->
        c.seed <- int_of_string v;
        go r
    | "--timeout" :: v :: r ->
        c.timeout <- int_of_string v;
        go r
    | "--jobs" :: v :: r ->
        c.jobs <- int_of_string v;
        go r
    | _ -> usage ()
  in
  go (List.tl (Array.to_list Sys.argv));
  if String.equal c.work "" then usage ();
  if List.is_empty c.files then c.files <- default_files;
  c

(* SIGKILL and a lost machine run no handler, so a mutant can outlive the run
   that wrote it. Reading it in as an original would write it back on every run
   after that, so refuse instead. Also refuses a [--work] that is not a git
   checkout, where nothing can tell the two apart. *)
let check_clean c =
  let argv =
    Array.of_list ([ "git"; "-C"; c.work; "diff"; "--quiet"; "--" ] @ c.files)
  in
  match run ~timeout:120 ~log:(log_path ()) argv with
  | Exited 0 -> ()
  | _ ->
      Fmt.pr "error: %s is not a clean git checkout of the target files@."
        c.work;
      Fmt.pr
        "  an interrupted run may have left a mutant behind; restore with@.";
      Fmt.pr "  git -C %s checkout -- %s@." c.work (String.concat " " c.files);
      exit 2

let load c =
  if not (Sys.file_exists (Filename.concat c.work "dune-project")) then (
    Fmt.pr "error: --work %s is not a checkout@." c.work;
    exit 2);
  check_clean c;
  List.map (fun f -> (f, read_file (Filename.concat c.work f))) c.files

let all_sites sources =
  number_sites (List.concat_map (fun (f, src) -> sites_in_file f src) sources)

let per_file sites =
  List.iter
    (fun f ->
      let n =
        List.length (List.filter (fun s -> String.equal s.file f) sites)
      in
      Fmt.pr "  %-28s %5d sites@." f n)
    (List.sort_uniq String.compare (List.map (fun s -> s.file) sites))

let capture_baseline ~work ~jobs ~timeout ~log =
  Fmt.pr "capturing baseline...@.";
  (match run ~timeout ~log (build_argv ~work ~jobs) with
  | Exited 0 -> ()
  | _ ->
      Fmt.pr "error: the unmutated tree does not build@.";
      exit 1);
  ignore (run ~timeout ~log (test_argv ~work ~jobs));
  let b = signature log in
  Fmt.pr "baseline: %d failing rules or cases@." (List.length b);
  List.iter (fun l -> Fmt.pr "  %s@." l) b;
  if not (List.is_empty b) then
    Fmt.pr
      "warning: those already fail, so a mutant that only changes what they \
       print scores SURVIVED@.";
  b

let campaign c sources sites =
  let log = log_path () in
  let baseline =
    capture_baseline ~work:c.work ~jobs:c.jobs ~timeout:c.timeout ~log
  in
  let chosen =
    if List.is_empty c.only then sample ~seed:c.seed ~n:c.n sites
    else List.filter (fun s -> List.mem s.id c.only) sites
  in
  Fmt.pr "@.running %d mutants (seed %d)@." (List.length chosen) c.seed;
  let results =
    List.map
      (fun s ->
        let v =
          run_site ~work:c.work ~jobs:c.jobs ~timeout:c.timeout ~log ~baseline
            sources s
        in
        show_result s v;
        (s, v))
      chosen
  in
  summary results;
  show_survivors results sources

let () =
  let c = parse_args () in
  let sources = load c in
  at_exit (fun () -> restore c.work sources);
  List.iter
    (fun s -> Sys.set_signal s (Sys.Signal_handle (fun _ -> exit 130)))
    [ Sys.sigint; Sys.sigterm ];
  Unix.putenv no_browser "1";
  let sites = all_sites sources in
  Fmt.pr "%d sites@." (List.length sites);
  per_file sites;
  if c.list_only then
    List.iter (fun s -> show_site s (List.assoc s.file sources)) sites
  else campaign c sources sites
