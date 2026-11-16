(** Locating the node and the headless browser the render harnesses and the
    CLI's browser comparison drive, and reporting their absence the same way. *)

val getenv : string -> string option
(** [getenv name] is the value of the environment variable [name], and [None]
    when it is unset or empty. *)

val executable : string -> bool
(** [executable path] is [true] when [path] names a file this process may
    execute: on Windows, which has no execute bit, a file whose extension
    [PATHEXT] lists. *)

val chrome_binary : unit -> string option
(** [chrome_binary ()] is a headless-capable Chromium: [CHROME] when that names
    an executable, then [PATH], then the puppeteer and playwright caches, then
    the macOS application bundle. Each cache keeps one directory per version, so
    the largest path is the newest build. *)

val node_binary : unit -> string option
(** [node_binary ()] is node: [NODE] when that names an executable, [PATH]
    otherwise. *)

val run :
  ?env:(string * string) list ->
  ?stderr:string ->
  string ->
  string list ->
  string list * Unix.process_status
(** [run ?env ?stderr prog args] runs [prog] with the arguments [args], with no
    shell between, and is the lines it writes on standard output and how it
    exited. [env] sets variables over this process's environment. Standard error
    goes to the file [stderr], and to the null device when [stderr] is absent;
    standard input is the null device.

    @raise Unix.Unix_error when [prog] cannot be started. *)

val chrome_version : string -> (int * int) option
(** [chrome_version chrome] is the [(major, minor)] the binary reports. The
    support dataset is keyed by version, so a harness that looks a production up
    has to say which build it measured rather than assume the one the default
    contract names. *)

val skip : string -> string -> 'a
(** [skip harness reason] prints a skip line naming [harness] and exits with
    status 0, so a machine without a browser does not fail the suite. *)

val suppressed : string -> unit
(** [suppressed harness] exits when [CASCADE_NO_BROWSER] is set: status 0 for
    the one value that acknowledges the run checks nothing, and status 1 for any
    other, so silencing [harness] cannot read as a pass. *)
