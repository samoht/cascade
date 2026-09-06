(** Locating the node and the headless browser the harnesses in this directory
    drive, and reporting their absence the same way. *)

val getenv : string -> string option
(** [getenv name] is the value of the environment variable [name], and [None]
    when it is unset or empty. *)

val executable : string -> bool
(** [executable path] is [true] when [path] names a file this process may
    execute. *)

val chrome_binary : unit -> string option
(** [chrome_binary ()] is a headless-capable Chromium: [CHROME] when that names
    an executable, then [PATH], then the puppeteer and playwright caches, then
    the macOS application bundle. Each cache keeps one directory per version, so
    the largest path is the newest build. *)

val node_binary : unit -> string option
(** [node_binary ()] is node: [NODE] when that names an executable, [PATH]
    otherwise. *)

val skip : string -> string -> 'a
(** [skip harness reason] prints a skip line naming [harness] and exits with
    status 0, so a machine without a browser does not fail the suite. *)

val suppressed : string -> unit
(** [suppressed harness] exits when [CASCADE_NO_BROWSER] is set: status 0 for
    the one value that acknowledges the run checks nothing, and status 1 for any
    other, so silencing [harness] cannot read as a pass. *)
