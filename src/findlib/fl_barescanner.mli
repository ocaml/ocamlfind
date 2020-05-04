(** Extract information from bare packages *)

(** A bare package doesn't have a META file. In the directory there is
    at least one archive with the fixed names "lib.cma", "lib.cmxa", or
    "lib.cmxs". These archive files contain further information about
    the package, in particular the list of required packages.
 *)

type bare_definition =
  { bare_name : string;                  (** dot-separated name *)
    bare_mainname : string;              (** main package name *)
    bare_subname : string;               (** name without main name *)
    bare_directory : string;             (** directory of package *)
    bare_byte_archive : string option;   (** cma path if any *)
    bare_byte_requires : string list;    (** dot-separated names *)
    bare_native_archive : string option; (** cmxa path if any *)
    bare_native_requires : string list;  (** dot-separated names *)
    bare_shared_archive : string option; (** cmxs path if any *)
    bare_shared_requires : string list;  (** dot-separated names *)
    bare_children : bare_definition list; (** sub packages *)
  }

val is_bare_pkg : string -> bool
  (** whether the given directory is a bare package *)

val scan_bare_files : string -> string -> (string -> in_channel option) -> bare_definition
  (** [scan_bare_files name dir open_archive]: pre-fills the definition
      from the archives. The archives (e.g. "lib.cma") are opened by
      calling [open_archive]. If this function returns a channel,
      the file is scanned. If it returns [None], the archive is assumed
      to be non-existing.
   *)

val scan_bare_pkg : string -> string -> bare_definition
  (** [scan_bare_pkg name dir]: scans the given directory [dir] for
      bare archives. The name of this package is assumed as [name]
      (a dot-separated path). The subdirectories are recursively
      scanned, too (unless ignore_chdilren).
   *)

val check_bare_pkg : bare_definition -> unit
  (** Peforms some checks, and fails if the check are not succeeding. *)

val to_pkg_expr : bare_definition -> Fl_metascanner.pkg_expr
  (** Converts the bare definition into an expression, as if a META
      file with the same information was present.
   *)

val to_pkg_definition : bare_definition -> Fl_metascanner.pkg_definition list
  (** Converts the bare definition into a pkg definition, as if a META
      file with the same information was present.
   *)
