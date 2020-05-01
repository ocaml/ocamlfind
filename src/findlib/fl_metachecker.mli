type incompat_reasons =
  | Incompat_not_declared_lean
  | Incompat_bad_lean_decl
  | Incompat_uses_directory
  | Incompat_uses_linkopts
  | Incompat_bad_archive of string * string
  | Incompat_bad_plugin of string * string
  | Incompat_bad_requires of string
  | Incompat_inline_subpackage

val check_incompat_with_lean_meta : string ->
                                    (string -> in_channel option) ->
                                    Fl_metascanner.pkg_expr ->
                                    incompat_reasons list
  (** [check_incompat_with_lean_meta name open_archive meta]: checks whether
      the lib with the META file [meta] can be regarded as a lean lib.
      If so, the empty list is returned. If not, the list describes
      the incompatibilities.

      [open_archive]: see {!Fl_barescanner.scan_bare_files}
   *)

val incompat_to_text : incompat_reasons list -> string
  (** Converts the tokens to a multi-line human-readable text *)
