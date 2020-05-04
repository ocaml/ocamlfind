open Fl_metascanner
open Fl_barescanner
open Fl_package_base
open Printf

type incompat_reasons =
  | Incompat_not_declared_lean
  | Incompat_bad_lean_decl
  | Incompat_uses_directory
  | Incompat_uses_linkopts
  | Incompat_bad_archive of string * string
  | Incompat_bad_plugin of string * string
  | Incompat_bad_requires of string
  | Incompat_inline_subpackage

let token_if p token =
  if p then [token] else []

let check_incompat_with_lean_meta name open_archive meta =
  (* List of checks:
     - must set "lean=true"
     - no "directory" variable
     - no "linkopts" variable
     - "archive" must match bare archives
     - "requires" must match bare requires
   *)
  let has_bad_lean_decl =
    List.exists
      (fun def ->
        def.def_var = "lean" &&
          (def.def_flav <> `BaseDef || def.def_preds <> [] ||
             def.def_value <> "true")
      )
      meta.pkg_defs in
  if has_bad_lean_decl then
    [ Incompat_bad_lean_decl ]
  else
    let is_declared_lean =
      List.exists
        (fun def ->
          def.def_var = "lean" && def.def_flav = `BaseDef &&
            def.def_preds = [] && def.def_value = "true"
        )
        meta.pkg_defs in
    if is_declared_lean then (
      let uses_directory =
        List.exists (fun def -> def.def_var = "directory") meta.pkg_defs in
      let uses_linkopts =
        List.exists (fun def -> def.def_var = "linkopts") meta.pkg_defs in
      let archive_byte =
        try Some(lookup "archive" ["byte"] meta.pkg_defs)
        with Not_found -> None in
      let archive_byte_ok =
        archive_byte = None || archive_byte = Some "lib.cma" in
      let archive_byte_plugin =
        try Some(lookup "plugin" ["byte"] meta.pkg_defs)
        with Not_found -> None in
      let archive_byte_plugin_ok =
        archive_byte_plugin = None || archive_byte = Some "lib.cma" in
      let archive_native =
        try Some(lookup "archive" ["native"] meta.pkg_defs)
        with Not_found -> None in
      let archive_native_ok =
        archive_native = None || archive_native = Some "lib.cmxa" in
      let archive_native_plugin =
        try Some(lookup "plugin" ["native"] meta.pkg_defs)
        with Not_found -> None in
      let archive_native_plugin_ok =
        archive_native_plugin = None || archive_native_plugin = Some "lib.cmxs" in
      let uses_inline_subpkg =
        meta.pkg_children <> [] in
      let reasons_1 =
        [ token_if uses_directory Incompat_uses_directory;
          token_if uses_linkopts Incompat_uses_linkopts;
          token_if uses_inline_subpkg Incompat_inline_subpackage;
          token_if (not archive_byte_ok) (Incompat_bad_archive("byte","not named lib.cma"));
          token_if (not archive_byte_plugin_ok) (Incompat_bad_archive("byte,plugin","not named lib.cma"));
          token_if (not archive_native_ok) (Incompat_bad_archive("native","not named lib.cmxa"));
          token_if (not archive_native_plugin_ok) (Incompat_bad_archive("native,plugin","not named lib.cmxs"));
        ] |> List.flatten in
      if reasons_1 = [] then (
        let bare = scan_bare_files name "" open_archive in
        let reqs =
          try
            lookup "requires" [] meta.pkg_defs
            |> Fl_split.in_words
            |> List.sort String.compare
          with Not_found -> [] in
        let reqs_ok l =
          reqs = List.sort String.compare l in
        [ token_if
            (archive_byte <> None && bare.bare_byte_archive = None)
            (Incompat_bad_archive("byte","missing file lib.cma"));
          token_if
            (archive_byte = None && bare.bare_byte_archive <> None)
            (Incompat_bad_archive("byte","missing entry: lib.cma"));
          token_if
            (archive_byte <> bare.bare_byte_archive)
            (Incompat_bad_archive("byte","not named lib.cma"));
          token_if
            (archive_byte_plugin <> None && bare.bare_byte_archive = None)
            (Incompat_bad_plugin("byte","missing file lib.cma"));
          token_if
            (archive_byte_plugin = None && bare.bare_byte_archive <> None)
            (Incompat_bad_plugin("byte","missing entry: lib.cma"));
          token_if
            (archive_byte_plugin <> bare.bare_byte_archive)
            (Incompat_bad_plugin("byte","not named lib.cma"));
          token_if
            (archive_native <> None && bare.bare_native_archive = None)
            (Incompat_bad_archive("native","missing file lib.cmxa"));
          token_if
            (archive_native = None && bare.bare_native_archive <> None)
            (Incompat_bad_archive("native","missing entry: lib.cmxa"));
          token_if
            (archive_native <> bare.bare_native_archive)
            (Incompat_bad_archive("native","not named lib.cmxa"));
          token_if
            (archive_native_plugin <> None && bare.bare_shared_archive = None)
            (Incompat_bad_plugin("native","missing file lib.cmxs"));
          token_if
            (archive_native_plugin = None && bare.bare_shared_archive <> None)
            (Incompat_bad_plugin("native","missing entry: lib.cmxs"));
          token_if
            (archive_native_plugin <> bare.bare_shared_archive)
            (Incompat_bad_archive("native","not named lib.cmxs"));
          token_if
            (bare.bare_byte_archive <> None && not(reqs_ok bare.bare_byte_requires))
            (Incompat_bad_requires "lib.cma");
          token_if
            (bare.bare_native_archive <> None && not(reqs_ok bare.bare_native_requires))
            (Incompat_bad_requires "lib.cmxa");
          token_if
            (bare.bare_shared_archive <> None && not(reqs_ok bare.bare_shared_requires))
            (Incompat_bad_requires "lib.cmxs");
        ] |> List.flatten
      ) else
        reasons_1
    ) else
      [ Incompat_not_declared_lean ]


let token_to_text token =
  match token with
    | Incompat_not_declared_lean ->
        " - The META file misses a 'lean = true' setting."
    | Incompat_bad_lean_decl ->
        " - The META file has a bad setting for the 'lean' variable."
    | Incompat_uses_directory ->
        " - The META file defines the 'directory' variable which is not permitted for lean libraries."
    | Incompat_uses_linkopts ->
        " - The META file defines the 'linkopts' variable which is not permitted for lean libraries."
    | Incompat_bad_archive(which,what) ->
        sprintf " - bad variable 'archive(%s)': %s" which what
    | Incompat_bad_plugin(which,what) ->
        sprintf " - bad variable 'plugin(%s)': %s" which what
    | Incompat_bad_requires file ->
        sprintf " - bad setting for the 'requires' variable which differs from what the file %s specifies." file
    | Incompat_inline_subpackage ->
        " - The META file contains inline subpackages, which is not supported for lean libraries. Subpackages need to go into subdirectories."

let incompat_to_text tokens =
  if tokens = [] then
    "This library is a fully-compatible lean library.\n"
  else
    "This library is not lean. The following incompatibilities have been detected:\n"
    ^ (String.concat "\n" (List.map token_to_text tokens))
    ^ "\n"

