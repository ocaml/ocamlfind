(** Extract information from bare packages *)

open Fl_metascanner

type bare_definition =
  { bare_name : string;
    bare_mainname : string;
    bare_subname : string;
    bare_directory : string;
    bare_byte_archive : string option;
    bare_byte_requires : string list;
    bare_native_archive : string option;
    bare_native_requires : string list;
    bare_shared_archive : string option;
    bare_shared_requires : string list;
    bare_children : bare_definition list;
  }

let is_bare_pkg dir =
  not (Sys.file_exists (Filename.concat dir "META")) &&
    ( Sys.file_exists (Filename.concat dir "lib.cma") ||
        Sys.file_exists (Filename.concat dir "lib.cmxa") ||
          Sys.file_exists (Filename.concat dir "lib.cmxs")
    )

(* TODO: also support:
    - older versions of ocaml: no package is bare
    - missing native compiler: Cmx_format is unavailable
 *)

let input_toc name exp_magic ic =
  let magic = really_input_string ic (String.length exp_magic) in
  if magic <> exp_magic then (
    close_in ic;
    failwith ("bad magic number: " ^ name)
  );
  let toc_pos = input_binary_int ic in
  seek_in ic toc_pos;
  let toc = input_value ic in
  close_in ic;
  toc

let scan_bare_files mainname dir open_archive =
  let name_cma = Filename.concat dir "lib.cma" in
  let name_cmxa = Filename.concat dir "lib.cmxa" in
  let chan_cma = open_archive "lib.cma" in
  let chan_cmxa = open_archive "lib.cmxa" in
  let req_cma =
    match chan_cma with
      | Some ic ->
          Cmo_format.((input_toc name_cma Config.cma_magic_number ic).lib_requires)
          |> List.map Lib.Name.to_string
      | None ->
          [] in
  let req_cmxa =
    match chan_cmxa with
      | Some ic ->
          Cmx_format.((input_toc name_cmxa Config.cma_magic_number ic).lib_requires)
          |> List.map Lib.Name.to_string
      | None ->
          [] in
  { bare_name = mainname;
    bare_mainname = mainname;
    bare_subname = "";
    bare_directory = "";
    bare_byte_archive = if chan_cma <> None then Some "lib.cma" else None;
    bare_byte_requires = req_cma;
    bare_native_archive = if chan_cmxa <> None then Some "lib.cmxa" else None;
    bare_native_requires = req_cmxa;
    bare_shared_archive = None;  (* TODO *)
    bare_shared_requires = [];   (* TODO *)
    bare_children = [];
  }

let scan_bare_pkg mainname dir =
  let sub_add subname n =
    if subname = "" then n else subname ^ "." ^ n in
  let open_archive dir file_name =
    let path = Filename.concat dir file_name in
    if Sys.file_exists path then
      Some(open_in_bin path)
    else
      None in
  let rec scan subname name dir =
    let bare = scan_bare_files name dir (open_archive dir) in
    let bare_children =
      (if Sys.is_directory dir then Sys.readdir dir else [| |])
      |> Array.to_list
      |> List.filter (fun n -> is_bare_pkg (Filename.concat dir n))
      |> List.map (fun n ->
             scan (sub_add subname n) (name ^ "." ^ n) (Filename.concat dir n)) in
    { bare with
      bare_name = name;
      bare_mainname = mainname;
      bare_subname = subname;
      bare_directory = dir;
      bare_children;
    } in
  scan "" mainname dir

let to_pkg_definition bare =
  let pkg_defs_byte =
    match bare.bare_byte_archive with
      | None -> []
      | Some file_cma ->
          [ { def_var = "archive";
              def_flav = `BaseDef;
              def_preds = [ `Pred "byte" ];
              def_value = file_cma
            };
            { def_var = "plugin";
              def_flav = `BaseDef;
              def_preds = [ `Pred "byte" ];
              def_value = file_cma
            }
          ] in
  let pkg_defs_native =
    match bare.bare_native_archive with
      | None -> []
      | Some file_cmxa ->
          [ { def_var = "archive";
              def_flav = `BaseDef;
              def_preds = [ `Pred "native" ];
              def_value = file_cmxa
            }
          ] in
  let pkg_defs_shared =
    match bare.bare_shared_archive with
      | None -> []
      | Some file_cmxs ->
          [ { def_var = "plugin";
              def_flav = `BaseDef;
              def_preds = [ `Pred "native" ];
              def_value = file_cmxs
            }
          ] in
  let requires =
    if bare.bare_byte_archive <> None then
      bare.bare_byte_requires
    else
      if bare.bare_native_archive <> None then
        bare.bare_native_requires
      else
        if bare.bare_shared_archive <> None then
          bare.bare_shared_requires
        else
          [] in
  let pkg_defs_base =
    [ { def_var = "name";
        def_flav = `BaseDef;
        def_preds = [];
        def_value = bare.bare_name
      };
      { def_var = "directory";
        def_flav = `BaseDef;
        def_preds = [];
        def_value = bare.bare_directory
      };
      { def_var = "requires";
        def_flav = `BaseDef;
        def_preds = [];
        def_value = String.concat "," requires;
      }
    ] in
  pkg_defs_base @ pkg_defs_byte @ pkg_defs_native @ pkg_defs_shared

let rec to_pkg_expr bare =
  { pkg_defs =
      to_pkg_definition bare;
    pkg_children =
      List.map (fun b -> (b.bare_subname, to_pkg_expr b)) bare.bare_children
  }

