#use "topfind";;
#list;;
#require "unix";;
#require "findlib";;
let realpath = Filename.concat (Sys.getcwd()) "src/findlib/realpath.ml" in
Topdirs.dir_use Format.err_formatter realpath;;
let () =
  let ld_conf = Findlib.ocaml_ldconf () in
  let print_line {Findlib.raw = raw; Findlib.eff = eff} =
    let real = realpath eff in
    let real =
      if real = eff then
        ""
      else
        "\n    -> " ^ real
    in
    let eff =
      if raw ^ real = eff then
        ""
      else
        "\n    -> " ^ eff
    in
    Printf.printf "  %s%s%s\n" raw eff real
  in
  Printf.printf "Reading %s\n" ld_conf;
  List.iter print_line (Findlib.read_ldconf ld_conf);;
