#use "topfind";;
#list;;
#require "findlib";;
let () =
  let ld_conf = Findlib.ocaml_ldconf () in
  let print_line {Findlib.raw = raw; Findlib.eff = eff} =
    let eff =
      if raw = eff then
        ""
      else
        "\n    -> " ^ eff
    in
    Printf.printf "  %s%s\n" raw eff
  in
  Printf.printf "Reading %s\n" ld_conf;
  List.iter print_line (Findlib.read_ldconf ld_conf);;
