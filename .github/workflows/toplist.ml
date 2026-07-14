#use "topfind";;
#list;;
#require "findlib";;
let () =
  let ld_conf = Findlib.ocaml_ldconf () in
  let print_line = Printf.printf "  %s\n" in
  Printf.printf "Reading %s\n" ld_conf;
  List.iter print_line (Findlib.read_ldconf ld_conf);;
