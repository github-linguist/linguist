#use "topfind"
#require "inifiles"
open Inifiles

let print_field ini (label, field) =
  try
    let v = ini#getval "params" field in
    Printf.printf "%s: %s\n" label v
  with Invalid_element _ ->
    Printf.printf "%s: not defined\n" label

let () =
  let ini = new inifile "./conf.ini" in
  let lst = [
    "Full name", "FULLNAME";
    "likes", "FAVOURITEFRUIT";
    "needs peeling", "NEEDSPEELING";
    "seeds removed", "SEEDSREMOVED";
  ] in
  List.iter (print_field ini) lst;

  let v = ini#getaval "params" "OTHERFAMILY" in
  print_endline "other family:";
  List.iter (Printf.printf "- %s\n") v;
;;
