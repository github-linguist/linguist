let strip_first_char str =
  if str = "" then "" else
  String.sub str 1 ((String.length str) - 1)

let strip_last_char str =
  if str = "" then "" else
  String.sub str 0 ((String.length str) - 1)

let strip_both_chars str =
  match String.length str with
  | 0 | 1 | 2 -> ""
  | len -> String.sub str 1 (len - 2)

let () =
  print_endline (strip_first_char "knight");
  print_endline (strip_last_char "socks");
  print_endline (strip_both_chars "brooms");
;;
