let is_string_empty s =
  (s = "")

let () =
  let s1 = ""
  and s2 = "not empty" in
  Printf.printf "s1 empty? %B\n" (is_string_empty s1);
  Printf.printf "s2 empty? %B\n" (is_string_empty s2);
;;
