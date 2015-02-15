print_string "Enter a string: ";
let str = read_line () in
  print_string "Enter an integer: ";
  let num = read_int () in
    Printf.printf "%s%d\n" str num
