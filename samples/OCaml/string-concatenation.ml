let s = "hello"
let s1 = s ^ " literal"
let () =
  print_endline (s ^ " literal");
  (* or Printf.printf "%s literal\n" s; *)
  print_endline s1
