let () =
  let url = "http://www.rosettacode.org" in
  let _,_, page_content = make_request ~url ~kind:GET () in
  print_endline page_content;
;;
