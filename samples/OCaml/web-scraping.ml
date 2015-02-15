let () =
  let _,_, page_content = make_request ~url:Sys.argv.(1) ~kind:GET () in

  let lines = Str.split (Str.regexp "\n") page_content in
  let str =
    List.find
      (fun line ->
        try ignore(Str.search_forward (Str.regexp "UTC") line 0); true
        with Not_found -> false)
      lines
  in
  let str = Str.global_replace (Str.regexp "<BR>") "" str in
  print_endline str;
;;
