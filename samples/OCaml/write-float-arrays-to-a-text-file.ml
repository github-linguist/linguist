let write_dat filename x y ?(xprec=3) ?(yprec=5) () =
  let oc = open_out filename in
  let write_line a b = Printf.fprintf oc "%.*g\t%.*g\n" xprec a yprec b in
    List.iter2 write_line x y;
    close_out oc
