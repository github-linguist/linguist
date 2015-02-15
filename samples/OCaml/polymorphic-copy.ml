let obj1 =
  object
    method name = "T"
  end

let obj2 =
  object
    method name = "S"
  end

let () =
  print_endline (Oo.copy obj1)#name; (* prints "T" *)
  print_endline (Oo.copy obj2)#name; (* prints "S" *)
