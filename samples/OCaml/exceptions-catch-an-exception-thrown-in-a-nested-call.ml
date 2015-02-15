exception U0
exception U1

let baz i =
  raise (if i = 0 then U0 else U1)

let bar i = baz i (* Nest those calls *)

let foo () =
  for i = 0 to 1 do
    try
      bar i
    with U0 ->
      print_endline "Function foo caught exception U0"
  done

let () = foo ()
