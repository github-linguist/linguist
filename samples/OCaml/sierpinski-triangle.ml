let sierpinski n =
  let rec loop down space n =
    if n = 0 then
      down
    else
      loop (List.map (fun x -> space ^ x ^ space) down @
              List.map (fun x -> x ^ " " ^ x) down)
        (space ^ space)
        (n - 1)
  in loop ["*"] " " n

let () =
  List.iter print_endline (sierpinski 4)
