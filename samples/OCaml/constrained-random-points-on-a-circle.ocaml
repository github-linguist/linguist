let p x y =
  let d = sqrt(x ** 2.0 +. y ** 2.0) in
  10.0 <= d && d <= 15.0

let () =
  Random.self_init();
  let rec aux i acc =
    if i >= 100 then acc else
      let x = (Random.float 40.0) -. 20.0
      and y = (Random.float 40.0) -. 20.0 in
      if (p x y)
      then aux (succ i) ((x,y)::acc)
      else aux i acc
  in
  let points = aux 0 [] in
  let g = Array.init 40 (fun _ -> String.make 40 ' ') in
  List.iter (fun (x,y) ->
    let x = (int_of_float x) + 20
    and y = (int_of_float y) + 20 in
    g.(y).[x] <- 'o'
  ) points;
  Array.iter print_endline g
