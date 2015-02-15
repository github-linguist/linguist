let p = [
    "Aleph",   1.0 /. 5.0;
    "Beth",    1.0 /. 6.0;
    "Gimel",   1.0 /. 7.0;
    "Daleth",  1.0 /. 8.0;
    "He",      1.0 /. 9.0;
    "Waw",     1.0 /. 10.0;
    "Zayin",   1.0 /. 11.0;
    "Heth", 1759.0 /. 27720.0;
  ]

let rec take k = function
  | (v, p)::tl -> if k < p then v else take (k -. p) tl
  | _ -> invalid_arg "take"

let () =
  let n = 1_000_000 in
  Random.self_init();
  let h = Hashtbl.create 3 in
  List.iter (fun (v, _) -> Hashtbl.add h v 0) p;
  let tot = List.fold_left (fun acc (_, p) -> acc +. p) 0.0 p in
  for i = 1 to n do
    let sel = take (Random.float tot) p in
    let n = Hashtbl.find h sel in
    Hashtbl.replace h sel (succ n)  (* count the number of each item *)
  done;
  List.iter (fun (v, p) ->
    let d = Hashtbl.find h v in
    Printf.printf "%s \t %f %f\n" v p (float d /. float n)
  ) p
