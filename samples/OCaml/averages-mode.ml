let mode lst =
  let seen = Hashtbl.create 42 in
    List.iter (fun x ->
                 let old = if Hashtbl.mem seen x then
                   Hashtbl.find seen x
                 else 0 in
                   Hashtbl.replace seen x (old + 1))
      lst;
    let best = Hashtbl.fold (fun _ -> max) seen 0 in
      Hashtbl.fold (fun k v acc ->
                      if v = best then k :: acc
                      else acc)
        seen []
