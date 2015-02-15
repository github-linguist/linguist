let sort_chars s =
  let r = String.copy s in
  for i = 0 to (String.length r) - 2 do
    for j = i + 1 to (String.length r) - 1 do
      if r.[i] > r.[j] then begin
        let tmp = r.[i] in
        r.[i] <- r.[j];
        r.[j] <- tmp;
      end
    done
  done;
  (r)

let deranged (s1, s2) =
  let len1 = String.length s1
  and len2 = String.length s2 in
  if len1 <> len2 then false else
  try
    for i = 0 to pred len1 do
      if s1.[i] = s2.[i] then raise Exit
    done;
    true
  with Exit -> false

let pairs_of_list lst =
  let rec aux acc = function
    | [] -> acc
    | x::xs ->
        aux (List.fold_left (fun acc y -> (x,y)::acc) acc xs) xs
  in
  aux [] lst

let () =
  let h = Hashtbl.create 3571 in
  let ic = open_in "unixdict.txt" in
  try while true do
    let word = input_line ic in
    let key = sort_chars word in
    let l =
      try Hashtbl.find h key
      with Not_found -> []
    in
    Hashtbl.replace h key (word::l);
  done with End_of_file ->
    close_in ic;
    let lst =
      Hashtbl.fold (fun _ lw acc ->
        if List.length lw < 2 then acc else lw::acc) h []
    in
    let lst =
      List.fold_left (fun acc anagrams ->
        let pairs = pairs_of_list anagrams in
        (List.filter deranged pairs) @ acc
      ) [] lst
    in
    let res, _ =
      List.fold_left (fun (res, n) (w1, w2) ->
        let len = String.length w1 in
        match Pervasives.compare len n with
        | 0 -> ((w1, w2)::res, n)
        | 1 -> ([w1, w2], len)
        | _ -> (res, n)
      ) ([], 0) lst
    in
    List.iter (fun (w1, w2) -> Printf.printf "%s, %s\n" w1 w2) res
