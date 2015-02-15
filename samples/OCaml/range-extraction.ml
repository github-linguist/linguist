let range_extract = function
  | [] -> []
  | x::xs ->
    let f (i,j,ret) k =
      if k = succ j then (i,k,ret) else (k,k,(i,j)::ret) in
    let (m,n,ret) = List.fold_left f (x,x,[]) xs in
    List.rev ((m,n)::ret)

let string_of_range rng =
  let str (a,b) =
    if a = b then string_of_int a
    else Printf.sprintf "%d%c%d" a (if b = succ a then ',' else '-') b in
  String.concat "," (List.map str rng)

let () =
  let li =
    [ 0; 1; 2; 4; 6; 7; 8; 11; 12; 14; 15; 16; 17; 18; 19; 20; 21;
      22; 23; 24; 25; 27; 28; 29; 30; 31; 32; 33; 35; 36; 37; 38; 39 ]
  in
  let rng = range_extract li in
  print_endline(string_of_range rng)
