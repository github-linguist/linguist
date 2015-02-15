let rec columns l =
  match List.filter ((<>) []) l with
    [] -> []
  | l -> List.map List.hd l :: columns (List.map List.tl l)

let replicate n x = Array.to_list (Array.make n x)

let bead_sort l =
  List.map List.length (columns (columns (List.map (fun e -> replicate e 1) l)))
