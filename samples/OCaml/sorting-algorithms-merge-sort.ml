let rec split_at n xs =
  match n, xs with
      0, xs ->
        [], xs
    | _, [] ->
        failwith "index too large"
    | n, x::xs when n > 0 ->
        let xs', xs'' = split_at (pred n) xs in
          x::xs', xs''
    | _, _ ->
        invalid_arg "negative argument"

let rec merge_sort cmp = function
    [] -> []
  | [x] -> [x]
  | xs ->
      let xs, ys = split_at (List.length xs / 2) xs in
        List.merge cmp (merge_sort cmp xs) (merge_sort cmp ys)

let _ =
  merge_sort compare [8;6;4;2;1;3;5;7;9]
