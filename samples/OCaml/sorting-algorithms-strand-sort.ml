let rec strand_sort (cmp : 'a -> 'a -> int) : 'a list -> 'a list = function
   []    -> []
 | x::xs ->
   let rec extract_strand x = function
      [] -> [x], []
    | x1::xs when cmp x x1 <= 0 ->
      let strand, rest = extract_strand x1 xs in x::strand, rest
    | x1::xs ->
      let strand, rest = extract_strand x xs in strand, x1::rest
   in
   let strand, rest = extract_strand x xs in
   List.merge cmp strand (strand_sort cmp rest)
