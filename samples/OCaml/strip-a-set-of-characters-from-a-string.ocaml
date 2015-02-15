let stripchars s cs =
  let len = String.length s in
  let res = String.create len in
  let rec aux i j =
    if i >= len then String.sub res 0 j
    else if String.contains cs s.[i] then
      aux (succ i) (j)
    else begin
      res.[j] <- s.[i];
      aux (succ i) (succ j)
    end
  in
  aux 0 0
