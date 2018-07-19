let rec insert_after a b = function
   c :: cs when a = c -> a :: b :: cs
 | c :: cs            -> c :: insert_after a b cs
 | []                 -> raise Not_found
