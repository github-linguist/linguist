let counting_sort_array arr lo hi =
  let count = Array.make (hi-lo+1) 0 in
    Array.iter (fun i -> count.(i-lo) <- count.(i-lo) + 1) arr;
    Array.concat (Array.to_list (Array.mapi (fun i x -> Array.make x (lo+i)) count))
