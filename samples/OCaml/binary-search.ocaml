let rec binary_search a value low high =
  if high = low then
    if a.(low) = value then
      low
    else
      raise Not_found
  else let mid = (low + high) / 2 in
    if a.(mid) > value then
      binary_search a value low (mid - 1)
    else if a.(mid) < value then
      binary_search a value (mid + 1) high
    else
      mid
