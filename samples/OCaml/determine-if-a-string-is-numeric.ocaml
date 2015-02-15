let is_int s =
  try ignore (int_of_string s); true
  with _ -> false

let is_float s =
  try ignore (float_of_string s); true
  with _ -> false

let is_numeric s = is_int s || is_float s
