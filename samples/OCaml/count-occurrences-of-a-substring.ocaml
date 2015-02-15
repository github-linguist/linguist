let count_substring str sub =
  let sub_len = String.length sub in
  let len_diff = (String.length str) - sub_len
  and reg = Str.regexp_string sub in
  let rec aux i n =
    if i > len_diff then n else
      try
        let pos = Str.search_forward reg str i in
        aux (pos + sub_len) (succ n)
      with Not_found -> n
  in
  aux 0 0

let () =
  Printf.printf "count 1: %d\n" (count_substring "the three truth" "th");
  Printf.printf "count 2: %d\n" (count_substring "ababababab" "abab");
;;
