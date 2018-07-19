let best_shuffle s =
  let len = String.length s in
  let r = String.copy s in
  for i = 0 to pred len do
    for j = 0 to pred len do
      if i <> j && s.[i] <> r.[j] && s.[j] <> r.[i] then
        begin
          let tmp = r.[i] in
          r.[i] <- r.[j];
          r.[j] <- tmp;
        end
    done;
  done;
  (r)

let count_same s1 s2 =
  let len1 = String.length s1
  and len2 = String.length s2 in
  let n = ref 0 in
  for i = 0 to pred (min len1 len2) do
    if s1.[i] = s2.[i] then incr n
  done;
  !n

let () =
  let test s =
    let s2 = best_shuffle s in
    Printf.printf " '%s', '%s' -> %d\n" s s2 (count_same s s2);
  in
  test "tree";
  test "abracadabra";
  test "seesaw";
  test "elk";
  test "grrrrrr";
  test "up";
  test "a";
;;
