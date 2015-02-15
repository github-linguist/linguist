let rec is_sorted comp = function
 | e1 :: e2 :: r -> comp e1 e2 <= 0 && is_sorted comp (e2 :: r)
 | _             -> true

(* Fisher-Yates shuffle on lists; uses temp array *)
let shuffle l =
  let ar = Array.of_list l in
    for n = Array.length ar - 1 downto 1 do
      let k = Random.int (n+1) in
      let temp = ar.(k) in (* swap ar.(k) and ar.(n) *)
        ar.(k) <- ar.(n);
        ar.(n) <- temp
    done;
    Array.to_list ar

let rec bogosort li =
  if is_sorted compare li then
    li
  else
    bogosort (shuffle li)
