let decimal_of_roman roman =
  let arabic = ref 0 in
  let lastval = ref 0 in
  for i = (String.length roman) - 1 downto 0 do
    let n =
      match roman.[i] with
      | 'M' | 'm' -> 1000
      | 'D' | 'd' -> 500
      | 'C' | 'c' -> 100
      | 'L' | 'l' -> 50
      | 'X' | 'x' -> 10
      | 'V' | 'v' -> 5
      | 'I' | 'i' -> 1
      | _ -> 0
    in
    if n < !lastval
    then arabic := !arabic - n
    else arabic := !arabic + n;
    lastval := n
  done;
  !arabic

let () =
  Printf.printf " %d\n" (decimal_of_roman "MCMXC");
  Printf.printf " %d\n" (decimal_of_roman "MMVIII");
  Printf.printf " %d\n" (decimal_of_roman "MDCLXVI");
;;
