let randN n =
  if Random.int n = 0 then 1 else 0

let rec unbiased n =
  let a = randN n in
  if a <> randN n then a else unbiased n

let () =
  Random.self_init();
  let n = 50_000 in
  for b = 3 to 6 do
    let cb = ref 0 in
    let cu = ref 0 in
    for i = 1 to n do
      cb := !cb + (randN b);
      cu := !cu + (unbiased b);
    done;
    Printf.printf "%d: %5.2f%%  %5.2f%%\n"
      b (100.0 *. float !cb /. float n) (100.0 *. float !cu /. float n)
  done
