let trials = 10000

type door = Car | Goat

let play switch =
  let n = Random.int 3 in
  let d1 = [|Car; Goat; Goat|].(n) in
    if not switch then d1
    else match d1 with
       Car  -> Goat
     | Goat -> Car

let cars n switch =
  let total = ref 0 in
  for i = 1 to n do
    let prize = play switch in
    if prize = Car then
      incr total
  done;
  !total

let () =
  let switch = cars trials true
  and stay   = cars trials false in
  let msg strat n =
    Printf.printf "The %s strategy succeeds %f%% of the time.\n"
      strat (100. *. (float n /. float trials)) in
  msg "switch" switch;
  msg "stay" stay
