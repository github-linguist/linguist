let _mod = 1_000_000_000
let state = Array.create 55 0
let si = ref 0
let sj = ref 0

let rec subrand_seed _p1 =
  let p1 = ref _p1 in
  let p2 = ref 1 in
  state.(0) <- !p1 mod _mod;
  let j = ref 21 in
  for i = 1 to pred 55 do
    if !j >= 55 then j := !j - 55;
    state.(!j) <- !p2;
    p2 := !p1 - !p2;
    if !p2 < 0 then p2 := !p2 + _mod;
    p1 := state.(!j);
    j := !j + 21;
  done;
  si := 0;
  sj := 24;
  for i = 0 to pred 165 do ignore (subrand()) done

and subrand() =
  if !si = !sj then subrand_seed 0;
  decr si;  if !si < 0 then si := 54;
  decr sj;  if !sj < 0 then sj := 54;
  let x = state.(!si) - state.(!sj) in
  let x = if x < 0 then x + _mod else x in
  state.(!si) <- x;
  (x)

let () =
  subrand_seed 292929;
  for i = 1 to 10 do Printf.printf "%d\n" (subrand()) done
