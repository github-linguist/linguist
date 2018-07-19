let s_of_n_creator n =
  let i = ref 0
  and sample = ref [| |] in
  fun item ->
    incr i;
    if !i <= n then sample := Array.append [| item |] !sample
    else if Random.int !i < n then !sample.(Random.int n) <- item;
    !sample

let test n items_set =
  let s_of_n = s_of_n_creator n in
  Array.fold_left (fun _ v -> s_of_n v) [| |] items_set

let () =
  Random.self_init();
  let n = 3 in
  let num_items = 10 in
  let items_set = Array.init num_items (fun i -> i) in
  let results = Array.create num_items 0 in
  for i = 1 to 100_000 do
    let res = test n items_set in
    Array.iter (fun j -> results.(j) <- succ results.(j)) res
  done;
  Array.iter (Printf.printf " %d") results;
  print_newline()
