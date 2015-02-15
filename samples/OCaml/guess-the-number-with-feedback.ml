let rec _read_int() =
  try read_int()
  with _ ->
    print_endline "Please give a cardinal numbers.";
    (* TODO: what is the correct word? cipher, digit, figure or numeral? *)
    _read_int() ;;

let () =
  print_endline "Please give a set limits (two integers):";
  let a = _read_int()
  and b = _read_int() in
  let a, b =
    if a < b
    then (a, b)
    else (b, a)
  in
  Random.self_init();
  let target = a + Random.int (b - a) in
  Printf.printf "I have choosen a number between %d and %d\n%!" a b;
  print_endline "Please guess it!";
  let rec loop () =
    let guess = _read_int() in
    if guess = target then
    begin
      print_endline "The guess was equal to the target.\nCongratulation!";
      exit 0
    end;
    if guess < a || guess > b then
      print_endline "The input was inappropriate."
    else if guess > target then
      print_endline "The guess was higher than the target."
    else if guess < target then
      print_endline "The guess was less than the target.";
    loop ()
  in
  loop ()
