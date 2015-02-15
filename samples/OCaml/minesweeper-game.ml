exception Lost
exception Won

let put_mines g m n mines_number =
  let rec aux i =
    if i < mines_number then
    begin
      let x = Random.int n
      and y = Random.int m in
      if g.(y).(x)
      then aux i
      else begin
        g.(y).(x) <- true;
        aux (succ i)
      end
    end
  in
  aux 0

let print_abscissas n =
  print_string "\n      "; for x = 1 to n do print_int (x mod 10) done;
  print_string "\n      "; for x = 1 to n do print_char '|' done;
  print_newline()

let print_display d n =
  print_abscissas n;
  Array.iteri (fun y line ->
    Printf.printf " %2d - " (y+1);  (* print ordinates *)
    Array.iter print_char line;
    print_newline()
  ) d;
  print_newline()

let reveal d g n =
  print_abscissas n;
  Array.iteri (fun y line ->
    Printf.printf " %2d - " (y+1);  (* print ordinates *)
    Array.iteri (fun x c ->
      print_char (
        match c, g.(y).(x) with
        | '0'..'9', _ -> c
        | '.', true -> 'x'
        | '?', true -> 'X'
        | '?', false -> 'N'
        | '.', false -> '.'
        | _ -> c)
    ) line;
    print_newline()
  ) d;
  print_newline()

let toggle_mark d x y =
  match d.(y).(x) with
  | '.' -> d.(y).(x) <- '?'
  | '?' -> d.(y).(x) <- '.'
  | _ -> ()

let rec feedback g d x y =
  if d.(y).(x) = '.' then
  begin
    let n = ref 0 in  (* the number of mines around *)
    for i = (pred y) to (succ y) do
      for j = (pred x) to (succ x) do
        try if g.(i).(j) then incr n
        with _ -> ()
      done;
    done;
    match !n with
    | 0 ->
        (* recursive feedback when no mines are around *)
        d.(y).(x) <- ' ';
        for j = (pred y) to (succ y) do
          for i = (pred x) to (succ x) do
            try feedback g d i j
            with _ -> ()
          done
        done
    | _ ->
        d.(y).(x) <- (string_of_int !n).[0]
  end

let clear_cell g d x y =
  if g.(y).(x)
  then (d.(y).(x) <- '!'; raise Lost)
  else feedback g d x y

let rec user_input g d =
  try
    let s = read_line() in
    match Scanf.sscanf s "%c %d %d" (fun c x y -> c,x,y) with
    | 'm', x, y -> toggle_mark d (x-1) (y-1)
    | 'c', x, y -> clear_cell g d (x-1) (y-1)
    | _ -> raise Exit
  with Exit | Scanf.Scan_failure _
  | Invalid_argument "index out of bounds" ->
      print_string "# wrong input, try again\n> ";
      user_input g d

let check_won g d =
  let won = ref true in
  Array.iteri (fun y line ->
    Array.iteri (fun x c ->
      match c, g.(y).(x) with
      | '.', _ -> won := false
      | '?', false -> won := false
      | _ -> ()
    ) line
  ) d;
  if !won then raise Won

let minesweeper n m percent =
  let round x = int_of_float (floor (x +. 0.5)) in
  let mines_number = round ((float (n * m)) *. percent) in
  (* the ground containing the mines *)
  let g = Array.make_matrix m n false in
  put_mines g m n mines_number;
  Printf.printf "# You have to find %d mines\n" mines_number;
  (* what's displayed to the user *)
  let d = Array.make_matrix m n '.' in
  try
    while true do
      print_display d n;
      print_string "> ";
      user_input g d;
      check_won g d;
    done
  with
  | Lost ->
      print_endline "# You lost!";
      reveal d g n
  | Won ->
      print_endline "# You won!";
      reveal d g n

let () =
  Random.self_init();
  let ios, fos = int_of_string, float_of_string in
  let n, m, percent =
    try ios Sys.argv.(1), ios Sys.argv.(2), fos Sys.argv.(3)
    with _ ->
      try ios Sys.argv.(1), ios Sys.argv.(2), 0.2
      with _ -> (6, 4, 0.2)
  in
  minesweeper n m percent;
;;
