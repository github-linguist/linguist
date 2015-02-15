$ ocaml

# Random.self_init();;
- : unit = ()

# let m = Array.make_matrix 10 10 0 ;;
val m : int array array =
  [|[|0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0|];
    [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0|];
    [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0|];
    [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0|];
    [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]|]

# for i = 0 to pred 10 do
    for j = 0 to pred 10 do
      m.(i).(j) <- 1 + Random.int 20
    done;
  done;;
- : unit = ()

# try
    for i = 0 to pred 10 do
      for j = 0 to pred 10 do
        Printf.printf " %d" m.(i).(j);
        if m.(i).(j) = 20 then raise Exit;
      done;
      print_newline()
    done;
  with Exit ->
    print_newline()
  ;;
 15 8 15 9 9 6 1 18 6 18
 17 1 13 15 13 1 16 4 13 9
 15 3 5 19 17 3 1 11 5 2
 1 1 6 19 20
- : unit = ()
