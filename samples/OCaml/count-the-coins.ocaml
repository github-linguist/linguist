let changes amount coins =
  let ways = Array.make (amount + 1) 0L in
  ways.(0) <- 1L;
  List.iter (fun coin ->
    for j = coin to amount do
      ways.(j) <- Int64.add ways.(j) ways.(j - coin)
    done
  ) coins;
  ways.(amount)

let () =
  Printf.printf "%Ld\n" (changes    1_00 [25; 10; 5; 1]);
  Printf.printf "%Ld\n" (changes 1000_00 [100; 50; 25; 10; 5; 1]);
;;
