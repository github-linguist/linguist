let rec hanoi n a b c =
  if n <> 0 then begin
    hanoi (pred n) a c b;
    Printf.printf "Move disk from pole %d to pole %d\n" a b;
    hanoi (pred n) c b a
  end

let () =
  hanoi 4 1 2 3
