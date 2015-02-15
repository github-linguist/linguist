let zigzag n =
  (* move takes references and modifies them directly *)
  let move i j =
    if !j < n - 1 then begin
      i := max 0 (!i - 1);
      incr j
    end else
      incr i
  in
  let a = Array.make_matrix n n 0
  and x = ref 0 and y = ref 0 in
  for v = 0 to n * n - 1 do
    a.(!x).(!y) <- v;
    if (!x + !y) mod 2 = 0 then
      move x y
    else
      move y x
  done;
  a
