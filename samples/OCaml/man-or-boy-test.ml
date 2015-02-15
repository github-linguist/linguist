let rec a k x1 x2 x3 x4 x5 =
  if k <= 0 then
    x4 () + x5 ()
  else
    let m = ref k in
    let rec b () =
      decr m;
      a !m b x1 x2 x3 x4
    in
    b ()

let () =
  Printf.printf "%d\n" (a 10 (fun () -> 1) (fun () -> -1) (fun () -> -1) (fun () -> 1) (fun () -> 0))
