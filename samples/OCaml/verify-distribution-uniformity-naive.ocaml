let distcheck fn n ?(delta=1.0) () =
  let h = Hashtbl.create 5 in
  for i = 1 to n do
    let v = fn() in
    let n =
      try Hashtbl.find h v
      with Not_found -> 0
    in
    Hashtbl.replace h v (n+1)
  done;
  Hashtbl.iter (fun v n -> Printf.printf "%d => %d\n%!" v n) h;
  let target = (float n) /. float (Hashtbl.length h) in
  Hashtbl.iter (fun key value ->
    if abs_float(float value -. target) > 0.01 *. delta *. (float n)
    then (Printf.eprintf
      "distribution potentially skewed for '%d': expected around %f, got %d\n%!"
       key target value)
  ) h;
;;
