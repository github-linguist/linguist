let to_vlq n =
  let a, b = n lsr 7, n land 0x7F in
  let rec aux n acc =
    let x = (n land 0x7F) lor 0x80
    and xs = n lsr 7 in
    if xs > 0
    then aux xs (x::acc)
    else x::acc
  in
  aux a [b]

let to_num = List.fold_left (fun n x -> n lsl 7 + (x land 0x7F)) 0

let v_rep n =
  Printf.printf "%d ->" n;
  let seq = to_vlq n in
  List.iter (Printf.printf " 0x%02X") seq;
  let num = to_num seq in
  Printf.printf "-> %d\n%!" num;
  assert (n = num)

let _ =
  v_rep 0x200000;
  v_rep 0x1FFFFF
