let () =
  let max = 12 in
  let fmax = float_of_int max in

  let dgts = int_of_float (ceil (log10 (fmax *. fmax))) in
  let fmt = Printf.printf " %*d" dgts in
  let fmt2 = Printf.printf "%*s%c" dgts in

  fmt2 "" 'x';
  for i = 1 to max do fmt i done;
  print_string "\n\n";

  for j = 1 to max do
    fmt j;
    for i = 1 to pred j do fmt2 "" ' '; done;
    for i = j to max do fmt (i*j); done;
    print_newline()
  done;
  print_newline()
