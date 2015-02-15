let () =
  let pi = 4. *. atan 1. in
  print_endline "Enter latitude		=> ";
  let lat = read_float () in
  print_endline "Enter longitude	=> ";
  let lng = read_float () in
  print_endline "Enter legal meridian	=> ";
  let ref = read_float () in
  print_newline ();

  let slat = sin (lat *. 2. *. pi /. 360.) in
  Printf.printf "    sine of latitude:   %.3f\n" slat;
  Printf.printf "    diff longitude:     %.3f\n" (lng -. ref);
  print_newline ();

  print_endline "Hour, sun hour angle, dial hour line angle from 6am to 6pm";

  for h = -6 to 6 do
    let hra = 15. *. float h in
    let hra = hra -. (lng -. ref) in
    let hla = atan (slat *. tan (hra *. 2. *. pi /. 360.)) *. 360. /. (2. *. pi) in
    Printf.printf "HR= %3d;  \t  HRA=%7.3f;  \t  HLA= %7.3f\n" h hra hla;
  done
