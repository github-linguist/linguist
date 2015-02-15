let pi_twice = 2.0 *. 3.14159_26535_89793_23846_2643
let day = float (24 * 60 * 60)

let rad_of_time t =
  t *. pi_twice /. day

let time_of_rad r =
  r *. day /. pi_twice

let mean_angle angles =
  let sum_sin = List.fold_left (fun sum a -> sum +. sin a) 0.0 angles
  and sum_cos = List.fold_left (fun sum a -> sum +. cos a) 0.0 angles in
  atan2 sum_sin sum_cos

let mean_time times =
  let angles = List.map rad_of_time times in
  let t = time_of_rad (mean_angle angles) in
  if t < 0.0 then t +. day else t

let parse_time t =
  Scanf.sscanf t "%d:%d:%d" (fun h m s -> float (s + m * 60 + h * 3600))

let round x = int_of_float (floor (x +. 0.5))

let string_of_time t =
  let t = round t in
  let h = t / 3600 in
  let rem = t mod 3600 in
  let m = rem / 60 in
  let s = rem mod 60 in
  Printf.sprintf "%d:%d:%d" h m s

let () =
  let times = ["23:00:17"; "23:40:20"; "00:12:45"; "00:17:19"] in
  Printf.printf "The mean time of [%s] is: %s\n"
    (String.concat "; " times)
    (string_of_time (mean_time (List.map parse_time times)))
