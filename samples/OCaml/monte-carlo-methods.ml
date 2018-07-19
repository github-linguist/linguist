let get_pi throws =
  let rec helper i count =
    if i = throws then count
    else
      let rand_x = Random.float 2.0 -. 1.0
      and rand_y = Random.float 2.0 -. 1.0 in
      let dist = sqrt (rand_x *. rand_x +. rand_y *. rand_y) in
      if dist < 1.0 then
        helper (i+1) (count+1)
      else
        helper (i+1) count
  in float (4 * helper 0 0) /. float throws
