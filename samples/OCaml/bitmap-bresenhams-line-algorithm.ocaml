let draw_line ~img ~color ~p0:(x0,y0) ~p1:(x1,y1) =

  let steep = abs(y1 - y0) > abs(x1 - x0) in

  let plot =
    if steep
    then (fun x y -> put_pixel img color y x)
    else (fun x y -> put_pixel img color x y)
  in

  let x0, y0, x1, y1 =
    if steep
    then y0, x0, y1, x1
    else x0, y0, x1, y1
  in
  let x0, x1, y0, y1 =
    if x0 > x1
    then x1, x0, y1, y0
    else x0, x1, y0, y1
  in

  let delta_x = x1 - x0
  and delta_y = abs(y1 - y0) in
  let error = -delta_x / 2
  and y_step =
    if y0 < y1 then 1 else -1
  in
  let rec loop x y error =
    plot x y;
    if x <= x1 then
      let error = error + delta_y in
      let y, error =
        if error > 0
        then (y + y_step), (error - delta_x)
        else y, error
      in
      loop (succ x) y error
  in
  loop x0 y0 error
;;
