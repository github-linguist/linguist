open Graphics

let round x =
  int_of_float (floor (x +. 0.5))

let () =
  open_graph "";
  let cols = size_x () in
  let rows = size_y () in
  let colors = [| black; red; green; blue; magenta; cyan; yellow; white |] in
  let n = Array.length colors in
  let bar_width = (float cols) /. (float n) in
  Array.iteri (fun i color ->
    let x1 = bar_width *. (float i) in
    let x2 = bar_width *. (float (succ i)) in
    set_color color;
    fill_rect (round x1) 0 (round x2) rows;
  ) colors;
  ignore (read_key ());
;;
