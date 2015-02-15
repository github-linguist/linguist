open Graphics

let round x = truncate (floor (x +. 0.5))

let () =
  open_graph "";
  let width = size_x ()
  and height = size_y () in
  let bars = [| 8; 16; 32; 64 |] in
  let n = Array.length bars in
  Array.iteri (fun i bar ->
    let part = float width /. float bar in
    let y = (height / n) * (n - i - 1) in
    for j = 0 to pred bar do
      let x = round (float j *. part) in
      let v = round (float j *. 255. /. float (bar - 1)) in
      let v = if (i mod 2) = 0 then v else 255 - v in
      set_color (rgb v v v);
      fill_rect x y (round part) (height / n)
    done
  ) bars;
  ignore(read_key())
