open Graphics

let draw_yinyang x y radius black white =
  let hr = radius / 2 in
  let sr = radius / 6 in
  set_color black;
  set_line_width 6;
  draw_circle x y radius;
  set_line_width 0;
  set_color black;
  fill_arc x y radius radius 270 450;
  set_color white;
  fill_arc x y radius radius 90 270;
  fill_arc x (y + hr) hr hr 270 450;
  set_color black;
  fill_arc x (y - hr) hr hr 90 270;
  fill_circle x (y + hr) sr;
  set_color white;
  fill_circle x (y - hr) sr

let () =
  open_graph "";
  let width = size_x()
  and height = size_y() in
  set_color (rgb 200 200 200);
  fill_rect 0 0 width height;
  let w = width / 3
  and h = height / 3 in
  let r = (min w h) / 3 in
  draw_yinyang w (h*2) (r*2) black white;
  draw_yinyang (w*2) h r blue magenta;
  ignore(read_key())
