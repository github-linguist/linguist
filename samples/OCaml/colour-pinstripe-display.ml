open Graphics

let () =
  open_graph "";
  let width = size_x ()
  and height = size_y () in
  let colors = [| black; red; green; blue; magenta; cyan; yellow; white |] in
  let num_colors = Array.length colors in
  let h = height / 4 in
  for i = 1 to 4 do
    let j = 4 - i in
    for x = 0 to pred width do
      set_color colors.((x / i) mod num_colors);
      moveto x (j * h); lineto x (j * h + h);
    done
  done;
  ignore(read_key())
