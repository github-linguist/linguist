open Graphics

type dir = North | East | South | West

let turn_left = function
  | North -> West
  | East  -> North
  | South -> East
  | West  -> South

let turn_right = function
  | North -> East
  | East  -> South
  | South -> West
  | West  -> North

let move (x, y) = function
  | North -> x, y + 1
  | East  -> x + 1, y
  | South -> x, y - 1
  | West  -> x - 1, y

let () =
  open_graph "";
  let rec loop (x, y as pos) dir =
    let color = point_color x y in
    set_color (if color = white then black else white);
    plot x y;
    let dir = (if color = white then turn_right else turn_left) dir in
    if not(key_pressed()) then loop (move pos dir) dir
  in
  loop (size_x()/2, size_y()/2) North
