open Curses

let ignite_prob = 0.02
let sprout_prob = 0.01

type cell = Empty | Burning | Tree

let get w x y =
  try w.(x).(y)
  with Invalid_argument _ -> Empty

let neighborhood_burning w x y =
  try
    for _x = pred x to succ x do
      for _y = pred y to succ y do
        if get w _x _y = Burning then raise Exit
      done
    done
    ; false
  with Exit -> true

let evolves w x y =
  match w.(x).(y) with
  | Burning -> Empty
  | Tree ->
      if neighborhood_burning w x y
      then Burning
      else begin
        if (Random.float 1.0) < ignite_prob
        then Burning
        else Tree
      end
  | Empty ->
      if (Random.float 1.0) < sprout_prob
      then Tree
      else Empty

let step width height w =
  for x = 0 to pred width do
    for y = 0 to pred height do
      w.(x).(y) <- evolves w x y
    done
  done

let i = int_of_char
let repr = function
  | Empty -> i ' ' | Burning -> i '#' | Tree -> i 't'

let draw width height w =
  for x = 0 to pred width do
    for y = 0 to pred height do
      ignore(move y x);
      ignore(delch ());
      ignore(insch (repr w.(x).(y)));
    done;
  done;
  ignore(refresh ())

let () =
  Random.self_init ();
  let wnd = initscr () in
  ignore(cbreak ());
  ignore(noecho ());
  let height, width = getmaxyx wnd in
  let w = Array.make_matrix width height Empty in
  clear ();
  ignore(refresh ());
  while true do
    draw width height w;
    step width height w;
    Unix.sleep 1;
  done;
  endwin()
