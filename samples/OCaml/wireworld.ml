let w = [|
    "  ......tH              ";
    " .        ......        ";
    "  ...Ht...      .       ";
    "               ....     ";
    "               .  ..... ";
    "               ....     ";
    "  tH......      .       ";
    " .        ......        ";
    "  ...Ht...              ";
  |]

let is_head w x y =
  try if w.(x).[y] = 'H' then 1 else 0
  with _ -> 0

let neighborhood_heads w x y =
  let n = ref 0 in
  for _x = pred x to succ x do
    for _y = pred y to succ y do
      n := !n + (is_head w _x _y)
    done;
  done;
  (!n)

let step w =
  let n = Array.init (Array.length w) (fun i -> String.copy w.(i)) in
  let width = Array.length w
  and height = String.length w.(0)
  in
  for x = 0 to pred width do
    for y = 0 to pred height do
      n.(x).[y] <- (
        match w.(x).[y] with
        | ' ' -> ' '
        | 'H' -> 't'
        | 't' -> '.'
        | '.' ->
            (match neighborhood_heads w x y with
            | 1 | 2 -> 'H'
            | _ -> '.')
        | _ -> assert false)
    done;
  done;
  (n)

let print = (Array.iter print_endline)

let () =
  let rec aux w =
    Unix.sleep 1;
    let n = step w in
    print n;
    aux n
  in
  aux w
