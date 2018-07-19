open Graphics

let round v =
  int_of_float (floor (v +. 0.5))

let middle (x1, y1) (x2, y2) =
  ((x1 +. x2) /. 2.0,
   (y1 +. y2) /. 2.0)

let draw_line (x1, y1) (x2, y2) =
  moveto (round x1) (round y1);
  lineto (round x2) (round y2);
;;

let draw_triangle (p1, p2, p3) =
  draw_line p1 p2;
  draw_line p2 p3;
  draw_line p3 p1;
;;

let () =
  open_graph "";
  let width = float (size_x ()) in
  let height = float (size_y ()) in
  let pad = 20.0 in
  let initial_triangle =
    ( (pad, pad),
      (width -. pad, pad),
      (width /. 2.0, height -. pad) )
  in
  let rec loop step tris =
    if step <= 0 then tris else
      loop (pred step) (
        List.fold_left (fun acc (p1, p2, p3) ->
          let m1 = middle p1 p2
          and m2 = middle p2 p3
          and m3 = middle p3 p1 in
          let tri1 = (p1, m1, m3)
          and tri2 = (p2, m2, m1)
          and tri3 = (p3, m3, m2) in
          tri1 :: tri2 :: tri3 :: acc
        ) [] tris
      )
  in
  let res = loop 6 [ initial_triangle ] in
  List.iter draw_triangle res;
  ignore (read_key ())
