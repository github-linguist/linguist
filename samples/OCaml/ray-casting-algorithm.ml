type point = { x:float; y:float }

type polygon = {
  vertices: point array;
  edges: (int * int) list;
}

let p x y = { x=x; y=y }

let square_v = [|
  (p 0. 0.); (p 10. 0.); (p 10. 10.); (p 0. 10.);
  (p 2.5 2.5); (p 7.5 0.1); (p 7.5 7.5); (p 2.5 7.5)
|]

let esa_v = [|
  (p 3. 0.); (p 7. 0.); (p 10. 5.); (p 7. 10.); (p 3. 10.); (p 0. 5.)
|]

let esa = {
  vertices = esa_v;
  edges = [ (0,1); (1,2); (2,3); (3,4); (4,5); (5,0) ]
}

let square = {
  vertices = square_v;
  edges = [ (0,1); (1,2); (2,3); (3,0) ]
}

let squarehole = {
  vertices = square_v;
  edges = [ (0,1); (1,2); (2,3); (3,0); (4,5); (5,6); (6,7); (7,4) ]
}

let strange = {
  vertices = square_v;
  edges = [ (0,4); (4,3); (3,7); (7,6); (6,2); (2,1); (1,5); (5,0) ]
}


let min_y ~a ~b = if a.y > b.y then (b) else (a)

let coeff_ang ~pa ~pb = (pb.y -. pa.y) /. (pb.x -. pa.x)

let huge_val = infinity

let hseg_intersect_seg ~s ~a ~b =
  let _eps =
    if s.y = (max a.y b.y) ||
       s.y = (min a.y b.y) then 0.00001 else 0.0
  in
  if  (s.y +. _eps) > (max a.y b.y) ||
      (s.y +. _eps) < (min a.y b.y) ||
       s.x > (max a.x b.x) then (false)
  else if s.x <= (min a.x b.x) then (true)
  else
    let ca = if a.x <> b.x then (coeff_ang a b) else (huge_val) in
    let my = min_y ~a ~b in
    let cp = if (s.x -. my.x) <> 0.0 then (coeff_ang my s) else (huge_val) in
    (cp >= ca)
;;


let point_is_inside ~poly ~pt =
  let cross = ref 0 in
  List.iter (fun (a,b) ->
    if hseg_intersect_seg pt
             poly.vertices.(a)
             poly.vertices.(b)
    then incr cross
  ) poly.edges;
  ( (!cross mod 2) <> 0)
;;


let make_test p label s =
  Printf.printf "point (%.5f,%.5f) is " p.x p.y;
  print_string (if point_is_inside s p
                then "INSIDE "
                else "OUTSIDE ");
  print_endline label;
;;


let () =
  let test_points = [
    (p 5. 5.); (p 5. 8.); (p 2. 2.); (p 0. 0.);
    (p 10. 10.); (p 2.5 2.5); (p 0.01 5.);
    (p 2.2 7.4); (p 0. 5.); (p 10. 5.); (p (-4.) 10.) ] in

  List.iter (fun p ->
    make_test p "square"     square;
    make_test p "squarehole" squarehole;
    make_test p "strange"    strange;
    make_test p "esa"        esa;
    print_newline()
  ) test_points;
;;
