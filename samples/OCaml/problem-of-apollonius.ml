type point = { x:float; y:float }
type circle = {
  center: point;
  radius: float;
}

let new_circle ~x ~y ~r =
  { center = { x=x; y=y };
    radius = r }

let print_circle ~c =
  Printf.printf "Circle(x=%.2f, y=%.2f, r=%.2f)\n"
    c.center.x c.center.y c.radius

let defxyr c =
  (c.center.x,
   c.center.y,
   c.radius)

let solve_apollonius ~c1 ~c2 ~c3
                     ~s1 ~s2 ~s3 =
  let ( * ) = ( *. ) in
  let ( / ) = ( /. ) in
  let ( + ) = ( +. ) in
  let ( - ) = ( -. ) in

  let x1, y1, r1 = defxyr c1
  and x2, y2, r2 = defxyr c2
  and x3, y3, r3 = defxyr c3 in

  let v11 = 2.0 * x2 - 2.0 * x1
  and v12 = 2.0 * y2 - 2.0 * y1
  and v13 = x1*x1 - x2*x2 + y1*y1 - y2*y2 - r1*r1 + r2*r2
  and v14 = (2.0 * s2 * r2) - (2.0 * s1 * r1)

  and v21 = 2.0 * x3 - 2.0 * x2
  and v22 = 2.0 * y3 - 2.0 * y2
  and v23 = x2*x2 - x3*x3 + y2*y2 - y3*y3 - r2*r2 + r3*r3
  and v24 = (2.0 * s3 * r3) - (2.0 * s2 * r2) in

  let w12 = v12 / v11
  and w13 = v13 / v11
  and w14 = v14 / v11 in

  let w22 = v22 / v21 - w12
  and w23 = v23 / v21 - w13
  and w24 = v24 / v21 - w14 in

  let p = -. w23 / w22
  and q = w24 / w22 in
  let m = -. w12 * p - w13
  and n = w14 - w12 * q in

  let a = n*n + q*q - 1.0
  and b = 2.0*m*n - 2.0*n*x1 + 2.0*p*q - 2.0*q*y1 + 2.0*s1*r1
  and c = x1*x1 + m*m - 2.0*m*x1 + p*p + y1*y1 - 2.0*p*y1 - r1*r1 in

  let d = b * b - 4.0 * a * c in
  let rs = (-. b - (sqrt d)) / (2.0 * a) in

  let xs = m + n * rs
  and ys = p + q * rs in

  (new_circle xs ys rs)

let () =
  let c1 = new_circle 0.0 0.0 1.0
  and c2 = new_circle 4.0 0.0 1.0
  and c3 = new_circle 2.0 4.0 2.0 in

  let r1 = solve_apollonius c1 c2 c3 1.0 1.0 1.0 in
  print_circle r1;

  let r2 = solve_apollonius c1 c2 c3 (-1.) (-1.) (-1.) in
  print_circle r2;
;;
