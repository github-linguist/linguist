#load "graphics.cma";;

let mandelbrot xMin xMax yMin yMax xPixels yPixels maxIter =
  let rec mandelbrotIterator z c n =
    if (Complex.norm z) > 2.0 then false else
      match n with
      | 0 -> true
      | n -> let z' = Complex.add (Complex.mul z z) c in
             mandelbrotIterator z' c (n-1) in
  Graphics.open_graph
    (" "^(string_of_int xPixels)^"x"^(string_of_int yPixels));
  let dx = (xMax -. xMin) /. (float_of_int xPixels)
  and dy = (yMax -. yMin) /. (float_of_int yPixels) in
  for xi = 0 to xPixels - 1 do
    for yi = 0 to yPixels - 1 do
      let c = {Complex.re = xMin +. (dx *. float_of_int xi);
               Complex.im = yMin +. (dy *. float_of_int yi)} in
      if (mandelbrotIterator Complex.zero c maxIter) then
        (Graphics.set_color Graphics.white;
         Graphics.plot xi yi )
      else
        (Graphics.set_color Graphics.black;
         Graphics.plot xi yi )
    done
  done;;

mandelbrot (-1.5) 0.5 (-1.0) 1.0 500 500 200;;
