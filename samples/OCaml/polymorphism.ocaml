class point ?(x=0.0) ?(y=0.0) () = (* extra () used to erase the optional parameters *)
object (self)
  val mutable x = x
  val mutable y = y

  method x = x
  method y = y
  method set_x x' = x <- x'
  method set_y y' = y <- y'
  method print = Printf.sprintf "Point (%f, %f)" x y
  method copy = {< >}
end

class circle ?(r=1.0) ?(x=0.0) ?(y=0.0) () =
object (self)
  inherit point ~x:x ~y:y ()
  val mutable r = r

  method r = r
  method set_r r' = r <- r'
  method print = Printf.sprintf "Circle (%f, %f, %f)" r x y
end

let print x = print_endline x#print

let () =
  let p = new point () in
  let c = new circle () in
    print c;
    print p;
    c#set_x 10.0;
    print c;
    print (new point ~y:2.1 ())
