#directory "+cairo"
#load "bigarray.cma"
#load "cairo.cma"

let img_name = "/tmp/fractree.png"
let width  = 480
let height = 640

let level = 9
let line_width = 4.0

let color = (1.0, 0.5, 0.0)

let pi = 4.0 *. atan 1.0

let angle_split = pi *. 0.12
let angle_rand  = pi *. 0.12

let () =
  Random.self_init();
  let surf = Cairo.image_surface_create Cairo.FORMAT_RGB24 ~width ~height in
  let ctx = Cairo.create surf in
  Cairo.set_antialias ctx Cairo.ANTIALIAS_SUBPIXEL;
  Cairo.set_line_cap ctx Cairo.LINE_CAP_ROUND;

  let draw_line (x,y) (dx,dy) =
    Cairo.move_to ctx x  (float height -. y);
    Cairo.line_to ctx dx (float height -. dy);
    Cairo.stroke ctx;
  in
  let set_color (r,g,b) v =
    Cairo.set_source_rgb ctx ~red:(r *. v) ~green:(g *. v) ~blue:(b *. v);
  in
  let trans_pos (x,y) len angle =
    let _x = cos angle
    and _y = sin angle in
    (x +. (_x *. len),
     y +. (_y *. len))
  in

  let rec loop ~level ~pos ~line_width ~line_len
               ~angle ~angle_split ~angle_rand ~intc =
    if level > 0 then begin
      (* draw the current segment *)
      Cairo.set_line_width ctx line_width;
      set_color color intc;
      let pos_to = trans_pos pos line_len angle in
      draw_line pos pos_to;
      (* evolution of the parameters *)
      let line_width = line_width *. 0.8
      and line_len   = line_len   *. 0.62
      and angle_split = angle_split *. 1.02
      and angle_rand  = angle_rand  *. 1.02
      and intc = intc *. 0.9
      in
      let next_loop =
        loop ~level:(pred level) ~pos:pos_to ~intc
             ~line_width ~line_len ~angle_split ~angle_rand
      in
      (* split *)
      let angle_left  = angle +. angle_split +. Random.float angle_rand
      and angle_right = angle -. angle_split -. Random.float angle_rand
      in
      next_loop ~angle:angle_left;
      next_loop ~angle:angle_right
    end
  in

  let pos = (float width *. 0.5, float height *. 0.1)
  and line_len = float height *. 0.3
  in
  loop ~level ~pos ~angle:(pi /. 2.0)
       ~angle_split ~angle_rand
       ~line_width ~line_len ~intc:1.0;

  Cairo_png.surface_write_to_file surf img_name
  (*Cairo_png.surface_write_to_channel surf stdout*)
