let quad_bezier ~img ~color
        ~p1:(_x1, _y1)
        ~p2:(_x2, _y2)
        ~p3:(_x3, _y3) =
  let (x1, y1, x2, y2, x3, y3) =
    (float _x1, float _y1, float _x2, float _y2, float _x3, float _y3)
  in
  let bz t =
    let a = (1.0 -. t) ** 2.0
    and b = 2.0 *. t *. (1.0 -. t)
    and c = t ** 2.0
    in
    let x = a *. x1 +. b *. x2 +. c *. x3
    and y = a *. y1 +. b *. y2 +. c *. y3
    in
    (int_of_float x, int_of_float y)
  in
  let rec loop _t acc =
    if _t > 20 then acc else
    begin
      let t = (float _t) /. 20.0 in
      let x, y = bz t in
      loop (succ _t) ((x,y)::acc)
    end
  in
  let pts = loop 0 [] in

  (*
  (* draw only points *)
  List.iter (fun (x, y) -> put_pixel img color x y) pts;
  *)

  (* draw segments *)
  let line = draw_line ~img ~color in
  let by_pair li f =
    ignore (List.fold_left (fun prev x -> f prev x; x) (List.hd li) (List.tl li))
  in
  by_pair pts (fun p0 p1 -> line ~p0 ~p1);
;;
