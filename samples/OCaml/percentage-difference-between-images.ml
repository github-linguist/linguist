#! /usr/bin/env ocaml
#directory "+glMLite/"
#load "jpeg_loader.cma"
#load "bigarray.cma"
open Jpeg_loader

let () =
  let img1, width1, height1, col_comp1, color_space1 = load_img (Filename Sys.argv.(1))
  and img2, width2, height2, col_comp2, color_space2 = load_img (Filename Sys.argv.(2)) in

  assert(width1 = width2);
  assert(height1 = height2);
  assert(col_comp1 = col_comp2);  (* number of color components *)
  assert(color_space1 = color_space2);

  let img1 = Bigarray.array3_of_genarray img1
  and img2 = Bigarray.array3_of_genarray img2 in

  let sum = ref 0.0
  and num = ref 0 in

  for x=0 to pred width1 do
    for y=0 to pred height1 do
      for c=0 to pred col_comp1 do
        let v1 = float img1.{x,y,c}
        and v2 = float img2.{x,y,c} in
        let diff = (abs_float (v1 -. v2)) /. 255. in
        sum := diff +. !sum;
        incr num;
      done;
    done;
  done;

  let diff_percent = !sum /. float !num in
  Printf.printf " diff: %f percent\n" diff_percent;
;;
