let rem_from rem from =
  List.filter ((<>) rem) from

let float_rgb (r,g,b) =  (* prevents int overflow *)
  (float r, float g, float b)

let round x =
  int_of_float (floor (x +. 0.5))

let int_rgb (r,g,b) =
  (round r, round g, round b)

let rgb_add (r1,g1,b1) (r2,g2,b2) =
  (r1 +. r2,
   g1 +. g2,
   b1 +. b2)

let rgb_mean px_list =
  let n = float (List.length px_list) in
  let r, g, b = List.fold_left rgb_add (0.0, 0.0, 0.0) px_list in
  (r /. n, g /. n, b /. n)

let extrems lst =
  let min_rgb = (infinity, infinity, infinity)
  and max_rgb = (neg_infinity, neg_infinity, neg_infinity) in
  List.fold_left (fun ((sr,sg,sb), (mr,mg,mb)) (r,g,b) ->
    ((min sr r), (min sg g), (min sb b)),
    ((max mr r), (max mg g), (max mb b))
  ) (min_rgb, max_rgb) lst

let volume_and_dims lst =
  let (sr,sg,sb), (br,bg,bb) = extrems lst in
  let dr, dg, db = (br -. sr), (bg -. sg), (bb -. sb) in
  (dr *. dg *. db),
  (dr, dg, db)

let make_cluster pixel_list =
  let vol, dims = volume_and_dims pixel_list in
  let len = float (List.length pixel_list) in
  (rgb_mean pixel_list, len *. vol, dims, pixel_list)

type axis = R | G | B
let largest_axis (r,g,b) =
  match compare r g, compare r b with
  | 1, 1 -> R
  | -1, 1 -> G
  | 1, -1 -> B
  | _ ->
      match compare g b with
      | 1 -> G
      | _ -> B

let subdivide ((mr,mg,mb), n_vol_prod, vol, pixels) =
  let part_func =
    match largest_axis vol with
    | R -> (fun (r,_,_) -> r < mr)
    | G -> (fun (_,g,_) -> g < mg)
    | B -> (fun (_,_,b) -> b < mb)
  in
  let px1, px2 = List.partition part_func pixels in
  (make_cluster px1, make_cluster px2)

let color_quant img n =
  let width, height = get_dims img in
  let clusters =
    let lst = ref [] in
    for x = 0 to pred width do
      for y = 0 to pred height do
        let rgb = float_rgb (get_pixel_unsafe img x y) in
        lst := rgb :: !lst
      done;
    done;
    ref [make_cluster !lst]
  in
  while (List.length !clusters) < n do
    let dumb = (0.0,0.0,0.0) in
    let unused = (dumb, neg_infinity, dumb, []) in
    let select ((_,v1,_,_) as c1) ((_,v2,_,_) as c2) =
      if v1 > v2 then c1 else c2
    in
    let cl = List.fold_left (fun c1 c2 -> select c1 c2) unused !clusters in
    let cl1, cl2 = subdivide cl in
    clusters := cl1 :: cl2 :: (rem_from cl !clusters)
  done;
  let module PxMap = Map.Make
    (struct type t = float * float * float let compare = compare end) in
  let m =
    List.fold_left (fun m (mean, _, _, pixel_list) ->
      let int_mean = int_rgb mean in
      List.fold_left (fun m px -> PxMap.add px int_mean m) m pixel_list
    ) PxMap.empty !clusters
  in
  let res = new_img ~width ~height in
  for y = 0 to pred height do
    for x = 0 to pred width do
      let rgb = float_rgb (get_pixel_unsafe img x y) in
      let mean_rgb = PxMap.find rgb m in
      put_pixel_unsafe res mean_rgb x y;
    done;
  done;
  (res)
