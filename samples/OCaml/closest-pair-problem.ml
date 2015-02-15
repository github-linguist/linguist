type point = { x : float; y : float }


let cmpPointX (a : point) (b : point) = compare a.x b.x
let cmpPointY (a : point) (b : point) = compare a.y b.y


let distSqrd (seg : (point * point) option) =
  match seg with
  | None -> max_float
  | Some(line) ->
    let a = fst line in
    let b = snd line in

    let dx = a.x -. b.x in
    let dy = a.y -. b.y in

    dx*.dx +. dy*.dy


let dist seg =
  sqrt (distSqrd seg)


let shortest l1 l2 =
  if distSqrd l1 < distSqrd l2 then
    l1
  else
    l2


let halve l =
  let n = List.length l in
  BatList.split_at (n/2) l


let rec closestBoundY from maxY (ptsByY : point list) =
  match ptsByY with
  | [] -> None
  | hd :: tl ->
    if hd.y > maxY then
      None
    else
      let toHd = Some(from, hd) in
      let bestToRest = closestBoundY from maxY tl in
      shortest toHd bestToRest


let rec closestInRange ptsByY maxDy =
  match ptsByY with
  | [] -> None
  | hd :: tl ->
    let fromHd = closestBoundY hd (hd.y +. maxDy) tl in
    let fromRest = closestInRange tl maxDy in
    shortest fromHd fromRest


let rec closestPairByX (ptsByX : point list) =
   if List.length ptsByX < 2 then
       None
   else
       let (left, right) = halve ptsByX in
       let leftResult = closestPairByX left in
       let rightResult = closestPairByX right in

       let bestInHalf = shortest  leftResult rightResult in
       let bestLength = dist bestInHalf in

       let divideX = (List.hd right).x in
       let inBand = List.filter(fun(p) -> abs_float(p.x -. divideX) < bestLength) ptsByX in

       let byY = List.sort cmpPointY inBand in
       let bestCross = closestInRange byY bestLength in
       shortest bestInHalf bestCross


let closestPair pts =
  let ptsByX = List.sort cmpPointX pts in
  closestPairByX ptsByX


let parsePoint str =
  let sep = Str.regexp_string "," in
  let tokens = Str.split sep str in
  let xStr = List.nth tokens 0 in
  let yStr = List.nth tokens 1 in

  let xVal = (float_of_string xStr) in
  let yVal = (float_of_string yStr) in

  { x = xVal; y = yVal }


let loadPoints filename =
  let ic = open_in filename in
  let result = ref [] in
  try
    while true do
      let s = input_line ic in
      if s <> "" then
        let p = parsePoint s in
        result := p :: !result;
    done;
    !result
  with End_of_file ->
    close_in ic;
    !result
;;

let loaded = (loadPoints "Points.txt") in
let start = Sys.time() in
let c = closestPair loaded in
let taken = Sys.time() -. start in
Printf.printf "Took %f [s]\n" taken;

match c with
| None -> Printf.printf "No closest pair\n"
| Some(seg) ->
  let a = fst seg in
  let b = snd seg in

  Printf.printf "(%f, %f) (%f, %f) Dist %f\n" a.x a.y b.x b.y (dist c)
