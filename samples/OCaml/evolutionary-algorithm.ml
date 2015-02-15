let target = "METHINKS IT IS LIKE A WEASEL"
let charset = "ABCDEFGHIJKLMNOPQRSTUVWXYZ "
let tlen = String.length target
let clen = String.length charset
let () = Random.self_init()

let parent =
  let s = String.create tlen in
  for i = 0 to tlen-1 do
    s.[i] <- charset.[Random.int clen]
  done;
  s

let fitness ~trial =
  let rec aux i d =
    if i >= tlen then d else
      aux (i+1) (if target.[i] = trial.[i] then d+1 else d) in
  aux 0 0

let mutate parent rate =
  let s = String.copy parent in
  for i = 0 to tlen-1 do
    if Random.float 1.0 > rate then
      s.[i] <- charset.[Random.int clen]
  done;
  s, fitness s

let () =
  let i = ref 0 in
  while parent <> target do
    let pfit = fitness parent in
    let rate = float pfit /. float tlen in
    let tries = Array.init 200 (fun _ -> mutate parent rate) in
    let min_by (a, fa) (b, fb) = if fa > fb then a, fa else b, fb in
    let best, f = Array.fold_left min_by (parent, pfit) tries in
    if !i mod 100 = 0 then
      Printf.printf "%5d - '%s'  (fitness:%2d)\n%!" !i best f;
    String.blit best 0 parent 0 tlen;
    incr i
  done;
  Printf.printf "%5d - '%s'\n" !i parent
