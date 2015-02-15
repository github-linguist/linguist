let srnd x =
  (* since OCaml's built-in int type is at least 31 (note: not 32) bits wide,
     and this problem takes mod 2^31, it is just enough if we treat it as
     an unsigned integer, which means taking the logical right shift *)
  let seed = ref x in
  fun () ->
    seed := (!seed * 214013 + 2531011) land 0x7fffffff;
    !seed lsr 16

let deal s =
  let rnd = srnd s in
  let t = Array.init 52 (fun i -> i) in
  let cards =
    Array.init 52 (fun j ->
      let n = 52 - j in
      let i = rnd() mod n in
      let this = t.(i) in
      t.(i) <- t.(pred n);
      this)
  in
  (cards)

let show cards =
  let suits = "CDHS"
  and nums = "A23456789TJQK" in
  Array.iteri (fun i card ->
    Printf.printf "%c%c%c"
      nums.[card / 4]
      suits.[card mod 4]
      (if (i mod 8) = 7 then '\n' else ' ')
  ) cards;
  print_newline()

let () =
  let s =
    try int_of_string Sys.argv.(1)
    with _ -> 11982
  in
  Printf.printf "Deal %d:\n" s;
  let cards = deal s in
  show cards
