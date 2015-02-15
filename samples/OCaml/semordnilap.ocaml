module StrSet = Set.Make(String)

let str_rev s =
  let len = String.length s in
  let r = String.create len in
  for i = 0 to len - 1 do
    r.[i] <- s.[len - 1 - i]
  done;
  (r)

let input_line_opt ic =
  try Some (input_line ic)
  with End_of_file -> close_in ic; None

let () =
  let ic = open_in "unixdict.txt" in
  let rec aux set acc =
    match input_line_opt ic with
    | Some word ->
        let rev = str_rev word in
        if StrSet.mem rev set
        then aux set ((word, rev) :: acc)
        else aux (StrSet.add word set) acc
    | None ->
        (acc)
  in
  let pairs = aux StrSet.empty [] in
  let len = List.length pairs in
  Printf.printf "Semordnilap pairs: %d\n" len;
  Random.self_init ();
  for i = 1 to 5 do
    let (word, rev) = List.nth pairs (Random.int len) in
    Printf.printf " %s %s\n" word rev
  done
