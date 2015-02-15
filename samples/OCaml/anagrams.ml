let explode str =
  let l = ref [] in
  let n = String.length str in
  for i = n - 1 downto 0 do
    l := str.[i] :: !l
  done;
  (!l)

let implode li =
  let n = List.length li in
  let s = String.create n in
  let i = ref 0 in
  List.iter (fun c -> s.[!i] <- c; incr i) li;
  (s)

let () =
  let h = Hashtbl.create 3571 in
  let ic = open_in "unixdict.txt" in
  try while true do
    let w = input_line ic in
    let k = implode (List.sort compare (explode w)) in
    let l =
      try Hashtbl.find h k
      with Not_found -> []
    in
    Hashtbl.replace h k (w::l);
  done with End_of_file -> ();
  let n = Hashtbl.fold (fun _ lw n -> max n (List.length lw)) h 0 in
  Hashtbl.iter (fun _ lw ->
    if List.length lw >= n then
    ( List.iter (Printf.printf " %s") lw;
      print_newline () )
  ) h
