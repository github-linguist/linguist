let input_line_opt ic =
  try Some (input_line ic)
  with End_of_file -> None

let cmp s1 s2 =
  try ignore(s1.[String.length s2]); 1     (* s1 is longer *)
  with _ ->
    try ignore(s2.[String.length s1]); -1  (* s2 is longer *)
    with _ -> 0                            (* both same length *)

let () =
  let ic = open_in Sys.argv.(1) in
  let rec loop longest acc =
    match input_line_opt ic with
    | Some line ->
      ( match cmp line longest with
        | 1 -> loop line (line ^ "\n")
        | 0 -> loop line (acc ^ line ^ "\n")
        | _ -> loop longest acc )
    | None ->
        close_in ic;
        print_string acc
  in
  loop "" ""
