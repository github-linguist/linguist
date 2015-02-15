let input_line_opt ic =
  try Some (input_line ic)
  with End_of_file -> None

let nth_line n filename =
  let ic = open_in filename in
  let rec aux i =
    match input_line_opt ic with
    | Some line ->
        if i = n then begin
          close_in ic;
          (line)
        end else aux (succ i)
    | None ->
        close_in ic;
        failwith "end of file reached"
  in
  aux 1

let () =
  print_endline (nth_line 7 Sys.argv.(1))
