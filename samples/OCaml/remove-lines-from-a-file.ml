let input_line_opt ic =
  try Some (input_line ic)
  with End_of_file -> None

let delete_lines filename start skip =
  if start < 1 || skip < 1 then
    invalid_arg "delete_lines";
  let tmp_file = filename ^ ".tmp" in
  let ic = open_in filename
  and oc = open_out tmp_file in
  let until = start + skip - 1 in
  let rec aux i =
    match input_line_opt ic with
    | Some line ->
        if i < start || i > until
        then (output_string oc line; output_char oc '\n');
        aux (succ i)
    | None ->
        close_in ic;
        close_out oc;
        Sys.remove filename;
        Sys.rename tmp_file filename
  in
  aux 1

let usage () =
  Printf.printf "Usage:\n%s <filename> <startline> <skipnumber>\n" Sys.argv.(0);
  exit 0

let () =
  if Array.length Sys.argv < 4 then usage ();
  delete_lines
    Sys.argv.(1) (int_of_string Sys.argv.(2)) (int_of_string Sys.argv.(3))
