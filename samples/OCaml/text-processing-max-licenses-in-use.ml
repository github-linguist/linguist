let () =
  let out = ref 0 in
  let max_out = ref(-1) in
  let max_times = ref [] in

  let ic = open_in "mlijobs.txt" in
  try while true do
    let line = input_line ic in
    let io, date, n =
      Scanf.sscanf line
        "License %3[IN OUT] %_c %19[0-9/:_] for job %d"
        (fun io date n -> (io, date, n))
    in
    if io = "OUT" then incr out else decr out;
    if !out > !max_out then
    ( max_out := !out;
      max_times := [date]; )
    else if !out = !max_out then
      max_times := date :: !max_times;
  done
  with End_of_file ->
    close_in ic;
    Printf.printf
      "Maximum simultaneous license use is %d \
       at the following times:\n" !max_out;
    List.iter print_endline !max_times;
;;
