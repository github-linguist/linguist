let () =
  let ic = open_in Sys.argv.(1) in
  let base = int_of_char 'a' in
  let arr = Array.make 26 0 in
  try while true do
    let c = Char.lowercase(input_char ic) in
    let ndx = int_of_char c - base in
    if ndx < 26 && ndx >= 0 then
      arr.(ndx) <- succ arr.(ndx)
  done
  with End_of_file ->
    close_in ic;
    for i=0 to 25 do
      Printf.printf "%c -> %d\n" (char_of_int(i + base)) arr.(i)
    done
