let is_control_code c =
  let d = int_of_char c in
  d < 32 || d = 127

let is_extended_char c =
  let d = int_of_char c in
  d > 127

let strip f str =
  let len = String.length str in
  let res = String.create len in
  let rec aux i j =
    if i >= len then String.sub res 0 j else
    if f str.[i]
    then aux (succ i) j
    else begin
      res.[j] <- str.[i];
      aux (succ i) (succ j)
    end
  in
  aux 0 0

let () =
  let len = 32 in
  let s = String.create len in
  Random.self_init();
  for i = 0 to pred len do
    s.[i] <- char_of_int (Random.int 256)
  done;
  print_endline (strip is_control_code s);
  print_endline (strip (fun c -> (is_control_code c) || (is_extended_char c)) s);
;;
