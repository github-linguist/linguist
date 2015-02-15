let char2value c =
  assert (not (String.contains "AEIOU" c));
  match c with
     '0'..'9' -> int_of_char c - int_of_char '0'
   | 'A'..'Z' -> int_of_char c - int_of_char 'A' + 10

let sedolweight = [1;3;1;7;3;9]

let explode s =
  let result = ref [] in
  String.iter (fun c -> result := c :: !result) s;
  List.rev !result

let checksum sedol =
  let tmp = List.fold_left2 (fun sum ch weight -> sum + char2value ch * weight)
              0 (explode sedol) sedolweight in
    string_of_int ((10 - (tmp mod 10)) mod 10)

List.iter (fun sedol -> print_endline (sedol ^ checksum sedol))
  [ "710889";
    "B0YBKJ";
    "406566";
    "B0YBLH";
    "228276";
    "B0YBKL";
    "557910";
    "B0YBKR";
    "585284";
    "B0YBKT" ]
