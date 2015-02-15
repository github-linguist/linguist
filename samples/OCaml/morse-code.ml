let codes = [
  'a', ".-";     'b', "-...";   'c', "-.-.";
  'd', "-..";    'e', ".";      'f', "..-.";
  'g', "--.";    'h', "....";   'i', "..";
  'j', ".---";   'k', "-.-";    'l', ".-..";
  'm', "--";     'n', "-.";     'o', "---";
  'p', ".--.";   'q', "--.-";   'r', ".-.";
  's', "...";    't', "-";      'u', "..-";
  'v', "...-";   'w', ".--";    'x', "-..-";
  'y', "-.--";   'z', "--..";   '0', "-----";
  '1', ".----";  '2', "..---";  '3', "...--";
  '4', "....-";  '5', ".....";  '6', "-....";
  '7', "--...";  '8', "---..";  '9', "----.";
]

let oc = open_out "/dev/dsp"

let bip u =
  for i = 0 to pred u do
    let j = sin(0.6 *. (float i)) in
    let k = ((j +. 1.0) /. 2.0) *. 127.0 in
    output_byte oc (truncate k)
  done

let gap u =
  for i = 0 to pred u do
    output_byte oc 0
  done

let morse =
  let u = 1000 in  (* length of one unit *)
  let u2 = u * 2 in
  let u3 = u * 3 in
  let u6 = u * 6 in
  String.iter (function
  | ' ' -> gap u6
  | 'a'..'z' | 'A'..'Z' | '0'..'9' as c ->
      let s = List.assoc c codes in
      String.iter (function
        '.' -> bip u; gap u
      | '-' -> bip u3; gap u
      | _ -> assert false
      ) s; gap u2
  | _ -> prerr_endline "unknown char")

let () = morse "rosettacode morse"
