let cipher src key crypt =
  let str = String.uppercase src in
  let key = String.uppercase key in

  (* strip out non-letters *)
  let len = String.length str in
  let rec aux i j =
    if j >= len then String.sub str 0 i else
    if str.[j] >= 'A' && str.[j] <= 'Z'
    then (str.[i] <- str.[j]; aux (succ i) (succ j))
    else aux i (succ j)
  in
  let res = aux 0 0 in

  let slen = String.length res in
  let klen = String.length key in

  let d = int_of_char in
  let f =
    if crypt
    then fun i -> d res.[i] - d 'A' + d key.[i mod klen] - d 'A'
    else fun i -> d res.[i] - d key.[i mod klen] + 26
  in
  for i = 0 to pred slen do
    res.[i] <- char_of_int (d 'A' + (f i) mod 26)
  done;
  (res)

let () =
  let str = "Beware the Jabberwock, my son! The jaws that bite, \
             the claws that catch!" in
  let key = "VIGENERECIPHER" in

  let cod = cipher str key true in
  let dec = cipher cod key false in

  Printf.printf "Text: %s\n" str;
  Printf.printf "key:  %s\n" key;
  Printf.printf "Code: %s\n" cod;
  Printf.printf "Back: %s\n" dec;
;;
