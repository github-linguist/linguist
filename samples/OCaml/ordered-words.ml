let input_line_opt ic =
  try Some(input_line ic)
  with End_of_file -> None

(* load each line in a list *)
let read_lines ic =
  let rec aux acc =
    match input_line_opt ic with
    | Some line -> aux (line :: acc)
    | None -> (List.rev acc)
  in
  aux []

let char_list_of_string str =
  let lst = ref [] in
  String.iter (fun c -> lst := c :: !lst) str;
  (List.rev !lst)

let is_ordered word =
  let rec aux = function
    | c1::c2::tl ->
        if c1 <= c2
        then aux (c2::tl)
        else false
    | c::[] -> true
    | [] -> true  (* should only occur with an empty string *)
  in
  aux (char_list_of_string word)

let longest_words words =
  let res, _ =
    List.fold_left
      (fun (lst, n) word ->
        let len = String.length word in
        let comp = compare len n in
        match lst, comp with
        | lst, 0  -> ((word::lst), n) (* len = n *)
        | lst, -1 -> (lst, n)         (* len < n *)
        | _, 1    -> ([word], len)    (* len > n *)
        | _ -> assert false
      )
      ([""], 0) words
  in
  (List.rev res)

let () =
  let ic = open_in "unixdict.txt" in
  let words = read_lines ic in
  let lower_words = List.map String.lowercase words in
  let ordered_words = List.filter is_ordered lower_words in
  let longest_ordered_words = longest_words ordered_words in
  List.iter print_endline longest_ordered_words
