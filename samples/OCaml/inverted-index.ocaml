TYPE_CONV_PATH "Inverted_index"

type files = string array with sexp
type inverted_index = (string * int list) list with sexp

type t = files * inverted_index with sexp

open Sexplib

let data_file = "data.inv"
let data_path = Filename.concat Filename.temp_dir_name data_file

let get_inv_index() =
  if Sys.file_exists data_path
  then t_of_sexp(Sexp.load_sexp data_path)
  else ([| |], [])

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let array_push ar v =
  let len = Array.length ar in
  Array.init (succ len) (fun i ->
    if i < len then Array.unsafe_get ar i else v), len

let uniq lst =
  let h = Hashtbl.create (List.length lst) in
  List.iter (fun x -> Hashtbl.replace h x ()) lst;
  Hashtbl.fold (fun x () xs -> x :: xs) h []

let combine words i inv_index =
  let h = Hashtbl.create (List.length inv_index) in
  List.iter (fun (w, from) -> Hashtbl.replace h w from) inv_index;
  List.iter (fun w ->
    let from =
      try Hashtbl.find h w
      except Not_found -> []
    in
    Hashtbl.replace h w (i::from)
  ) words;
  Hashtbl.fold (fun w from acc -> (w, from) :: acc) h []

let words_of_file in_file =
  let str = load_file in_file in
  let words = Str.split (Str.regexp "[ \r\n\t,;.?!:'/\034()]") str in
  let words = uniq words in
  (words)

let index_file in_file =
  let words = words_of_file in_file in
  let files, inv_index = get_inv_index() in
  let files, i = array_push files in_file in
  let inv_index = combine words i inv_index in
  let se = sexp_of_t (files, inv_index) in
  Sexp.save data_path se

let search_word word =
  let files, inv_index = get_inv_index() in
  try
    let is_in = List.assoc word inv_index in
    List.iter (fun i -> print_endline files.(i)) is_in
  with Not_found ->
    print_endline "# Not Found"

let usage() =
  Printf.printf "Usage: %s \
    --index-file <file.txt> / \
    --search-word <some-word>\n%!" Sys.argv.(0);
  exit 1

let () =
  let cmd, arg = try (Sys.argv.(1), Sys.argv.(2)) with _ -> usage() in
  match cmd, arg with
  | "--index-file", in_file -> index_file in_file
  | "--search-word", word -> search_word word
  | _ -> usage()
