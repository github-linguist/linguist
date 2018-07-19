#! /usr/bin/env ocaml
#load "unix.cma"

let notes_file = "notes.txt"

let take_notes() =
  let gmt = Unix.gmtime (Unix.time()) in
  let date =
    Printf.sprintf "%d-%02d-%02d %02d:%02d:%02d"
      (1900 + gmt.Unix.tm_year) (1 + gmt.Unix.tm_mon) gmt.Unix.tm_mday
      gmt.Unix.tm_hour gmt.Unix.tm_min gmt.Unix.tm_sec
  in
  let oc = open_out_gen [Open_append; Open_creat; Open_text] 0o644 notes_file in
  output_string oc (date ^ "\t");
  output_string oc (String.concat " " (List.tl(Array.to_list Sys.argv)));
  output_string oc "\n";
;;

let dump_notes() =
  if not(Sys.file_exists notes_file)
  then (prerr_endline "no local notes found"; exit 1);
  let ic = open_in notes_file in
  try while true do
    print_endline (input_line ic)
  done with End_of_file ->
    close_in ic

let () =
  if Array.length Sys.argv = 1
  then dump_notes()
  else take_notes()
