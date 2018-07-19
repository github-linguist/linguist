#load "str.cma"
open Str

let input = "\
Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column."

let () =
  let lines = split (regexp_string "\n") input in
  let fields_l = List.map (split (regexp_string "$")) lines in
  let fields_l = List.map Array.of_list fields_l in
  let n = (* number of columns *)
    List.fold_left
      (fun n fields -> max n (Array.length fields))
      0 fields_l
  in
  let pads = Array.make n 0 in
  List.iter (
    (* calculate the max padding for each column *)
    Array.iteri
      (fun i word -> pads.(i) <- max pads.(i) (String.length word))
  ) fields_l;

  let print f =
    List.iter (fun fields ->
      Array.iteri (fun i word ->
        f word (pads.(i) - (String.length word))
      ) fields;
      print_newline()
    ) fields_l;
  in

  (* left column-aligned output *)
  print (fun word pad ->
    let spaces = String.make pad ' ' in
    Printf.printf "%s%s " word spaces);

  (* right column-aligned output *)
  print (fun word pad ->
    let spaces = String.make pad ' ' in
    Printf.printf "%s%s " spaces word);

  (* center column-aligned output *)
  print (fun word pad ->
    let pad1 = pad / 2 in
    let pad2 = pad - pad1 in
    let sp1 = String.make pad1 ' ' in
    let sp2 = String.make pad2 ' ' in
    Printf.printf "%s%s%s " sp1 word sp2);
;;
