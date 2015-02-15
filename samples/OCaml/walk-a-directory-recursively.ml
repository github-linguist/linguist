#!/usr/bin/env ocaml
#load "unix.cma"
#load "str.cma"
open Unix

let walk_directory_tree dir pattern =
  let select str = Str.string_match (Str.regexp pattern) str 0 in
  let rec walk acc = function
  | [] -> (acc)
  | dir::tail ->
      let contents = Array.to_list (Sys.readdir dir) in
      let contents = List.rev_map (Filename.concat dir) contents in
      let dirs, files =
        List.fold_left (fun (dirs,files) f ->
             match (stat f).st_kind with
             | S_REG -> (dirs, f::files)  (* Regular file *)
             | S_DIR -> (f::dirs, files)  (* Directory *)
             | _ -> (dirs, files)
          ) ([],[]) contents
      in
      let matched = List.filter (select) files in
      walk (matched @ acc) (dirs @ tail)
  in
  walk [] [dir]
;;

let () =
  let results = walk_directory_tree "/usr/local/lib/ocaml"  ".*\\.cma" in
  List.iter print_endline results;
;;
