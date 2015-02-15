let dep_libs = [
  ("des_system_lib", ["std"; "synopsys"; "std_cell_lib"; "des_system_lib"; "dw02"; "dw01"; "ramlib"; "ieee"]);
  ("dw01",           (*"dw04"::*)["ieee"; "dw01"; "dware"; "gtech"]);
  ("dw02",           ["ieee"; "dw02"; "dware"]);
  ("dw03",           ["std"; "synopsys"; "dware"; "dw03"; "dw02"; "dw01"; "ieee"; "gtech"]);
  ("dw04",           ["dw04"; "ieee"; "dw01"; "dware"; "gtech"]);
  ("dw05",           ["dw05"; "ieee"; "dware"]);
  ("dw06",           ["dw06"; "ieee"; "dware"]);
  ("dw07",           ["ieee"; "dware"]);
  ("dware",          ["ieee"; "dware"]);
  ("gtech",          ["ieee"; "gtech"]);
  ("ramlib",         ["std"; "ieee"]);
  ("std_cell_lib",   ["ieee"; "std_cell_lib"]);
  ("synopsys",       []);
]

let dep_libs =
  let f (lib, deps) =  (* remove self dependency *)
    (lib,
     List.filter (fun d -> d <> lib) deps) in
  List.map f dep_libs

let rev_unique =
  List.fold_left (fun acc x -> if List.mem x acc then acc else x::acc) []

let libs =  (* list items, each being unique *)
  rev_unique (List.flatten(List.map (fun (lib, deps) -> lib::deps) dep_libs))

let get_deps lib =
  try (List.assoc lib dep_libs)
  with Not_found -> []

let res =
  let rec aux acc later todo progress =
  match todo, later with
  | [], [] -> (List.rev acc)
  | [], _ ->
      if progress
      then aux acc [] later false
      else invalid_arg "un-orderable data"
  | x::xs, _ ->
      let deps = get_deps x in
      let ok = List.for_all (fun dep -> List.mem dep acc) deps in
      if ok
      then aux (x::acc) later xs true
      else aux acc (x::later) xs progress
  in
  let starts, todo = List.partition (fun lib -> get_deps lib = []) libs in
  aux starts [] todo false

let () =
  print_string "result: \n ";
  print_endline (String.concat ", " res);
;;
