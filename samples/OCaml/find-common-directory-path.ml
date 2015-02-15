let rec aux acc paths =
  if List.mem [] paths
  then (List.rev acc) else
  let heads = List.map List.hd paths in
  let item = List.hd heads in
  let all_the_same =
    List.for_all ((=) item) (List.tl heads)
  in
  if all_the_same
  then aux (item::acc) (List.map List.tl paths)
  else (List.rev acc)

let common_prefix sep = function
  | [] -> invalid_arg "common_prefix"
  | dirs ->
      let paths = List.map (Str.split (Str.regexp_string sep)) dirs in
      let res = aux [] paths in
      (sep ^ (String.concat sep res))

let () =
  let dirs = [
    "/home/user1/tmp/coverage/test";
    "/home/user1/tmp/covert/operator";
    "/home/user1/tmp/coven/members";
  ] in
  print_endline (common_prefix "/" dirs);
;;
