open StdLabels

let to_string (name,_,s,_) = (Printf.sprintf "%s (%d)" name s)

let take n li =
  let rec aux i acc = function
  | _ when i >= n -> (List.rev acc)
  | [] -> (List.rev acc)
  | x::xs -> aux (succ i) (x::acc) xs
  in
  aux 0 [] li ;;

let toprank data n =
  let len = List.length data in
  let h = Hashtbl.create len in
  List.iter data ~f:(fun ((_,_,_,dep) as employee) ->
    Hashtbl.add h dep employee);
  let deps =
    List.fold_left data ~init:[] ~f:
      (fun ac (_,_,_,v) -> if List.mem v ac then ac else v::ac) in
  let f dep =
    Printf.printf "Department: %s\n " dep;
    let l = Hashtbl.find_all h dep in
    let l2 = List.sort (fun (_,_,v1,_) (_,_,v2,_) -> compare v2 v1) l in
    let l3 = (take n l2) in
    print_endline(String.concat ", " (List.map to_string l3));
    print_newline()
  in
  List.iter f deps;
;;

let data = [
  "Tyler Bennett",   "E10297",  32000,  "D101";
  "John Rappl",      "E21437",  47000,  "D050";
  "George Woltman",  "E00127",  53500,  "D101";
  "Adam Smith",      "E63535",  18000,  "D202";
  "Claire Buckman",  "E39876",  27800,  "D202";
  "David McClellan", "E04242",  41500,  "D101";
  "Rich Holcomb",    "E01234",  49500,  "D202";
  "Nathan Adams",    "E41298",  21900,  "D050";
  "Richard Potter",  "E43128",  15900,  "D101";
  "David Motsinger", "E27002",  19250,  "D202";
  "Tim Sampair",     "E03033",  27000,  "D101";
  "Kim Arlich",      "E10001",  57000,  "D190";
  "Timothy Grove",   "E16398",  29900,  "D190";
]

let () =
  toprank data 3;
;;
