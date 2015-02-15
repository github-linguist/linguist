let seen = Hashtbl.create 7
let mark t = Hashtbl.add seen t true
let marked t = Hashtbl.mem seen t

let walls = Hashtbl.create 7
let ord a b = if a <= b then (a,b) else (b,a)
let join a b = Hashtbl.add walls (ord a b) true
let joined a b = Hashtbl.mem walls (ord a b)

let () =
  let nx = int_of_string Sys.argv.(1) in
  let ny = int_of_string Sys.argv.(2) in

  let rec random_order = function
    | [] -> []
    | [a] -> [a]
    | x -> let i = Random.int (List.length x) in
      let rec del i = function
        | [] -> failwith "del"
        | h::t -> if i = 0 then t else h :: del (i-1) t in
      (List.nth x i) :: random_order (del i x) in

  let get_neighbours (x,y) =
    let lim n k = (0 <= k) && (k < n) in
    let bounds (x,y) = lim nx x && lim ny y in
    List.filter bounds [(x-1,y);(x+1,y);(x,y-1);(x,y+1)] in

  let rec visit cell =
    mark cell;
    let check k =
      if not (marked k) then (join cell k; visit k) in
    List.iter check (random_order (get_neighbours cell)) in

  let print_maze () =
    begin
    for i = 1 to nx do print_string "+---";done; print_endline "+";
    let line n j k l s t u =
      for i = 0 to n do print_string (if joined (i,j) (i+k,j+l) then s else t) done;
      print_endline u in
    for j = 0 to ny-2 do
      print_string "|   ";
      line (nx-2) j 1 0 "    " "|   " "|";
      line (nx-1) j 0 1 "+   " "+---" "+";
    done;
    print_string "|   ";
    line (nx-2) (ny-1) 1 0 "    " "|   " "|";
    for i = 1 to nx do print_string "+---";done; print_endline "+";
   end in

  Random.self_init();
  visit (Random.int nx, Random.int ny);
  print_maze ();
