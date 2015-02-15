let men = [
  "abe",  ["abi";"eve";"cath";"ivy";"jan";"dee";"fay";"bea";"hope";"gay"];
  "bob",  ["cath";"hope";"abi";"dee";"eve";"fay";"bea";"jan";"ivy";"gay"];
  "col",  ["hope";"eve";"abi";"dee";"bea";"fay";"ivy";"gay";"cath";"jan"];
  "dan",  ["ivy";"fay";"dee";"gay";"hope";"eve";"jan";"bea";"cath";"abi"];
  "ed",   ["jan";"dee";"bea";"cath";"fay";"eve";"abi";"ivy";"hope";"gay"];
  "fred", ["bea";"abi";"dee";"gay";"eve";"ivy";"cath";"jan";"hope";"fay"];
  "gav",  ["gay";"eve";"ivy";"bea";"cath";"abi";"dee";"hope";"jan";"fay"];
  "hal",  ["abi";"eve";"hope";"fay";"ivy";"cath";"jan";"bea";"gay";"dee"];
  "ian",  ["hope";"cath";"dee";"gay";"bea";"abi";"fay";"ivy";"jan";"eve"];
  "jon",  ["abi";"fay";"jan";"gay";"eve";"bea";"dee";"cath";"ivy";"hope"];
]

let women = [
  "abi",  ["bob";"fred";"jon";"gav";"ian";"abe";"dan";"ed";"col";"hal"];
  "bea",  ["bob";"abe";"col";"fred";"gav";"dan";"ian";"ed";"jon";"hal"];
  "cath", ["fred";"bob";"ed";"gav";"hal";"col";"ian";"abe";"dan";"jon"];
  "dee",  ["fred";"jon";"col";"abe";"ian";"hal";"gav";"dan";"bob";"ed"];
  "eve",  ["jon";"hal";"fred";"dan";"abe";"gav";"col";"ed";"ian";"bob"];
  "fay",  ["bob";"abe";"ed";"ian";"jon";"dan";"fred";"gav";"col";"hal"];
  "gay",  ["jon";"gav";"hal";"fred";"bob";"abe";"col";"ed";"dan";"ian"];
  "hope", ["gav";"jon";"bob";"abe";"ian";"dan";"hal";"ed";"col";"fred"];
  "ivy",  ["ian";"col";"hal";"gav";"fred";"bob";"abe";"ed";"jon";"dan"];
  "jan",  ["ed";"hal";"gav";"abe";"bob";"jon";"col";"ian";"fred";"dan"];
]

type woman_name = string
type man_name = string

type man =
  { m_name: man_name;
    mutable free: bool;
    women_rank: woman_name list;
    has_proposed: (woman_name, unit) Hashtbl.t (* a set *)
  }

type woman =
  { w_name: woman_name;
    men_rank: man_name list;
    mutable engaged: man_name option
  }


let prefers w m1 m2 =
  (* returns true if w has a lower (better) rank for m1 than m2 *)
  let rec aux = function
  | [] -> invalid_arg "rank_cmp"
  | x::_ when x = m1 -> true
  | x::_ when x = m2 -> false
  | _::xs -> aux xs
  in
  aux w.men_rank

let take_while f lst =
  let rec aux acc = function
  | x::xs when f x -> aux (x::acc) xs
  | _ -> List.rev acc
  in
  aux [] lst

let more_ranked_than name =
  take_while ((<>) name)

let build_structs ~men ~women =
  List.map (fun (name, rank) ->
    { m_name = name;
      women_rank = rank;
      free = true;
      has_proposed = Hashtbl.create 42 }
  ) men,
  List.map (fun (name, rank) ->
    { w_name = name;
      men_rank = rank;
      engaged = None }
  ) women


let _stable_matching ms ws =
  let men_by_name = Hashtbl.create 42 in
  List.iter (fun m -> Hashtbl.add men_by_name m.m_name m) ms;
  let women_by_name = Hashtbl.create 42 in
  List.iter (fun w -> Hashtbl.add women_by_name w.w_name w) ws;
  try
    while true do
      (*TODO free men who still has some w to propose to *)
      let m = List.find (fun m -> m.free) ms in
      (* highest ranked woman who the man has not proposed to yet *)
      let w_name =
        List.find (fun w -> not (Hashtbl.mem m.has_proposed w)) m.women_rank in
      Hashtbl.add m.has_proposed w_name ();
      let w = Hashtbl.find women_by_name w_name in
      match w.engaged with
      | None -> (* w is free *)
          (* (m, w) become engaged *)
          w.engaged <- Some m.m_name;
          m.free <- false
      | Some m'_name -> (* some pair (m', w) already exists *)
          if prefers w m.m_name m'_name
          then begin (* w prefers m to m' *)
            w.engaged <- Some m.m_name;
            let m' = Hashtbl.find men_by_name m'_name in
            m'.free <- true;
            m.free <- false
          end
    done;
    assert false
  with Not_found -> ()


let stable_matching ~men ~women =
  let ms, ws = build_structs ~men ~women in
  _stable_matching ms ws;
  let some = function Some v -> v | None -> "" in
  List.map (fun w -> w.w_name, some w.engaged) ws


let is_stable ~men ~women eng =
  let ms, ws = build_structs ~men ~women in
  not (List.exists (fun (wn, mn) ->
      let m = List.find (fun m -> m.m_name = mn) ms in
      let prefered_women = more_ranked_than wn m.women_rank in
      List.exists (fun pref_w ->
        let w = List.find (fun w -> w.w_name = pref_w) ws in
        let eng_m = List.assoc pref_w eng in
        let prefered_men = more_ranked_than eng_m w.men_rank in
        List.mem m.m_name prefered_men (* exists unstable engagement *)
      ) prefered_women
    ) eng)


let perturb_engagements eng =
  Random.self_init();
  let eng = Array.of_list eng in
  let len = Array.length eng in
  for n = 1 to 3 do
    let i = Random.int len
    and j = Random.int len in
    let w1, m1 = eng.(i)
    and w2, m2 = eng.(j) in
    eng.(i) <- (w1, m2);
    eng.(j) <- (w2, m1);
  done;
  Array.to_list eng


let print engs =
  List.iter (fun (w,m) ->
    Printf.printf " %4s is engaged with %s\n" w m) engs;
  Printf.printf "# Engagements %s stable\n"
    (if is_stable ~men ~women engs then "are" else "are not")

let () =
  let engagements = stable_matching ~men ~women in
  print engagements;
  print_endline "========================";
  let engagements = perturb_engagements engagements in
  print engagements;
;;
