type 'a huffman_tree =
  | Leaf of 'a
  | Node of 'a huffman_tree * 'a huffman_tree

module HSet = Set.Make
  (struct
     type t = int * char huffman_tree (* pair of frequency and the tree *)
     let compare = compare
       (* We can use the built-in compare function to order this: it will order
          first by the first element (frequency) and then by the second (the tree),
          the latter of which we don't care about but which helps prevent elements
          from being equal, since Set does not allow duplicate elements *)
   end);;

let build_tree charFreqs =
  let leaves = List.fold_left (fun z (c,f) -> HSet.add (f, Leaf c) z) HSet.empty charFreqs in
  let rec aux trees =
    let f1, a = HSet.min_elt trees in
    let trees' = HSet.remove (f1,a) trees in
    if HSet.is_empty trees' then
      a
    else
      let f2, b = HSet.min_elt trees' in
      let trees'' = HSet.remove (f2,b) trees' in
      let trees''' = HSet.add (f1 + f2, Node (a, b)) trees'' in
      aux trees'''
  in
  aux leaves

let rec print_tree code = function
  | Leaf c ->
      Printf.printf "%c\t%s\n" c (String.concat "" (List.rev code));
  | Node (l, r) ->
      print_tree ("0"::code) l;
      print_tree ("1"::code) r

let () =
  let str = "this is an example for huffman encoding" in
  let charFreqs = Hashtbl.create 42 in
  String.iter (fun c ->
      let old =
        try Hashtbl.find charFreqs c
        with Not_found -> 0 in
      Hashtbl.replace charFreqs c (old+1)
    ) str;

  let charFreqs = Hashtbl.fold (fun c f acc -> (c,f)::acc) charFreqs [] in
  let tree = build_tree charFreqs in
  print_string "Symbol\tHuffman code\n";
  print_tree [] tree
