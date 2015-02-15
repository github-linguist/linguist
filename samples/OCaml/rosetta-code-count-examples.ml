open Http_client.Convenience


let repl_quote s =
  let reg = Str.regexp_string "&#039;" in
  (Str.global_replace reg "%27" s)

let repl_space s =
  let s = String.copy s in
  for i = 0 to pred(String.length s) do
    if s.[i] = ' ' then s.[i] <- '_'
  done;
  (s)
(* or in OCaml 4.00+:
   let repl_space = String.map (fun c -> if c = ' ' then '_' else c)
*)

let count_ex s =
  let pat = Str.regexp_string "=={{header|" in
  let rec aux n p =
    try
      let p = Str.search_forward pat s p in
      aux (n+1) (p+1)
    with Not_found -> (n)
  in
  aux 0 0

let get_child child xml =
  let child =
    List.find
      (function Xml.Element (tag,_,_) when tag = child -> true | _ -> false) xml
  in
  Xml.children child

let () =
  let url = "http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&\
               cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml" in

  let xml = Xml.parse_string (http_get url) in

  let total = ref 0 in
  at_exit (fun () -> Printf.printf "\n Total: %d\n" !total);

  let f = function
  | Xml.Element ("cm", attrs, _) ->
      (try
        let _title = List.assoc "title" attrs in
        let title = repl_quote (repl_space _title) in
        let url = "http://www.rosettacode.org/w/index.php?title="^ title ^"&action=raw" in
        let n = count_ex (http_get url) in
        Printf.printf "%s: %d\n%!" _title n;
        total := n + !total;
      with Http_client.Http_error (404, _) -> ())
  | _ -> ()
  in

  match xml with
  | Xml.Element ("api", _, ch) ->
      let query = get_child "query" ch in
      let catmb = get_child "categorymembers" query in
      List.iter f catmb
  | _ -> ()
