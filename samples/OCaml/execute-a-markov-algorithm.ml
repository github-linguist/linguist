(* Useful for resource cleanup (such as filehandles) *)
let try_finally x f g =
  try let res = f x in g x; res
  with e -> g x; raise e

(* Substitute string 'b' for first occurance of regexp 'a' in 's';
 * Raise Not_found if there was no occurance of 'a'. *)
let subst a b s =
  ignore (Str.search_forward a s 0); (* to generate Not_found *)
  Str.replace_first a b s

let parse_rules cin =
  let open Str in
  let rule = regexp "\\(.+\\)[ \t]+->[ \t]+\\(.*\\)" in
  let leader s c = String.length s > 0 && s.[0] = c in
  let parse_b s = if leader s '.' then (string_after s 1,true) else (s,false) in
  let rec parse_line rules =
    try
      let s = input_line cin in
      if leader s '#' then parse_line rules
      else if string_match rule s 0 then
        let a = regexp_string (matched_group 1 s) in
        let b,terminate = parse_b (matched_group 2 s) in
        parse_line ((a,b,terminate)::rules)
      else failwith ("parse error: "^s)
    with End_of_file -> rules
  in List.rev (parse_line [])

let rec run rules text =
  let rec apply s = function
    | [] -> s
    | (a,b,term)::next ->
        try
          let s' = subst a b s in
          if term then s' else run rules s'
        with Not_found -> apply s next
  in apply text rules

let _ =
  if Array.length Sys.argv <> 2 then
    print_endline "Expecting one argument: a filename where rules can be found."
  else
    let rules = try_finally (open_in Sys.argv.(1)) parse_rules close_in in
    (* Translate lines read from stdin, until EOF *)
    let rec translate () =
      print_endline (run rules (input_line stdin));
      translate ()
    in try translate () with End_of_file -> ()
