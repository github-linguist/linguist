# let find_index pred lst =
    let rec loop n = function
       []    -> raise Not_found
     | x::xs -> if pred x then n
                          else loop (n+1) xs
    in
    loop 0 lst;;
val find_index : ('a -> bool) -> 'a list -> int = <fun>

# let haystack =
    ["Zig";"Zag";"Wally";"Ronald";"Bush";"Krusty";"Charlie";"Bush";"Bozo"];;
val haystack : string list =
  ["Zig"; "Zag"; "Wally"; "Ronald"; "Bush"; "Krusty"; "Charlie"; "Bush";
   "Bozo"]
# List.iter (fun needle ->
               try
                 Printf.printf "%i %s\n" (find_index ((=) needle) haystack) needle
               with Not_found ->
                 Printf.printf "%s is not in haystack\n" needle)
            ["Washington"; "Bush"];;
Washington is not in haystack
4 Bush
- : unit = ()
