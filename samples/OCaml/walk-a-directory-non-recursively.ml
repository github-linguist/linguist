#load "str.cma"
let contents = Array.to_list (Sys.readdir ".") in
let select pat str = Str.string_match (Str.regexp pat) str 0 in
List.filter (select ".*\\.jpg") contents
