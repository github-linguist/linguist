let left_pos s len =
  let rec aux i =
    if i >= len then None
    else match s.[i] with
    | ' ' | '\n' | '\t' | '\r' -> aux (succ i)
    | _ -> Some i
  in
  aux 0

let right_pos s len =
  let rec aux i =
    if i < 0 then None
    else match s.[i] with
    | ' ' | '\n' | '\t' | '\r' -> aux (pred i)
    | _ -> Some i
  in
  aux (pred len)

let trim s =
  let len = String.length s in
  match left_pos s len, right_pos s len with
  | Some i, Some j -> String.sub s i (j - i + 1)
  | None, None -> ""
  | _ -> assert false

let ltrim s =
  let len = String.length s in
  match left_pos s len with
  | Some i -> String.sub s i (len - i)
  | None -> ""

let rtrim s =
  let len = String.length s in
  match right_pos s len with
  | Some i -> String.sub s 0 (i + 1)
  | None -> ""
