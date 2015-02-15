open Unix

let months = [| "January"; "February"; "March"; "April"; "May"; "June";
      "July"; "August"; "September"; "October"; "November"; "December" |]

let () =
  let t = Unix.gmtime 0.0 in
  Printf.printf "%s %d, %d\n" months.(t.tm_mon) t.tm_mday (1900 + t.tm_year)
