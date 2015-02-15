let f x = sqrt x +. 5.0 *. (x ** 3.0)
let p x = x < 400.0

let () =
  print_endline "Please enter 11 Numbers:";
  let lst = Array.to_list (Array.init 11 (fun _ -> read_float ())) in
  List.iter (fun x ->
    let res = f x in
    if p res
    then Printf.printf "f(%g) = %g\n%!" x res
    else Printf.eprintf "f(%g) :: Overflow\n%!" x
  ) (List.rev lst)
