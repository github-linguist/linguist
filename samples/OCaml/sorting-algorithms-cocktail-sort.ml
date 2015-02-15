let swap a i j =
  let tmp = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- tmp;
;;

let cocktail_sort a =
  let begin_ = ref(-1)
  and end_ = ref(Array.length a - 2) in
  let swapped = ref true in
  try while !swapped do
    swapped := false;
    incr begin_;
    for i = !begin_ to !end_ do
      if a.(i) > a.(i+1) then begin
        swap a (i) (i+1);
        swapped := true;
      end;
    done;
    if !swapped = false then raise Exit;
    swapped := false;
    decr end_;
    for i = !end_ downto !begin_ do
      if a.(i) > a.(i+1) then begin
        swap a (i) (i+1);
        swapped := true
      end;
    done;
  done with Exit -> ()
;;

let () =
  let a = [| 3; 7; 4; 9; 6; 1; 8; 5; 2; |] in
  cocktail_sort a;
  Array.iter (Printf.printf " %d") a;
  print_newline();
;;
