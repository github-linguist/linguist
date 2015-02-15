let dice5() = 1 + Random.int 5 ;;

let dice7 =
  let rolls2answer = Hashtbl.create 25 in
  let n = ref 0 in
  for roll1 = 1 to 5 do
    for roll2 = 1 to 5 do
      Hashtbl.add rolls2answer (roll1,roll2) (!n / 3 +1);
      incr n
    done;
  done;
  let rec aux() =
    let trial = Hashtbl.find rolls2answer (dice5(),dice5()) in
    if trial <= 7 then trial else aux()
  in
  aux
;;
