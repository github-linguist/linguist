# let for_step a b step fn =
    let rec aux i =
      if i <= b then begin
        fn i;
        aux (i+step)
      end
    in
    aux a
  ;;
val for_step : int -> int -> int -> (int -> 'a) -> unit = <fun>

# for_step 0 8 2  (fun i -> Printf.printf " %d\n" i) ;;
 0
 2
 4
 6
 8
- : unit = ()
