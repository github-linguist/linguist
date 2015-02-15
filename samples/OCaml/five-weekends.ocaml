open CalendarLib

let list_first_five = function
  | x1 :: x2 :: x3 :: x4 :: x5 :: _ -> [x1; x2; x3; x4; x5]
  | _ -> invalid_arg "list_first_five"

let () =
  let months = ref [] in
  for year = 1900 to 2100 do
    for month = 1 to 12 do
      let we = ref 0 in
      let num_days = Date.days_in_month (Date.make_year_month year month) in
      for day = 1 to num_days - 2 do
        let d0 = Date.day_of_week (Date.make year month day)
        and d1 = Date.day_of_week (Date.make year month (day + 1))
        and d2 = Date.day_of_week (Date.make year month (day + 2)) in
        if (d0, d1, d2) = (Date.Fri, Date.Sat, Date.Sun) then incr we
      done;
      if !we = 5 then months := (year, month) :: !months
    done;
  done;
  Printf.printf "Number of months with 5 weekends: %d\n" (List.length !months);
  print_endline "First and last months between 1900 and 2100:";
  let print_month (year, month) = Printf.printf "%d-%02d\n" year month in
  List.iter print_month (list_first_five (List.rev !months));
  List.iter print_month (List.rev (list_first_five !months));
;;
