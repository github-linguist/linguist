# for i = 1 to 10 do
    try
      print_int i;
      if (i mod 5) = 0 then raise Exit;
      print_string ", "
    with Exit ->
      print_newline()
  done
  ;;
1, 2, 3, 4, 5
6, 7, 8, 9, 10
- : unit = ()
