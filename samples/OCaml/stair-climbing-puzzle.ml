let rec step_up() =
  while not(step()) do
    step_up()
  done
;;
