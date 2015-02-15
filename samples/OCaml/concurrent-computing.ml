#directory "+threads"
#load "unix.cma"
#load "threads.cma"

let sleepy_print msg =
  Unix.sleep (Random.int 4);
  print_endline msg

let threads =
  List.map (Thread.create sleepy_print) ["Enjoy"; "Rosetta"; "Code"]

let () =
  Random.self_init ();
  List.iter (Thread.join) threads
