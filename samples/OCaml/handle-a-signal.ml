#load "unix.cma";; (* for sleep and gettimeofday; not needed for the signals stuff per se *)

let start = Unix.gettimeofday ();;

Sys.set_signal Sys.sigint
  (Sys.Signal_handle (fun _signum ->
                        Printf.printf "Ran for %f seconds.\n"
                          (Unix.gettimeofday () -. start);
                        exit 0));;

let rec loop n =
  Printf.printf "%d\n%!" n;
  Unix.sleep 1;
  loop (n + 1)
in
  loop 1;;
