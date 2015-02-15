#load "unix.cma";;
let pid = Unix.fork ();;
if pid > 0 then
  print_endline "This is the original process"
else
  print_endline "This is the new process";;
