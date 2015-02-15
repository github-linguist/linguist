#load "unix.cma"
#directory "+ANSITerminal"
#load "ANSITerminal.cma"

module Trm = ANSITerminal

let () =
  Trm.erase Trm.Screen;
  Trm.set_cursor 3 6;
  Trm.print_string [] "Hello";
;;
