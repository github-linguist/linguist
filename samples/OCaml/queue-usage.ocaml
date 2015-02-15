# let q = Queue.create ();;
val q : '_a Queue.t = <abstr>
# Queue.is_empty q;;
- : bool = true
# Queue.add 1 q;;
- : unit = ()
# Queue.is_empty q;;
- : bool = false
# Queue.add 2 q;;
- : unit = ()
# Queue.add 3 q;;
- : unit = ()
# Queue.peek q;;
- : int = 1
# Queue.length q;;
- : int = 3
# Queue.iter (Printf.printf "%d, ") q; print_newline ();;
1, 2, 3,
- : unit = ()
# Queue.take q;;
- : int = 1
# Queue.take q;;
- : int = 2
# Queue.peek q;;
- : int = 3
# Queue.length q;;
- : int = 1
# Queue.add 4 q;;
- : unit = ()
# Queue.take q;;
- : int = 3
# Queue.peek q;;
- : int = 4
# Queue.take q;;
- : int = 4
# Queue.is_empty q;;
- : bool = true
