let p = ref 1;; (* create a new "reference" data structure with initial value 1 *)
let k = !p;;    (* "dereference" the reference, returning the value inside *)
p := k + 1;;    (* set the value inside to a new value *)
