let i = ref 42 (* initial value doesn't matter *)

let sum' i lo hi term =
  let result = ref 0. in
    i := lo;
    while !i <= hi do
      result := !result +. term ();
      incr i
    done;
    !result

let () =
  Printf.printf "%f\n" (sum' i 1 100 (fun () -> 1. /. float !i))
