let rms a =
  sqrt (Array.fold_left (fun s x -> s +. x*.x) 0.0 a /.
        float_of_int (Array.length a))
;;

rms (Array.init 10 (fun i -> float_of_int (i+1))) ;;
(* 6.2048368229954285 *)
