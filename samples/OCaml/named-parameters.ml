# let foo ~arg1 ~arg2 = arg1 + arg2;;
val foo : arg1:int -> arg2:int -> int = <fun>
# let foo ~arg1:x ~arg2:y = x + y;; (* you can also use different variable names internally if you want *)
val foo : arg1:int -> arg2:int -> int = <fun>
# foo ~arg2:17 ~arg1:42;;
- : int = 59
