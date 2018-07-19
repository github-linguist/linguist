# infinity;;
- : float = infinity
# neg_infinity;;
- : float = neg_infinity
# nan;;
- : float = nan
# -0.;;
- : float = -0.
# -. 0.;;
- : float = -0.
# 1. /. 0.;;
- : float = infinity
# -1. /. 0.;;
- : float = neg_infinity
# -. infinity;;
- : float = neg_infinity
# infinity +. neg_infinity;;
- : float = nan
# 0. /. 0.;;
- : float = nan
# infinity /. infinity;;
- : float = nan
# nan = nan;;
- : bool = false
# nan == nan;;
- : bool = true
# 0. *. infinity;;
- : float = nan
# 0. = -0.;;
- : bool = true
# 0. == -0.;;
- : bool = false
