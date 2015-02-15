# let s = "ABCDEFGH" ;;
val s : string = "ABCDEFGH"

# let n, m = 2, 3 ;;
val n : int = 2
val m : int = 3

# String.sub s n m ;;
- : string = "CDE"

# String.sub s n (String.length s - n) ;;
- : string = "CDEFGH"

# String.sub s 0 (String.length s - 1) ;;
- : string = "ABCDEFG"

# String.sub s (String.index s 'D') m ;;
- : string = "DEF"

# #load "str.cma";;
# let n = Str.search_forward (Str.regexp_string "DE") s 0 in
  String.sub s n m ;;
- : string = "DEF"
