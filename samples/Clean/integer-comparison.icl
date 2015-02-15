import StdEnv

compare a b
    | a < b = "A is less than B"
    | a > b = "A is more than B"
    | a == b = "A equals B"

Start world
    # (console, world) = stdio world
      (_, a, console) = freadi console
      (_, b, console) = freadi console
    = compare a b
