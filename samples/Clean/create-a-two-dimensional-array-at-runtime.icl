import StdEnv

Start :: *World -> { {Real} }
Start world
    # (console, world) = stdio world
      (_, dim1, console) = freadi console
      (_, dim2, console) = freadi console
    = createArray dim1 (createArray dim2 1.0)
