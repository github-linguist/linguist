if2 :: Bool -> Bool -> a -> a -> a -> a -> a
if2 p1 p2 e12 e1 e2 e =
  if p1 then
    if p2 then e12 else e1
    else if p2 then e2 else e

main = print $ if2 True False (error "TT") "TF" (error "FT") (error "FF")
