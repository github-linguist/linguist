{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

yinyang = lw 0 $
          perim # lw 0.003 <>
          torus white black # xform id <>
          torus black white # xform negate <>
          clipBy perim base
  where perim      = arc 0 (360 :: Deg) # scale (1/2)
        torus c c' = circle (1/3) # fc c' <> circle 1 # fc c
        xform f    = translateY (f (1/4)) . scale (1/4)
        base       = rect (1/2) 1 # fc white ||| rect (1/2) 1 # fc black

main = defaultMain $
       pad 1.1 $
       beside (2,-1) yinyang (yinyang # scale (1/4))
