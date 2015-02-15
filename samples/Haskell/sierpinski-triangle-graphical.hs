import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

triangle = eqTriangle # fc black # lw 0

reduce t =     t
              ===
           (t ||| t)

sierpinski = iterate reduce triangle

main = defaultMain $ sierpinski !! 7
