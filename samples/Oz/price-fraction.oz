fun {PriceFraction X}
   OutOfRange = {Value.failed outOfRange(X)}
in
   for Limit#Result in
      [0.00#OutOfRange
       0.06#0.10 0.11#0.18 0.16#0.26 0.21#0.32 0.26#0.38 0.31#0.44 0.36#0.5
       0.41#0.54 0.46#0.58 0.51#0.62 0.56#0.66 0.61#0.70 0.66#0.74 0.71#0.78
       0.76#0.82 0.81#0.86 0.86#0.90 0.91#0.94 0.96#0.98 1.01#1.00
      ]
      return:Return
      default:OutOfRange
   do
      if X < Limit then {Return Result} end
   end
end
