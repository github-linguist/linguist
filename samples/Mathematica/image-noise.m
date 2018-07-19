time = AbsoluteTime[]; Animate[
 Column[{Row[{"FPS: ", Round[n/(AbsoluteTime[] - time)]}],
   RandomImage[1, {320, 240}]}], {n, 1, Infinity, 1}]
