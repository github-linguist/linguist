DynamicModule[{n = 0},
 CreateDialog[{Dynamic@
    TextCell@If[n == 0, "There have been no clicks yet", n],
   Button["click me", n++]}]]
