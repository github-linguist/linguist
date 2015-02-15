freq = 8; length = freq^(-1/2);
Animate[Graphics[
  List[{Line[{{0, 0}, length {Sin[T], -Cos[T]}} /. {T -> (Pi/6) Cos[2 Pi freq t]}], PointSize[Large],
               Point[{length {Sin[T], -Cos[T]}} /. {T -> (Pi/6) Cos[2 Pi freq t]}]}],
  PlotRange -> {{-0.3, 0.3}, {-0.5, 0}}], {t, 0, 1}, AnimationRate -> 0.07]
