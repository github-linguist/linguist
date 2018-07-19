canvasdim = 1000;
n = 0.35*canvasdim^2;
canvas = ConstantArray[0, {canvasdim, canvasdim}];
init = Floor@(0.5*{canvasdim, canvasdim});  (*RandomInteger[canvasdim,2]*)
canvas[[init[[1]], init[[2]]]] = 1;         (*1st particle initialized to midpoint*)

Monitor[                                    (*Provides real-time intermediate result monitoring*)
 Do[
  particle = RandomInteger[canvasdim, 2];
  While[True,
   ds = RandomInteger[{-1, 1}, 2];
   While[                                   (*New Particle Domain Limit Section*)
    !And @@ (0 < (particle + ds)[[#]] <= canvasdim & /@ {1, 2}),
    particle = RandomInteger[canvasdim, 2];
    ];
                                            (* Particle Aggregation Section *)
   If[canvas[[(particle + ds)[[1]], (particle + ds)[[2]]]] > 0,
    canvas[[particle[[1]], particle[[2]]]] = i;
    Break[],
    particle += ds
    ];
   ],
  {i, n}],
 {i, (particle + ds), MatrixPlot@canvas}
 ]
MatrixPlot[canvas,FrameTicks->None,ColorFunction->"DarkRainbow",ColorRules->{0 -> None}]
