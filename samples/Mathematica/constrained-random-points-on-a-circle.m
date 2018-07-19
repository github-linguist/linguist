sample = Take[Cases[RandomInteger[{-15, 15}, {500, 2}], {x_, y_} /; 10 <= Sqrt[x^2 + y^2] <= 15], 100];

Show[{RegionPlot[10 <= Sqrt[x^2 + y^2] <= 15, {x, -16, 16}, {y, -16, 16}, Axes -> True], ListPlot[sample]}]
