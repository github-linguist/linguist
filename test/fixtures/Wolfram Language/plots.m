linspace[start_, stop_, count_] := Subdivide[start, stop, count - 1];
samples = linspace[0, 1, 101];
figure = ListLinePlot[samples];
