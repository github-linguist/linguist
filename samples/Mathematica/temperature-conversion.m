tempConvert[t_] :=
Grid[Transpose@{{"K", "C", "F", "R"},
Round[{t, t - 273.15, 9 t/5 - 459.67, 9 t/5}, .01]}]

tempConvert[21]
