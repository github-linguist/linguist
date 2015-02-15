data = Transpose@{Range[0, 10], {1, 6, 17, 34, 57, 86, 121, 162, 209, 262, 321}};
Fit[data, {1, x, x^2}, x]
