xprecision = 3
yprecision = 5
x = round([1, 2, 3, 1e11],xprecision)
y = round([1, 1.4142135623730951, 1.7320508075688772, 316227.76601683791],yprecision)
writedlm("filename", [x y], '\t')
