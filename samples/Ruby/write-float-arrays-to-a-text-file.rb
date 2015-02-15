# prepare test data
x = [1, 2, 3, 1e11]
y = x.collect { |xx| Math.sqrt xx }
xprecision = 3
yprecision = 5

# write the arrays
open('sqrt.dat', 'w') do |f|
  x.zip(y) { |xx, yy| f.printf("%.*g\t%.*g\n", xprecision, xx, yprecision, yy) }
end

# print the result file
open('sqrt.dat', 'r') { |f| puts f.read }
