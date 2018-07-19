# Loop plus half.  This code shows how to break out of a loop early
# on the last iteration.  For the contrived example, there are better
# ways to generate a comma-separated list, of course.
start = 1
end = 10
s = ''
for i in [start..end]
  # the top half of the loop executes every time
  s += i
  break if i == end
  # the bottom half of the loop is skipped for the last value
  s += ', '
console.log s
