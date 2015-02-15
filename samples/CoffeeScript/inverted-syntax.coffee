alert "hello" if true
alert "hello again" unless false # the same as the above; unless is a negated if.

idx = 0
arr = (++idx while idx < 10) # arr is [1,2,3,4,5,6,7,8,9,10]

idx = 0
arr = (++idx until idx is 10) # same as above; until is an inverted while.
