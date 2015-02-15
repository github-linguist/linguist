# create an array with one object in it
a = ['foo']

# the Array#new method allows several additional ways to create arrays

# push objects into the array
a << 1         # ["foo", 1]
a.push(3,4,5)  # ["foo", 1, 3, 4, 5]

# set the value at a specific index in the array
a[0] = 2       # [2, 1, 3, 4, 5]

# a couple of ways to set a slice of the array
a[0,3] = 'bar'    # ["bar", 4, 5]
a[1..-1] = 'baz'  # ["bar", "baz"]
a[0] = nil        # [nil, "baz"]
a[0,1] = nil      # ["baz"]

# retrieve an element
puts a[0]
