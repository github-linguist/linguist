
assert(terralib.sizeof(int)*3 <= terralib.sizeof(vector(int,3)))
assert(terralib.sizeof(vector(int,1)) == 4)
assert(terralib.sizeof(vector(int,4)) == 16)
assert(terralib.sizeof(vector(int,5)) == 32)