f = loadstring(s) -- load a string as a function. Returns a function.

one = loadstring"return 1" -- one() returns 1

two = loadstring"return ..." -- two() returns the arguments passed to it
