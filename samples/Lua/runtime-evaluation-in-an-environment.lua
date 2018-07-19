code = loadstring"return x^2" --this doesn't really need to be input, does it?
val1 = setfenv(code, {x = io.read() + 0})()
val2 = setfenv(code, {x = io.read() + 0})()
print(val2 - val1)
