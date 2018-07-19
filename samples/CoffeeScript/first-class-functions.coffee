# Functions as values of a variable
cube = (x) -> Math.pow x, 3
cuberoot = (x) -> Math.pow x, 1 / 3

# Higher order function
compose = (f, g) -> (x) -> f g(x)

# Storing functions in a array
fun = [Math.sin, Math.cos, cube]
inv = [Math.asin, Math.acos, cuberoot]

# Applying the composition to 0.5
console.log compose(inv[i], fun[i])(0.5) for i in [0..2]​​​​​​​
