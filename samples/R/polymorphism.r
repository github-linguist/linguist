setClass("point",
   representation(
      x="numeric",
      y="numeric"),
   prototype(
      x=0,
      y=0))

# Instantiate class with some arguments
p1 <- new("point", x=3)
# Access some values
p1@x    # 3
# Define a print method
setMethod("print", signature("point"),
   function(x, ...)
   {
      cat("This is a point, with location, (", x@x, ",", x@y, ").\n")
   })
print(p1)

# Define a circle class
setClass("circle",
   representation(
      centre="point",
      r="numeric"),
   prototype(
      centre=new("point"),
      r=1))
circS4 <- new("circle", r=5.5)
# Access some values
circS4@r    # 5.5
circS4@centre@x   # 0
# Define a print method
setMethod("print", signature("circle"),
   function(x, ...)
   {
      cat("This is a circle, with radius", x@r, "and centre (", x@centre@x, ",", x@centre@y, ").\n")
   })
print(circS4)
