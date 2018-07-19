compose <- function(f,g) function(x) { f(g(x)) }
r <- compose(sin, cos)
print(r(.5))
