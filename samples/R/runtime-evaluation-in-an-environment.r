evalWithAB <- function(expr, var, a, b) {
  env <- new.env()           # provide a separate env, so that the choosen
  assign(var, a, envir=env)  # var name do not collide with symbols inside
                             # this function (e.g. it could be even "env")
  atA <- eval(parse(text=expr), env)
                             # and then evaluate the expression inside this
                             # ad hoc env-ironment
  assign(var, b, envir=env)
  atB <- eval(parse(text=expr), env)
  return(atB - atA)
}

print(evalWithAB("2*x+1", "x", 5, 3))
print(evalWithAB("2*y+1", "y", 5, 3))
print(evalWithAB("2*y+1", "x", 5, 3)) # error: object "y" not found
