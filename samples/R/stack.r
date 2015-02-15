library(proto)

stack <- proto(expr = {
   l <- list()
   empty <- function(.) length(.$l) == 0
   push <- function(., x)
   {
      .$l <- c(list(x), .$l)
      print(.$l)
      invisible()
   }
   pop <- function(.)
   {
      if(.$empty()) stop("can't pop from an empty list")
      .$l[[1]] <- NULL
      print(.$l)
      invisible()
   }
})

stack$empty()
# [1] TRUE
stack$push(3)
# [[1]]
# [1] 3
stack$push("abc")
# [[1]]
# [1] "abc"
# [[2]]
# [1] 3
stack$push(matrix(1:6, nrow=2))
# [[1]]
#      [,1] [,2] [,3]
# [1,]    1    3    5
# [2,]    2    4    6
# [[2]]
# [1] "abc"
# [[3]]
# [1] 3
stack$empty()
# [1] FALSE
stack$pop()
# [[1]]
[1] "abc"
# [[2]]
# [1] 3
stack$pop()
# [[1]]
# [1] 3
stack$pop()
# list()
stack$pop()
# Error in get("pop", env = stack, inherits = TRUE)(stack, ...) :
#   can't pop from an empty list
