# Method
pow <- function(x, y)
{
   x <- as.numeric(x)
   y <- as.integer(y)
   prod(rep(x, y))
}
#Operator
"%pow%" <- function(x,y) pow(x,y)

pow(3, 4)    # 81
2.5 %pow% 2  # 6.25
