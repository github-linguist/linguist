#Get the limit
options("expressions")

#Set it
options(expressions = 10000)

#Test it
recurse <- function(x)
{
  print(x)
  recurse(x+1)

}
recurse(0)
