#concat concatenates the new values to the existing vector of values, then discards any values that are too old.
lastvalues <- local(
{
   values <- c();
   function(x, len)
   {
      values <<- c(values, x);
      lenv <- length(values);
      if(lenv > len) values <<- values[(len-lenv):-1]
      values
   }
})

#moving.average accepts a numeric scalars input (and optionally a length, i.e. the number of values to retain) and calculates the stateful moving average.
moving.average <- function(latestvalue, len=3)
{
   #Check that all inputs are numeric scalars
   is.numeric.scalar <- function(x) is.numeric(x) && length(x)==1L
   if(!is.numeric.scalar(latestvalue) || !is.numeric.scalar(len))
   {
      stop("all arguments must be numeric scalars")
   }

   #Calculate mean of variables so far
   mean(lastvalues(latestvalue, len))
}
moving.average(5)  # 5
moving.average(1)  # 3
moving.average(-3) # 1
moving.average(8)  # 2
moving.average(7)  # 4
