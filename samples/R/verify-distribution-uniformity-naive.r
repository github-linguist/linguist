distcheck <- function(fn, repetitions=1e4, delta=3)
{
   if(is.character(fn))
   {
      fn <- get(fn)
   }
   if(!is.function(fn))
   {
      stop("fn is not a function")
   }
   samp <- fn(n=repetitions)
   counts <- table(samp)
   expected <- repetitions/length(counts)
   lbound <- expected * (1 - 0.01*delta)
   ubound <- expected * (1 + 0.01*delta)
   status <- ifelse(counts < lbound, "under",
      ifelse(counts > ubound, "over", "okay"))
   data.frame(value=names(counts), counts=as.vector(counts), status=status)
}
distcheck(dice7.vec)
