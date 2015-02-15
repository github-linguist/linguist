ncsub <- function(x)
{
   n <- length(x)
   a <- seq_len(n)
   seqlist <- list()
   for(i in 2:(n-1))
   {
      seqs <- combn(a, i)                                                          # Get all subseqs
      ok <- apply(seqs, 2, function(x) any(diff(x)!=1))                            # Find noncts ones
      newseqs <- unlist(apply(seqs[,ok], 2, function(x) list(x)), recursive=FALSE) # Convert matrix to list of its columns
      seqlist <- c(seqlist, newseqs)                                               # Append to existing list
   }
   lapply(seqlist, function(index) x[index])
}
# Example usage
ncsub(1:4)
ncsub(letters[1:5])
