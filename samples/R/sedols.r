# Read in data from text connection
datalines <- readLines(tc <- textConnection("710889
B0YBKJ
406566
B0YBLH
228276
B0YBKL
557910
B0YBKR
585284
B0YBKT")); close(tc)

# Check data valid
checkSedol <- function(datalines)
{
   ok <- grep("^[[:digit:][:upper:]]{6}$", datalines)
   if(length(ok) < length(datalines))
   {
      stop("there are invalid lines")
   }
}
checkSedol(datalines)

# Append check digit
appendCheckDigit <- function(x)
{
   if(length(x) > 1) return(sapply(x, appendCheckDigit))
   ascii <- as.integer(charToRaw(x))
   scores <- ifelse(ascii < 65, ascii - 48, ascii - 55)
   weights <- c(1, 3, 1, 7, 3, 9)
   chkdig <- (10 - sum(scores * weights) %% 10) %% 10
   paste(x, as.character(chkdig), sep="")
}
withchkdig <- appendCheckDigit(datalines)

#Print in format requested
writeLines(withchkdig)
