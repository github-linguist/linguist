# Read in text
lines <- readLines(tc <- textConnection("Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.")); close(tc)

#Split words by the dollar
words <- strsplit(lines, "\\$")

#Reformat
maxlen <- max(sapply(words, length))
words <- lapply(words, function(x) {length(x) <- maxlen; x})
block <- matrix(unlist(words), byrow=TRUE, ncol=maxlen)
block[is.na(block)] <- ""
leftjust <- format(block)
rightjust <- format(block, justify="right")
centrejust <- format(block, justify="centre")

# Print
print0 <- function(x) invisible(apply(x, 1, function(x) cat(x, "\n")))
print0(leftjust)
print0(rightjust)
print0(centrejust)
