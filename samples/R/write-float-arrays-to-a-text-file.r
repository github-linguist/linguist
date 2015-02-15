writexy <- function(file, x, y, xprecision=3, yprecision=3)
{
   #Format inputs as required, and join together
   fx <- formatC(x, digits=xprecision, format="g", flag="-")
   fy <- formatC(y, digits=yprecision, format="g", flag="-")
   dfr <- data.frame(fx, fy)
   #Write to file.  Note that this encloses the formatted number in quotes,
   write.table(dfr, file=file, sep="\t", row.names=FALSE)
   #... so we have to process the output
   str <- readLines(file)
   writeLines(gsub('"', '', str), file)
}


x <- c(1, 2, 3, 1e11)
y <- sqrt(x)
writexy('test.txt', x, y, yp=5)
