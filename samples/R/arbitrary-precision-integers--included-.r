library(gmp)
large=pow.bigz(5,pow.bigz(4,pow.bigz(3,2)))
largestr=as.character(large)
cat("first 20 digits:",substr(largestr,1,20),"\n",
    "last 20 digits:",substr(largestr,nchar(largestr)-19,nchar(largestr)),"\n",
    "number of digits: ",nchar(largestr),"\n")
