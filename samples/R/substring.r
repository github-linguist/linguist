s <- "abcdefgh"
n <- 2; m <- 2; char <- 'd'; chars <- 'cd'
substring(s, n, n + m)
substring(s, n)
substring(s, 1, nchar(s)-1)
indx <- which(strsplit(s, '')[[1]]%in%strsplit(char, '')[[1]])
substring(s, indx, indx + m)
indx <- which(strsplit(s, '')[[1]]%in%strsplit(chars, '')[[1]])[1]
substring(s, indx, indx + m)
