# Results are system dependent
num <- c(1, 3, 6, 10)
object.size(num)  # e.g. 56 bytes

#Allocating vectors using ':' results in less memory being (reportedly) used
num2 <- 1:4
object.size(num2) # e.g. 40 bytes

#Memory shared by objects isn't always counted
l <- list(a=c(1, 3, 6, 10), b=1:4)
object.size(l)    # e.g. 280 bytes

l2 <- list(num, num2)
object.size(l2)   # e.g. 128 bytes
