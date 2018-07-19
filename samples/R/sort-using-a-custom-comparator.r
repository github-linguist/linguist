v = c("Here", "are", "some", "sample", "strings", "to", "be", "sorted")
print(v[order(-nchar(v), tolower(v))])
