x=numeric(10)  # allocate a numeric vector of size 10 to x
rm(x)  # remove x

x=vector("list",10) #allocate a list of length 10
x=vector("numeric",10) #same as x=numeric(10), space allocated to list vector above now freed
rm(x)  # remove x
