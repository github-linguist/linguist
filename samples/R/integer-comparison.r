print("insert number a")
a <- scan(what=numeric(0), nmax=1)
print("insert number b")
b <- scan(what=numeric(0), nmax=1)
if ( a < b ) {
  print("a is less than b")
} else if ( a > b ) {
  print("a is greater than b")
} else if ( a == b ) { # could be simply else of course...
  print("a and b are the same")
}
