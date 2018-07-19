checkTrue(palindroc("aba"))  # TRUE
checkTrue(!palindroc("ab"))  # TRUE
checkException(palindroc())  # TRUE
checkTrue(palindroc(""))     # Error.  Uh-oh, there's a bug in the function
