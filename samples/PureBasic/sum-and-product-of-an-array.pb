Dim MyArray(9)
Define a, sum=0, prod=1

For a = 0 To ArraySize(MyArray())     ; Create a list of some random numbers
  MyArray(a) = 1 + Random(9)          ; Insert a number [1...10] in current element
Next

For a = 0 To ArraySize(MyArray())     ; Calculate Sum and Product of this Array
  sum  + MyArray(a)
  prod * MyArray(a)
Next

Debug "The sum is " + Str(sum)        ; Present the results
Debug "Product is " + Str(prod)
