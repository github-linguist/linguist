Function fibo(n as integer) As UInt64

  dim noOne as UInt64 = 1
  dim noTwo as UInt64 = 1	
  dim sum As UInt64

  for i as integer = 1 to n
      sum = noOne + noTwo
      noTwo = noOne
      noOne = sum
  Next

  Return noOne
End Function
