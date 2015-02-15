Function factors(num As UInt64) As UInt64()
  'This function accepts an unsigned 64 bit integer as input and returns an array of unsigned 64 bit integers
  Dim result() As UInt64
  Dim iFactor As UInt64 = 1
  While iFactor <= num/2 'Since a factor will never be larger than half of the number
    If num Mod iFactor = 0 Then
      result.Append(iFactor)
    End If
    iFactor = iFactor + 1
  Wend
  result.Append(num) 'Since a given number is always a factor of itself
  Return result
End Function
