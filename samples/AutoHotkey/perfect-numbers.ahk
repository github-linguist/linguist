Loop, 30 {
  If isMersennePrime(A_Index + 1)
    res .= "Perfect number: " perfectNum(A_Index + 1) "`n"
}

MsgBox % res

perfectNum(N) {
  Return 2**(N - 1) * (2**N - 1)
}

isMersennePrime(N) {
  If (isPrime(N)) && (isPrime(2**N - 1))
    Return true
}

isPrime(N) {
  Loop, % Floor(Sqrt(N))
    If (A_Index > 1 && !Mod(N, A_Index))
      Return false
  Return true
}
