MsgBox % "12345678901234567890`n" Sieve(20)

Sieve(n) { ; Sieve of Eratosthenes => string of 0|1 chars, 1 at position k: k is prime
   Static zero := 48, one := 49 ; Asc("0"), Asc("1")
   VarSetCapacity(S,n,one)
   NumPut(zero,S,0,"char")
   i := 2
   Loop % sqrt(n)-1 {
      If (NumGet(S,i-1,"char") = one)
         Loop % n//i
            If (A_Index > 1)
               NumPut(zero,S,A_Index*i-1,"char")
      i += 1+(i>2)
   }
   Return S
}
