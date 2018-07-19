MsgBox % IsPrime(1995937)
Loop
   MsgBox % A_Index-3 . " is " . (IsPrime(A_Index-3) ? "" : "not ") . "prime."

IsPrime(n,k=2) { ; testing primality with trial divisors not multiple of 2,3,5, up to sqrt(n)
   d := k+(k<7 ? 1+(k>2) : SubStr("6-----4---2-4---2-4---6-----2",Mod(k,30),1))
   Return n < 3 ? n>1 : Mod(n,k) ? (d*d <= n ? IsPrime(n,d) : 1) : 0
}
