msgbox, % factors(45) "`n" factors(53) "`n" factors(64)

Factors(n)
{  Loop, % floor(sqrt(n))
   {  v := A_Index = 1 ? 1 "," n : mod(n,A_Index) ? v : v "," A_Index "," n//A_Index
   }
   Sort, v, N U D,
   Return, v
}
