Item = map,compass,water,sandwich,glucose,tin,banana,apple,cheese,beer,suntan cream
      ,camera,tshirt,trousers,umbrella,waterproof trousers,waterproof overclothes,notecase
      ,sunglasses,towel,socks,book
Weight= 9,13,153,50,15,68,27,39,23,52,11,32,24,48,73,42,43,22,7,18,4,30
Value = 150,35,200,60,60,45,60,40,30,10,70,30,15,10,40,70,75,80,20,12,50,10
Bound = 1,1,2,2,2,3,3,3,1,3,1,1,2,2,1,1,1,1,1,2,1,2

StringSplit I, Item,  `, ; Put input in arrays
StringSplit W, Weight,`,
StringSplit V, Value, `,
StringSplit B, Bound, `,

W := 400, N := I0, I0 := V0 := W0 := 0 ; W = total weight allowed, maximize total value
Loop %W%
   m0_%A_Index% := 0

Loop %N% { ; build achievable value matrix m [ N rows, W columns ]
   j := -1+i := A_Index,  m%j%_0 := 0 ; m[i,P] = max value with items 1..i, weight <=P
   Loop %W% {                         ; m[i,P] = max_k {m[i-1,P-k*Wi]}
      p := A_Index, k := 0, y := m%j%_%p%
      While ++k <= B%i% && (r := p - k*W%i%) >= 0
         y := y < (c:=m%j%_%r%+k*V%i%) ? c : y
      m%i%_%p% := y
   }
}

i := 1+j := N, p := W, s := 0
While --i, --j { ; read out solution from value matrix m
   If (m%i%_%p% = m%j%_%p%)
      Continue
   r := p, m := m%i%_%p%, k := 1
   While 0 <= (r-=W%i%)  &&  m%j%_%r% != (m-=V%i%)
      k++ ; find multiplier
   t := k " " I%i% "`n" t, s += k*W%i%, p -= k*W%i%
}

MsgBox % "Value = " m%N%_%W% "`nWeight = " s "`n`n" t
