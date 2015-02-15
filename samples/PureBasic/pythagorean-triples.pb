Procedure.i ConsoleWrite(t.s)  ; compile using /CONSOLE option
        OpenConsole()
        PrintN (t.s)
        CloseConsole()
        ProcedureReturn 1
EndProcedure

Procedure.i StdOut(t.s)  ; compile using /CONSOLE option
        OpenConsole()
        Print(t.s)
        CloseConsole()
        ProcedureReturn 1
EndProcedure

Procedure.i gcDiv(n,m) ; greatest common divisor
if n=0:ProcedureReturn m:endif
while m <> 0
  if n > m
    n - m
  else
    m - n
  endif
wend
ProcedureReturn n
EndProcedure

st=ElapsedMilliseconds()

nmax      =10000
power     =8

dim primitiveA(power)
dim alltripleA(power)
dim pmaxA(power)

x=1
for i=1 to power
  x*10:pmaxA(i)=x/2
next

for n=1 to nmax
  for m=(n+1) to (nmax+1) step 2 ; assure m-n is odd
    d=gcDiv(n,m)
    p=m*m+m*n
    for i=1 to power
      if p<=pmaxA(i)
        if d =1
        primitiveA(i)+1        ; right here we have the primitive perimeter "seed" 'p'
        k=1:q=p*k              ; set k to one to include p : use q as the 'p*k'
          while q<=pmaxA(i)
          alltripleA(i)+1      ; accumulate multiples of this perimeter while q <= pmaxA(i)
          k+1:q=p*k
          wend
        endif
      endif
    next
  next
next

for i=1 to power
  t.s="Up to "+str(pmaxA(i)*2)+": "
  t.s+str(alltripleA(i))+" triples, "
  t.s+str(primitiveA(i))+" primitives."
  ConsoleWrite(t.s)
next
ConsoleWrite("")
et=ElapsedMilliseconds()-st:ConsoleWrite("Elapsed time = "+str(et)+" milliseconds")
