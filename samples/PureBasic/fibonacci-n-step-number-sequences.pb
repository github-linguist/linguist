Procedure.i FibonacciLike(k,n=2,p.s="",d.s=".")
Protected i,r
if k<0:ProcedureReturn 0:endif
if p.s
n=CountString(p.s,d.s)+1
for i=0 to n-1
if k=i:ProcedureReturn val(StringField(p.s,i+1,d.s)):endif
next
else
if k=0:ProcedureReturn 1:endif
if k=1:ProcedureReturn 1:endif
endif
for i=1 to n
r+FibonacciLike(k-i,n,p.s,d.s)
next
ProcedureReturn r
EndProcedure

; The fact that PureBasic supports default values for procedure parameters
; is very useful in a case such as this.
; Since:
; k=4
; Debug FibonacciLike(k)               ;good old Fibonacci

; Debug FibonacciLike(k,3)             ;here we specified n=3 [Tribonacci]
; Debug FibonacciLike(k,3,"1.1.2")     ;using the default delimiter "."
; Debug FibonacciLike(k,3,"1,1,2",",") ;using a different delimiter ","
; the last three all produce the same result.

; as do the following two for the Lucas series:
; Debug FibonacciLike(k,2,"2.1")     ;using the default delimiter "."
; Debug FibonacciLike(k,2,"2,1",",") ;using a different delimiter ","

m=10
t.s=lset("n",5)
for k=0 to m
  t.s+lset(str(k),5)
  next
Debug t.s
for n=2 to 10
  t.s=lset(str(n),5)
  for k=0 to m
    t.s+lset(str(FibonacciLike(k,n)),5)
    next
Debug t.s
next
Debug ""
p.s="2.1"
t.s=lset(p.s,5)
for k=0 to m
  t.s+lset(str(FibonacciLike(k,n,p.s)),5)
  next
Debug t.s
Debug ""
