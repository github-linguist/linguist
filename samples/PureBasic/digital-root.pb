; if you just want the DigitalRoot
; Procedure.q DigitalRoot(N.q) apparently will do
; i must have missed something because it seems too simple
; http://en.wikipedia.org/wiki/Digital_root#Congruence_formula

Procedure.q DigitalRoot(N.q)
Protected M.q=N%9
if M=0:ProcedureReturn 9
Else  :ProcedureReturn M:EndIf
EndProcedure

; there appears to be a proof guarantying that Len(N$)<=1 for some X
; http://en.wikipedia.org/wiki/Digital_root#Proof_that_a_constant_value_exists

Procedure.s DigitalRootandPersistance(N.q)
Protected r.s,t.s,X.q,M.q,persistance,N$=Str(N)
M=DigitalRoot(N.q) ; just a test to see if we get the same DigitalRoot via the Congruence_formula

Repeat
X=0:Persistance+1

For i=1 to Len(N$)       ; finding X as the sum of the digits of N
X+Val(Mid(N$,i,1))
Next

N$=Str(X)
If Len(N$)<=1:Break:EndIf ; If Len(N$)<=1:Break:EndIf
Forever

If Not (X-M)=0:t.s=" Error in my logic":else:t.s=" ok":EndIf

r.s=RSet(Str(N),15)+" has additive persistance "+Str(Persistance)
r.s+" and digital root of X(slow) ="+Str(X)+" M(fast) ="+Str(M)+t.s
ProcedureReturn r.s
EndProcedure

NewList Nlist.q()
AddElement(Nlist()) : Nlist()=627615
AddElement(Nlist()) : Nlist()=39390
AddElement(Nlist()) : Nlist()=588225
AddElement(Nlist()) : Nlist()=393900588225

FirstElement(Nlist())

ForEach Nlist()
N.q=Nlist()
; cw(DigitalRootandPersistance(N))
Debug DigitalRootandPersistance(N)
Next
