Define.i Pop = 100 ,Mrate = 6
Define.s targetS = "METHINKS IT IS LIKE A WEASEL"
Define.s CsetS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ "

Procedure.i fitness (Array aspirant.c(1),Array target.c(1))
  Protected.i i ,len, fit
  len = ArraySize(aspirant())
  For i=0 To len
      If aspirant(i)=target(i): fit +1: EndIf
  Next
  ProcedureReturn fit
EndProcedure

Procedure mutatae(Array parent.c(1),Array child.c(1),Array CsetA.c(1),rate.i)
  Protected i.i ,L.i,maxC
  L = ArraySize(child())
  maxC = ArraySize(CsetA())
  For i = 0 To L
    If Random(100) < rate
      child(i)= CsetA(Random(maxC))
    Else
      child(i)=parent(i)
    EndIf
  Next
EndProcedure

Procedure.s Carray2String(Array A.c(1))
  Protected S.s ,len.i
  len = ArraySize(A())+1 : S = LSet("",len," ")
  CopyMemory(@A(0),@S, len *SizeOf(Character))
  ProcedureReturn S
EndProcedure

Define.i Mrate , maxC ,Tlen ,i ,maxfit ,gen ,fit,bestfit
Dim targetA.c(Len(targetS)-1)
  CopyMemory(@targetS, @targetA(0), StringByteLength(targetS))

Dim CsetA.c(Len(CsetS)-1)
  CopyMemory(@CsetS, @CsetA(0), StringByteLength(CsetS))

maxC   = Len(CsetS)-1
maxfit = Len(targetS)
Tlen   = Len(targetS)-1
Dim    parent.c(Tlen)
Dim     child.c(Tlen)
Dim Bestchild.c(Tlen)

For i = 0 To Tlen
  parent(i)= CsetA(Random(maxC))
Next

fit = fitness (parent(),targetA())
OpenConsole()

PrintN(Str(gen)+": "+Carray2String(parent())+" Fitness= "+Str(fit)+"/"+Str(maxfit))

While bestfit <> maxfit
  gen +1 :
  For i = 1 To Pop
    mutatae(parent(),child(),CsetA(),Mrate)
    fit = fitness (child(),targetA())
    If fit > bestfit
      bestfit = fit : Swap Bestchild() , child()
    EndIf
  Next
  Swap parent() , Bestchild()
  PrintN(Str(gen)+": "+Carray2String(parent())+" Fitness= "+Str(bestfit)+"/"+Str(maxfit))
Wend
PrintN("Press any key to exit"): Repeat: Until Inkey() <> ""
