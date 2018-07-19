Define.f TotalWeight, TotalVolyme
Define.i maxPanacea, maxIchor, maxGold, maxValue
Define.i i, j ,k
Dim n.i(2)

Enumeration
  #Panacea
  #Ichor
  #Gold
  #Sack
  #Current
EndEnumeration

Structure Bounty
  value.i
  weight.f
  volyme.f
EndStructure

Dim Item.Bounty(4)
CopyMemory(?panacea,@Item(#Panacea),SizeOf(Bounty))
CopyMemory(?ichor,  @Item(#Ichor),  SizeOf(Bounty))
CopyMemory(?gold,   @Item(#gold),   SizeOf(Bounty))
CopyMemory(?sack,   @Item(#Sack),   SizeOf(Bounty))

Procedure.f min(a.f, b.f)
  If a<b
    ProcedureReturn a
  Else
    ProcedureReturn b
  EndIf
EndProcedure

maxPanacea=min(Item(#Sack)\weight/Item(#Panacea)\weight,Item(#Sack)\volyme/Item(#Panacea)\volyme)
maxIchor  =min(Item(#Sack)\weight/Item(#Ichor)\weight,  Item(#Sack)\volyme/Item(#Ichor)\volyme)
maxGold   =min(Item(#Sack)\weight/Item(#Gold)\weight,   Item(#Sack)\volyme/Item(#Gold)\volyme)

For i=0 To maxPanacea
  For j=0 To maxIchor
    For k=0 To maxGold
      Item(#Current)\value=k*Item(#Gold)\value  +j*item(#Ichor)\value +i*item(#Panacea)\value
      Item(#Current)\weight=k*Item(#Gold)\weight+j*Item(#Ichor)\weight+i*Item(#Panacea)\weight
      Item(#Current)\volyme=k*Item(#Gold)\volyme+j*Item(#Ichor)\volyme+i*Item(#Panacea)\volyme
      If Item(#Current)\weight>Item(#Sack)\weight Or Item(#Current)\volyme>Item(#Sack)\volyme
        Continue
      EndIf
      If Item(#Current)\value>maxValue
        maxValue=Item(#Current)\value
        TotalWeight=Item(#Current)\weight
        TotalVolyme=Item(#Current)\volyme
        n(#Panacea)=i: n(#Ichor)=j: n(#Gold)=k
      EndIf
    Next k
  Next j
Next i

If OpenConsole()
  Define txt$
  txt$="Maximum value achievable is "+Str(maxValue)+#CRLF$
  txt$+"This is achieved by carrying "+Str(n(#Panacea))+" panacea, "
  txt$+Str(n(#Ichor))+" ichor and "+Str(n(#Gold))+" gold items."+#CRLF$
  txt$+"The weight to carry is "+StrF(totalWeight,2)
  txt$+" and the volume used is "+StrF(TotalVolyme,2)
  PrintN(txt$)

  Print(#CRLF$+"Press Enter to quit"): Input()
EndIf

DataSection
panacea:
  Data.i 3000
  Data.f 0.3, 0.025
ichor:
  Data.i 1800
  Data.f 0.2, 0.015
gold:
  Data.i 2500
  Data.f 2.0, 0.002
sack:
  Data.i  0
  Data.f  25.0, 0.25
EndDataSection
