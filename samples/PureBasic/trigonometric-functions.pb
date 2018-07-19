OpenConsole()

Macro DegToRad(deg)
  deg*#PI/180
EndMacro
Macro RadToDeg(rad)
  rad*180/#PI
EndMacro

degree    = 45
radians.f = #PI/4

PrintN(StrF(Sin(DegToRad(degree)))+" "+StrF(Sin(radians)))
PrintN(StrF(Cos(DegToRad(degree)))+" "+StrF(Cos(radians)))
PrintN(StrF(Tan(DegToRad(degree)))+" "+StrF(Tan(radians)))

arcsin.f = ASin(Sin(radians))
PrintN(StrF(arcsin)+" "+Str(RadToDeg(arcsin)))
arccos.f = ACos(Cos(radians))
PrintN(StrF(arccos)+" "+Str(RadToDeg(arccos)))
arctan.f = ATan(Tan(radians))
PrintN(StrF(arctan)+" "+Str(RadToDeg(arctan)))

Input()
