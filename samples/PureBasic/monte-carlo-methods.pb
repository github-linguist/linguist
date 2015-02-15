OpenConsole()

Procedure.d MonteCarloPi(throws.d)
	inCircle.d = 0
		For i = 1 To throws.d
			randX.d = (Random(2147483647)/2147483647)*2-1
			randY.d = (Random(2147483647)/2147483647)*2-1
			dist.d  = Sqr(randX.d*randX.d + randY.d*randY.d)
			If dist.d < 1
				inCircle = inCircle + 1
			EndIf
		Next i
	pi.d = (4 * inCircle / throws.d)	
	ProcedureReturn pi.d
	
EndProcedure

PrintN ("'built-in' #Pi         = " + StrD(#PI,20))
PrintN ("MonteCarloPi(10000)    = " + StrD(MonteCarloPi(10000),20))
PrintN ("MonteCarloPi(100000)   = " + StrD(MonteCarloPi(100000),20))
PrintN ("MonteCarloPi(1000000)  = " + StrD(MonteCarloPi(1000000),20))
PrintN ("MonteCarloPi(10000000) = " + StrD(MonteCarloPi(10000000),20))

PrintN("Press any key"): Repeat: Until Inkey() <> ""
