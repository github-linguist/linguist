
Local bk = CreateBank(8)
PokeFloat bk, 0, -1
Print Bin(PeekInt(bk, 0))
Print %1000000000000000
Print Bin(1 Shl 31)
Print $1f
Print $ff
Print $1f + (127 - 15)
Print Hex(%01111111100000000000000000000000)
Print Hex(~%11111111100000000000000000000000)

Print Bin(FloatToHalf(-2.5))
Print HalfToFloat(FloatToHalf(-200000000000.0))

Print Bin(FToI(-2.5))

WaitKey
End


; Half-precision (16-bit) arithmetic library
;============================================

Global Half_CBank_

Function FToI(f#)
	If Half_CBank_ = 0 Then Half_CBank_ = CreateBank(4)
	PokeFloat Half_CBank_, 0, f
	Return PeekInt(Half_CBank_, 0)
End Function

Function HalfToFloat#(h)
	Local signBit, exponent, fraction, fBits
	
	signBit = (h And 32768) <> 0
	exponent = (h And %0111110000000000) Shr 10
	fraction = (h And %0000001111111111)
	
	If exponent = $1F Then exponent = $FF : ElseIf exponent Then exponent = (exponent - 15) + 127
	fBits = (signBit Shl 31) Or (exponent Shl 23) Or (fraction Shl 13)
	
	If Half_CBank_ = 0 Then Half_CBank_ = CreateBank(4)
	PokeInt Half_CBank_, 0, fBits
	Return PeekFloat(Half_CBank_, 0)
End Function

Function FloatToHalf(f#)
	Local signBit, exponent, fraction, fBits
	
	If Half_CBank_ = 0 Then Half_CBank_ = CreateBank(4)
	PokeFloat Half_CBank_, 0, f
	fBits = PeekInt(Half_CBank_, 0)
	
	signBit = (fBits And (1 Shl 31)) <> 0
	exponent = (fBits And $7F800000) Shr 23
	fraction = fBits And $007FFFFF
	
	If exponent
		exponent = exponent - 127
		If Abs(exponent) > $1F
			If exponent <> ($FF - 127) Then fraction = 0
			exponent = $1F * Sgn(exponent)
		Else
			exponent = exponent + 15
		EndIf
		exponent = exponent And %11111
	EndIf
	fraction = fraction Shr 13
	
	Return (signBit Shl 15) Or (exponent Shl 10) Or fraction
End Function

Function HalfAdd(l, r)
	
End Function

Function HalfSub(l, r)
	
End Function

Function HalfMul(l, r)
	
End Function

Function HalfDiv(l, r)
	
End Function

Function HalfLT(l, r)
	
End Function

Function HalfGT(l, r)
	
End Function


; Double-precision (64-bit) arithmetic library)
;===============================================

Global DoubleOut[1], Double_CBank_

Function DoubleToFloat#(d[1])
	
End Function

Function FloatToDouble(f#)
	
End Function

Function IntToDouble(i)
	
End Function

Function SefToDouble(s, e, f)
	
End Function

Function DoubleAdd(l, r)
	
End Function

Function DoubleSub(l, r)
	
End Function

Function DoubleMul(l, r)
	
End Function

Function DoubleDiv(l, r)
	
End Function

Function DoubleLT(l, r)
	
End Function

Function DoubleGT(l, r)
	
End Function


;~IDEal Editor Parameters:
;~F#1A#20#2F
;~C#Blitz3D