Bucket := [],	Buckets := 10,	Originaltotal = 0
loop, %Buckets% {
	Random, rnd, 0,50
	Bucket[A_Index] := rnd,		Originaltotal += rnd
}

loop 100
{
	total := 0
	Randomize(B1, B2, Buckets)
	temp := (Bucket[B1] + Bucket[B2]) /2
	Bucket[B1] := floor(temp),	Bucket[B2] := Ceil(temp)	; values closer to equal

	Randomize(B1, B2, Buckets)
	temp := Bucket[B1] + Bucket[B2]
	Random, value, 0, %temp%
	Bucket[B1] := value,	Bucket[B2] := temp-value		; redistribute values arbitrarily
	
	VisualTip := "Original Total = " Originaltotal "`n"
	loop, %Buckets%
		VisualTip .= SubStr("0" Bucket[A_Index], -1) " : " x(Bucket[A_Index]) "`n" , total += Bucket[A_Index]
		
	ToolTip % VisualTip "Current Total = " total
	if (total <> Originaltotal)
		MsgBox "Error"
	Sleep, 100
}
return

Randomize(ByRef B1, ByRef B2, Buckets){
	Random, B1, 1, %Buckets%
	Loop
		Random, B2, 1, %Buckets%
	until (B1<>B2)
}

x(n){
	loop, % n
		Res.= ">"
	return Res
}
