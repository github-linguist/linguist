#NoEnv
SetBatchLines -1
Process, Priority,, high

output := "Derangements for 1, 2, 3, 4:`n"

obj := [1, 2, 3, 4], objS := obj.Clone()
Loop ; permute 4
{
	obj := perm_NextObj(Obj)
	If !obj
		break
	For k, v in obj
		if ( objS[k] = v )
			continue 2
	output .= ObjDisp(obj) "`n"
}
output .= "`nTable of n, counted, calculated derangements:`n"

Loop 10 ; Count !n
{
	obj := []
	count := 0
	output .= A_Tab . (i := A_Index-1) . A_Tab
	Loop % i
		obj[A_Index] := A_Index
	objS := obj.Clone()
	Loop
	{
		obj := perm_NextObj(Obj)
		If !obj
			break
		For k, v in obj
			if ( objS[k] = v )
				continue 2
		count++
	}
	output .= count . A_Tab . cd(i) . "`n"
}
output .= "`nApproximation of !20: " . cd(20)
MsgBox % Clipboard := output

perm_NextObj(obj){ ; next lexicographic permutation
	p := 0, objM := ObjMaxIndex(obj)
	Loop % objM
	{
		If A_Index=1
			continue
		t := obj[objM+1-A_Index]
		n := obj[objM+2-A_Index]
		If ( t < n )
		{
			p := objM+1-A_Index, pC := obj[p]
			break
		}
	}
	If !p
		return false
	Loop
	{
		t := obj[objM+1-A_Index]
		If ( t > pC )
		{
			n := objM+1-A_Index, nC := obj[n]
			break
		}
	}

	obj[n] := pC, obj[p] := nC
	return ObjReverse(obj, objM-p)
}

ObjReverse(Obj, tail){
 o := ObjClone(Obj), ObjM := ObjMaxIndex(O)
 Loop % tail
	o[ObjM-A_Index+1] := Obj[ObjM+A_Index-tail]
 return o
}

ObjDisp(obj){
	For k, v in obj
		s .= v ", "
	return SubStr(s, 1, strLen(s)-2)
}


cd(n){  ; Count Derangements
	static e := 2.71828182845904523536028747135
	return n ? floor(ft(n)/e + 1/2) : 1
}
ft(n){  ; FacTorial
	a := 1
	Loop % n
		a *= A_Index
	return a
}
