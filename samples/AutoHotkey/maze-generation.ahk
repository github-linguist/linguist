; Initially build the board
Width	:= 11
Height	:= 8
Loop % height*2+1
{
	Outer := A_Index
	Loop % Width
		maze .= Outer & 1 ? "+-" : "|0"
	maze .= (Outer & 1 ? "+" : "|") "`n"
}
StringTrimRight, maze, maze, 1 ; removes trailing newline
Clipboard := Walk(maze)

Walk(S, x=0, y=0){
	If !x{	; --Start at a random cell...
		StringReplace, junk, S, `n,,UseErrorLevel ; Calculate rows
		Random, y, 1, ErrorLevel//2
		Random, x, 1, InStr(S, "`n")//2-1         ; Calculate height
	}
	
	; --Obtain a list of its neighbors...
	neighbors := x "," y+1 "`n" x "," y-1 "`n" x+1 "," y "`n" x-1 "," y
	; --Randomize the list...
	Sort neighbors, random
	
	; --Then for each neighbor...
	Loop Parse, neighbors, `n
	{
		pC := InStr(A_LoopField, ","), x2 := SubStr(A_LoopField, 1, pC-1), y2 := SubStr(A_LoopField, pC+1)
		; If it has not been visited...
		If GetChar(S, 2*x2, 2*y2) = "0"{
			; Mark it as visited...
			S := ChangeChar(s, 2*x2, 2*y2, " ")
			; Remove the wall between this cell and the neighbor...
			S := ChangeChar(S, x+x2, y+y2, " ")
			; Then recurse with the neighbor as the current cell
			S := Walk(S, x2, y2)
		}
	}
	return S
}
	
; Change a character in a string using x and y coordinates
ChangeChar(s, x, y, c){
	Loop Parse, s, `n
	{
		If (A_Index = Y)
			Loop Parse, A_LoopField
				If (A_Index = x)
					out .= c
				Else	out .= A_LoopField
		Else out .= A_LoopField
		out .= "`n"
	}
	StringTrimRight, out, out, 1
	return out
}

; retrieve a character in a string using x and y coordinates
GetChar(s, x, y, n=1){
	x*=n, y*=n
	Loop Parse, s, `n
		If (A_Index = Y)
			return SubStr(A_LoopField, x, 1)
}
