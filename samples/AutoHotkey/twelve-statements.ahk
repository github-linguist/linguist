; Note: the original puzzle provides 12 statements and starts with
; "Given the following twelve statements...", so the first statement
; should ignore the F1 flag and always be true (see "( N == 1 )").

S := 12 ; number of statements
Output := ""
Loop, % 2**S {
	;;If !Mod(A_Index,100) ;; optional 'if' to show the loop progress
	;;	ToolTip, Index: %A_Index%
	SetFlags(A_Index-1), Current := "", Count := 0
	Loop, %S%
		R := TestStatement(A_Index), Current .= " " R, Count += (R == F%A_Index%)
	If ( Count >= S-1 )
		Output .= Count " ->" Current "`n"
	If ( Count = S )
		Solution := "`nSolution = " Current
}
ToolTip
MsgBox, % Output . Solution
Return

;-------------------------------------------------------------------------------------

SetFlags(D) {
	Local I
	Loop, %S%
		I := S-A_Index+1 , F%I% := (D >> (S-A_Index)) & 1
}

;-------------------------------------------------------------------------------------

TestStatement(N) {
	Local I, C := 0
	If ( N == 1 ) ; This is a numbered list of twelve statements.
		Return ( S == 12 ) ; should always be true
	If ( N == 2 ) { ; Exactly 3 of the last 6 statements are true.
		Loop, 6
			I := S-A_Index+1 , C += F%I%
		Return ( C == 3 )
	}
	If ( N == 3 ) { ; Exactly 2 of the even-numbered statements are true.
		Loop, %S%
			C += ( !Mod(A_Index,2) & F%A_Index% )
		Return ( C == 2 )
	}
	If ( N == 4 ) ; If statement 5 is true, then statements 6 and 7 are both true.
		Return ( F5 ? F6 & F7 : 1 )
	If ( N == 5 ) { ; The 3 preceding statements are all false.
		Loop, 3
			I := N-A_Index , C += F%I%
		Return ( C == 0 )
	}
	If ( N == 6 ) { ; Exactly 4 of the odd-numbered statements are true.
		Loop, %S%
			C += ( !!Mod(A_Index,2) & F%A_Index% )
		Return ( C == 4 )
	}
	If ( N == 7 ) ; Either statement 2 or 3 is true, but not both.
		Return ( F2 ^ F3 )
	If ( N == 8 ) ; If statement 7 is true, then 5 and 6 are both true.
		Return ( F7 ? F5 & F6 : 1 )
	If ( N == 9 ) { ; Exactly 3 of the first 6 statements are true.
		Loop, 6
			C += F%A_Index%
		Return ( C == 3 )
	}
	If ( N == 10 ) ; The next two statements are both true.
		Return ( F11 & F12 )
	If ( N == 11 ) ; Exactly 1 of statements 7, 8 and 9 are true
		Return ( F7+F8+F9 == 1 )
	If ( N == 12 ) { ; Exactly 4 of the preceding statements are true
		Loop, % N-1
			C += F%A_Index%
		Return ( C == 4 )
	}
}
