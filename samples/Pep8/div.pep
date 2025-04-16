main:	SUBSP	8, i
	DECI	0, s
	DECI	2, s
	CALL	div
	DECO	4, s
	CHARO	'\n', i
	DECO	6, s
	CHARO	'\n', i
	STOP

; Divides two numbers following the euclidian method
;
; Parameters:
;	SP + 2: Dividend 
;	SP + 4: Divider
; Returns:
;	SP + 6: Quotient
;	SP + 8: Remain
div:		LDX	0, i
		LDA	dividend, s
divlp:		CPA	divider, s
		BRLT	divout
		ADDX	1, i
		SUBA	divider, s
		BR	divlp
divout:		STX	quot, s
		STA	rem, s
		RET0
dividend:	.EQUATE 2
divider:	.EQUATE 4
quot:		.EQUATE 6
rem:		.EQUATE 8

		.END
