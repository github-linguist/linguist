main:		SUBSP	34, i
		LDA	31, i
		STA	0, s
		CALL	fgets
		ADDSP	2, i
		CALL	ststro
		STOP

; Reads a string from stdin, stops reading when one of the following is true:
;	- Read a \n
;	- Read a maximum of `max` chars
;
; Parameters:
;	- SP + 2: `max`, the maximum number of chars to read
;	- SP + 4: `buffer` of length `max` + 1
; Returns:
;	void
fgets:		LDX	0, i
		LDA	0, i
fgetslp:	CHARI	buffer, sx
		LDBYTEA	buffer, sx
		CPA	'\n', i
		BREQ	fout
		CPX	max, s
		BREQ	fout
		ADDX	1, i
		BR	fgetslp
fout:		LDA	'\x00', i
		STBYTEA	buffer, sx
		RET0
max:	.EQUATE 2
buffer: .EQUATE 4

; Prints a string stored in stack
;
; Parameters:
;	SP + 2: `string`
; Returns:
;	void
ststro:		LDX	0, i
		LDA	0, i
strolp:		LDBYTEA	string, sx
		CPA	'\x00', i
		BREQ	strout
		CHARO	string, sx
		ADDX	1, i
		BR	strolp
strout:		RET0
string: .EQUATE 2
	.END
