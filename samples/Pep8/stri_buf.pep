main:

; Reads a string in stdin, returns the buffer it was read in
; Stops reading at the first encounter of a \n character.
;
; Parameters:
;	void
;
; Returns:
;	- X: Address of the buffer
stri:		SUBSP	2, i
		LDA	32, i
		CALL	new
		CPX	buflen, s
		BRGE	strinlrg
strinlrg:	LDA	buflen, d
		LDX	2, i
		CALL	mult
		STA	buflen
		CALL	new
		
buflen:		.EQUATE 0

; Copies the content of a buffer to another one
;
; Parameters:
;	- SP + 2: Destination buffer
;	- SP + 4: Source buffer
;	- SP + 6: Length to copy
memcpy:		LDX	0, i
memcplp:	CPX	cpylen, s
		BREQ	memcpout
		LDBYTEA	srcbuf, sxf
		STBYTEA	dstbuf, sxf
		ADDX	1, i
		BR	memcplp
memcpout:	RET0
; Destination buffer
dtsbuf:		.EQUATE 2
; Source buffer
srcbuf:		.EQUATE 4
; Copy length
cpylen:		.EQUATE 6

; Allocates a new structure in the heap
;
; Parameters:
;	- A: Length of the structure to allocate (bytes)
;
; Returns:
;	- X: Address of the allocated structure
new:		ADDA	hpptr, d
		LDX	hpptr, d
		STA	hpptr, d
		RET0

; Pointer to the next available byte on the heap
hpptr:		.ADDRSS heap
; Start of the heap
heap:		.BLOCK 1
	.END
