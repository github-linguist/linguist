; Sorts a statically defined array using the recursive implementation
; of the quicksort algorithm.
;
; In this implementation, the pivot is supposed to be the rightmost
; value of the slice of the array being sorted.
;
; Note that the code presented below should work on any array,
; whether defined statically or dynamically.
;
; Calling conventions:
;	Except when mentionned otherwise, every parameter is to be passed on the stack.
;	The return values are also on the stack.
;	No assumption is to be made on the content of a register on a function call.
;	The values of the registers are to be locally saved for further use if necessary.
main:		SUBSP	4, i
		LDA	11, i
		ASLA
		STA	2, s
		LDA	arr, i
		STA	0, s
		CALL	printarr
		SUBSP	2, i
		LDA	arr, i
		STA	0, s
		LDA	0, i
		STA	2, s
		LDA	10, i
		STA	4, s
		CALL	qsort
		ADDSP	2, i
		CHARO	'\n', i
		LDA	11, i
		ASLA
		STA	2, s
		LDA	arr, i
		STA	0, s
		CALL	printarr
		STOP

; Sorts an array using the quicksort algorithm
;
; Parameters:
;	- SP + 2: Address of the array
;	- SP + 4: Left bound
;	- SP + 6: Right bound
; Returns:
;	void
qsort:		SUBSP	2, i
		LDA	qsarrlb, s
		CPA	qsarrrb, s
		BRGE	qsortout
		SUBSP	6, i
		LDA	10, s
		STA	0, s
		LDA	12, s
		STA	2, s
		LDA	14, s
		STA	4, s
		CALL	part
		LDA	10, s
		STA	0, s
		LDA	12, s
		STA	2, s
		LDA	6, s
		SUBA	1, i
		STA	4, s
		CALL	qsort
		LDA	10, s
		STA	0, s
		LDA	6, s
		ADDA	1, i
		STA	2, s
		LDA	14, s
		STA	4, s
		CALL	qsort
		ADDSP	6, i
qsortout:	ADDSP	2, i
		RET0
; Address of the array
qsarradd:	.EQUATE 4
; Left bound
qsarrlb:	.EQUATE 6
; Right bound
qsarrrb:	.EQUATE 8
; Pivot value returned by the part command
qsortp:		.EQUATE 0

; Partitions an array in two following the quicksort rules.
;
; All the lower values compared to the pivot will be on the left
; All the upper values compared to the pivot will be on the right
; The pivot's final index is then returned
;
; Parameters:
;	- SP + 2: Address of the array
;	- SP + 4: Left bound
;	- SP + 6: Right bound
;
; Returns:
;	- SP + 8: Pivot final index
part:		SUBSP	8, i
		LDA	parrrb, s
		STA	partpiv, s
		LDA	parrlb, s
		STA	pstind, s
		STA	piter, s
partflp:	CPA	parrrb, s
		BRGE	partout
		LDX	piter, s
		ASLX
		LDA	paraddr, sxf
		STA	parrival, s
		LDX	partpiv, s
		ASLX
		LDA	paraddr, sxf
		CPA	parrival, s
		BRLT	parlpinc
		SUBSP	6, i ; Call swap(arr, i, st_index)
		LDA	16, s
		STA	0, s
		LDA	8, s
		STA	2, s
		LDA	10, s
		STA	4, s
		CALL	swap
		ADDSP	6, i
		LDA	pstind, s
		ADDA	1, i
		STA	pstind, s
parlpinc:	LDA	piter, s
		ADDA	1, i
		STA	piter, s
		BR	partflp
partout:	SUBSP	6, i ; Call swap(arr, piv, st_index)
		LDA	16, s
		STA	0, s
		LDA	12, s
		STA	2, s
		LDA	10, s
		STA	4, s
		CALL	swap
		ADDSP	6, i
		LDA	pstind, s
		ADDSP	8, i
		STA	8, s
		RET0
; Address of the array		
paraddr:	.EQUATE 10
; Left bound
parrlb:		.EQUATE 12
; Right bound
parrrb:		.EQUATE 14
; Pivot value
partpiv:	.EQUATE 6
; st_index
pstind:		.EQUATE 4
; For iterator value
piter:		.EQUATE 2
; arr[i] value 
parrival:	.EQUATE 0

; Swaps the value of two elements of an array of integers
;
; Parameters:
;	- SP + 2: Address of the array
;	- SP + 4: Index of the 1st element to swap
;	- SP + 6: Index of the 2nd element to swap
;
; Returns:
;	void
swap:		SUBSP	2, i
		LDX	fstelind, s
		ASLX
		LDA	arraddr, sxf
		STA	swaptmp, s
		LDX	secelind, s
		ASLX
		LDA	arraddr, sxf
		LDX	fstelind, s
		ASLX
		STA	arraddr, sxf
		LDA	swaptmp, s
		LDX	secelind, s
		ASLX
		STA	arraddr, sxf
		ADDSP	2, i
		RET0
; Temporary value for the swap
swaptmp:	.EQUATE 0
; Address of the array on which the swap is done
arraddr:	.EQUATE 4
; Index of the first element
fstelind:	.EQUATE 6
; Index of the second element
secelind:	.EQUATE 8

; Prints the content of an array
;
; Parameters:
;	SP + 2: Address of the array
;	SP + 4: Length of the array
;
; Returns:
;	void
printarr:	LDX	0, i
parrlp:		CPX	4, s
		BRGE	parrout
		DECO	2, sxf
		CHARO	' ', i
		ADDX	2, i
		BR	parrlp
parrout:	RET0

; Unsorted array for testing purposes
arr:	.WORD 9
	.WORD 5
	.WORD 8
	.WORD 10
	.WORD 4
	.WORD 7
	.WORD 0
	.WORD 3
	.WORD 2
	.WORD 1
	.WORD 6

	.END
