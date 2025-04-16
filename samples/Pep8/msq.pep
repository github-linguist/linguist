; Reads a square from stdin, then computes whether it is a magic square or not.
;
; A Magic Square is a square following a specific set of rules, namely:
; - The sum of each row must be the same as the sum of the diagonal
; - The sum of the anti-diagonal must be the same as the sum of the diagonal
; - The sum of each column must be the same as the sum of the diagonal
;
; If any column, row, or anti-diagonal does not follow the aformented rules,
; the program will output its number to stdout.
;
; Columns are identified by a negative digit, ranging from -1 to -n
; The anti-diagonal is identified by the number 0.
; Finally, rows are identified by a positive integer, ranging from 1 to n.
;
; Formatting:
; First a number `n` is read from Stdin, it will determine the size of the square
; Then, enter the data for the square, `n` entries will be read
; The data is sequentially added to the square in memory, from the upper-left corner
; to the lower-right corner, in a zig-zag pattern
;
; Example:
; 3
; 4   9   3
; 3   5   7
; 8   1   6
;
; Limitation: Since there is no dynamic allocation, the size
; of the square is capped at a maximum of 32*32.
; Any size lower than 1 or higher than 32 will produce
; an error and the termination of the program.

;_start
		DECI	sidelen, d
		LDA	sidelen, d
		CPA	1, i
		BRLT	sderror
		CPA	32, i
		BRGT	sderror
		LDX	sidelen, d
		CALL	mult
		STA	sqlen, d
		CALL	fillsq
		LDA	sidelen, d
		LDX	square, i
		CALL	diagsum
		STA	dgsm, d
		CALL	colsums
		LDA	sidelen, d
		LDX	square, i
		CALL	cdiagsum
		CPA	dgsm, d
		BREQ	cnt
		DECO	0, i
		CHARO	'\n', i
cnt:		STA	cdsm, d
		CALL	rowsums
		STOP
el:		.BLOCK 2
; Length of a side of the square
sidelen:	.WORD 0
; Total length  of the square
sqlen:		.BLOCK 2
; 32 * 32 square of integers
square: 	.BLOCK 255
		.BLOCK 255
		.BLOCK 255
		.BLOCK 255
		.BLOCK 255
		.BLOCK 255
		.BLOCK 255
		.BLOCK 255
		.BLOCK 8

; Prints an error and terminates the program
sderror:	STRO stderr, d
		STOP

; Parameters:	A: Size of a side of the square
;		X: Base address of the square
;		cscolid: Identifier of the column (0-based)

; Computes the sum of each column
; If the sum is not the same as dgsm, its index will be printed (in negative form)
;
; Parameters:	A: Size of a side of the square
;		X: Address of the square
;
; Return:	void
colsums:STA	clsmsqsz, d
		STX	clsmsqad, d
		SUBA	1, i
		STA	clsmyp, d
clssmlp:CPA	0 ,i
		BRLT	clsmout
		STA	cscolid, d
		LDA	clsmsqsz, d
		LDX	clsmsqsz, d
		CALL	colsum
		CPA	dgsm, d
		BREQ	clsdecpt
		LDX	clsmyp, d
		NEGX
		STX	clsmyp, d
		DECO	clsmyp, d
		CHARO	'\n', i
		LDX	clsmyp, d
		NEGX
		STX	clsmyp, d
clsdecpt:	LDA	clsmyp, d
		SUBA	1, i
		STA	clsmyp, d
		BR	clssmlp
clsmout:	RET0
clsmsqad:	.BLOCK 2
clsmsqsz:	.BLOCK 2
clsmyp_:		.BLOCK 2

; Compute the sum of each row
; Prints its index if the value does not match dgsum
;
; Parameters:	A: Size of a side of the square
;		X: Address of the square
;
; Returns:	void
rowsums:	STA	maxrows, d
		STX	rowssqad, d
		LDA	0, i
		STA	tmprwsm, d
		STA	rowid, d
rwsmslp:	CPA	maxrows, d
		BRGE	rwsmsout
		STA	rwxpos, d
		LDA	maxrows, d
		LDX	rowssqad, d
		CALL	rowsum
		CPA	dgsm, d
		STA	tmprwsm, d
		BREQ	rwinccpt
		DECO	rowid, d
		CHARO	'\n', i
rwinccpt:	LDA	rowid, d
		ADDA	1, i
		STA	rowid, d
		BR	rwsmslp
rwsmsout:	RET0
; Number of rows to compute
maxrows:	.BLOCK 2
; Square address
rowssqad:	.BLOCK 2
; Current rowid
rowid:		.BLOCK 2
; Current rowsum
tmprwsm:	.BLOCK 2

; Gets an element at the indexes given as parameter
; The square is supposed to contain only integers
; No check will be made on the correctness of the indexes
;
; Parameters:	A: Size of a side of the square (in elements)
;		X: Base address of the square
;		xpos: Position in X for the element (0-indexed)
;		ypos: Position in Y for the element (0-indexed)
;
; Return:	A will contain the element
;
; Side-effects: Registers A and X will neither be saved nor restored upon call
;		ypos will be altered
elemat:		STX	elsqaddr, d
		ASLA
		LDX	xpos, d
		CALL	mult
		STA	xpos, d
		LDX	ypos, d
		ASLX
		STX	ypos, d
		ADDA	ypos, d
		ADDA	elsqaddr, d
		STA	elsqaddr, d
		LDX	elsqaddr, d
		LDA	0, x
		RET0
; X-index in square (in elements)
xpos:		.BLOCK 2
; Y-index in square (in elements)
ypos:		.BLOCK 2
; Address to fetch elements at
elsqaddr:	.BLOCK 2

; Fills the square with input from the user
;
; Pass via register A the number of inputs to be read
fillsq:		LDX	0, i
filloop:	SUBA	1, i
		CPA	0, i
		BRLT	fillout
		DECI	square, x
		ADDX	2, i
		BR	filloop
fillout:	RET0

; Computes the sum of the digits of a column
; The square is supposed to contain integers only
;
; Parameters:	A: Size of a side of the square
;		X: Base address of the square
;		cscolid: Identifier of the column (0-based)
;
; Return:	A: Sum of the digits of the column
colsum:		STA	csclsqsz, d
		STX	csclsqad, d
		LDA	0, i
		STA	csclsum, d
		STA	csclxpos, d
clsmloop:	CPA	csclsqsz, d
		BRGE	colout
		LDA	cscolid, d
		STA	ypos, d
		LDA	csclxpos, d
		STA	xpos, d
		LDA	csclsqsz, d
		LDX	csclsqad, d
		CALL	elemat
		ADDA	csclsum, d
		STA	csclsum, d
		LDA	csclxpos, d
		ADDA	1, i
		STA	csclxpos, d
		BR	clsmloop
colout:		LDA	csclsum, d
		RET0
; Identifier of the column which sum is to be computed
cscolid:	.BLOCK 2
; Temporary for x position
csclxpos:	.BLOCK 2
; Base address of the square
csclsqad:	.BLOCK 2
; Size of a side of the square
csclsqsz:	.BLOCK 2
; Sum of the column
csclsum:	.BLOCK 2

; Computes the sum of the digits of a row
; The square is supposed to contain integers only
;
; Parameters:	A: Size of a side of the square
;		X: Base address of the square
;		rwxpos: Row index (0-based)
;
; Returns:	A: Sum of the digits of the row
rowsum:		STA	rwsqsz, d
		STX	rwbsqadr, d
		LDA	0,i
		STA	rwsum, d
		STA	rwypos, d
rwsumlp:	LDA	rwypos, d
		CPA	rwsqsz, d
		BRGE	rwsumout
		STA	ypos, d
		LDA	rwxpos, d
		STA	xpos, d
		LDA	rwsqsz, d
		LDX	rwbsqadr, d
		CALL	elemat
		ADDA	rwsum, d
		STA	rwsum, d
		LDA	rwypos, d
		ADDA	1, i
		STA	rwypos, d
		BR	rwsumlp
rwsumout:	LDA	rwsum, d
		RET0
; Square size (in elements)
rwsqsz:		.BLOCK 2
; Square base address
rwbsqadr:	.BLOCK 2
; Position of the row to compute
rwxpos:		.BLOCK 2
; Current column visited
rwypos:		.BLOCK 2
; Sum of the row
rwsum:		.BLOCK 2

; Computes the sum for the antidiagonal of a square
; The square is supposed to contain integers only
;
; Parameters:	A: Size of a side of the square (elements)
;		X: Base address of the square
;
; Returns:	A: Sum of the antidiagonal
cdiagsum:	STA	cdsqsz, d
		SUBA	1,i
		STA	cdtmpy, d
		LDA	0, i
		STA	cdtmpx, d
		STA	cdsum, d
		STX	cdsqaddr, d
cdiaglp:	LDA	cdtmpx, d
		STA	xpos, d
		LDA	cdtmpy, d
		STA	ypos, d
		CPA	0, i
		BRLT	cdout
		LDA	cdsqsz, d
		LDX	cdsqaddr, d
		CALL	elemat
		ADDA	cdsum, d
		STA	cdsum,d
		LDA	cdtmpx, d
		ADDA	1, i
		STA	cdtmpx, d
		LDA	cdtmpy, d
		SUBA	1, i
		STA	cdtmpy, d
		BR	cdiaglp
cdout:		LDA	cdsum, d
		RET0
; Temporary handle for square size (elements)
cdsqsz:		.BLOCK 2
; Square address
cdsqaddr:	.BLOCK 2
; Keep x address
cdtmpx:		.BLOCK 2
; Keep y address
cdtmpy:		.BLOCK 2
; Sum of antidiagonal
cdsum:		.BLOCK 2

; Computes the sum for the diagonal of a square
; The square is supposed to contain integers only
;
; Parameters:	A: Size of a side of the square (elements)
;		X: Base address of the square
;
; Returns:	A: Sum of the diagonal
;
diagsum:	STA	dsqsz, d
		STX	dsqaddr, d
		LDA 	0, i
		STA	tmpsum, d
		STA	curra, d
dglp:		CPA	dsqsz, d
		BRGE	dglpout
		STA	xpos, d
		STA	ypos, d
		LDA	dsqsz, d
		LDX	dsqaddr, d
		CALL	elemat
		ADDA	tmpsum, d
		STA	tmpsum, d
		LDA	curra, d
		ADDA	1, i
		STA	curra, d
		BR	dglp
dglpout:	LDA	tmpsum, d
		RET0
; Address of the square
dsqaddr:	.BLOCK 2
; Size of a side of the square (elements)
dsqsz:		.BLOCK 2
; Current value of the x and y indexes
curra:		.BLOCK 2
; Sum of the values
tmpsum:		.BLOCK 2

; Muliplies two ints
;
; Parameters:
; Register A : Left part of the multiplication
; Register X : Right part of the multiplication
;
; Return:
; Register A : Result of the multiplication
;
; Side-effects:
; Uses multmp as a temporary value
mult:		STA	multmp, d
		LDA	0, i
muloop:		CPX	0, i
		BRLE	mulout
		ADDA	multmp, d
		SUBX 	1, i
		BR	muloop
mulout:		RET0
; Temporary variable for mult function
; Holds the initial value of A
multmp:		.WORD 0

; For debugging purposes
; Prints the content of the square to stdout
;
; Parameters: 	A: Size of a side
;		X: Base address of square
;
; Side-effects:
;		Consider variables sidesz, sqaddr, sqmaxa as local, they will be written
;		Registers A and X will not be saved nor restored upon call
printsq:	STA	sidesz, d
		STX	sqaddr, d
		LDX	sidesz, d
		CALL	mult
		ASLA
		ADDA	sqaddr, d
		STA	sqmaxa, d
		LDX	sqaddr, d
		LDA	0, i
priloop:	DECO	0, x
		CHARO	' ', i
		ADDX	2, i
		CPX	sqmaxa, d
		BREQ	priout
		ADDA	1, i
		CPA	sidesz, d
		BRLT	priloop
		LDA 	0, i
		CHARO	'\n', i
		BR	priloop
priout:		RET0
; Size of a side of the square
sidesz: 	.BLOCK 2
; Address of the square
sqaddr: 	.BLOCK 2
; Maximum address to iterate upon
sqmaxa: 	.BLOCK 2

; ------------------ GLOBALLY ACCESSIBLE SYMBOLS -------------------- ;
;
; Sum of the diagonal for the square
; Reference value for magic-square
dgsm:		.WORD 0
; Sum of the counter-diagonal
cdsm:		.WORD 0
; Input error string
stderr:		.ASCII "A number between 1 and 32 (both inclusive) must be entered as value for the size of the square for the program to work.\n\x00"
		.END
