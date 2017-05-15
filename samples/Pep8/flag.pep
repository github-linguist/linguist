_start:	LDA	0,i
	LDX	0,i
	LDA	20, i
	ADDA	51, i
	CPA	0,i
	BRLT	s3
	BR	s4
s1:	LDBYTEA	s3, x
	NOTA
	STBYTEA s3, x
	ADDX	1,i
	CPX	12, i
	BRNE	s1
s2:	STOP
s4:	LDA	31, d
	LDX	50, d
	RET0
	STOP
s3:	CPX	-27746, d
	ANDX	-8241, i
	SUBA	-12337, sxf
	LDX	-12289, sx
	.END
