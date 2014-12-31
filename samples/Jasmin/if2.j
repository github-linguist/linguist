.class public if2
.super java/lang/Object
;
; standard initializer (calls java.lang.Object's initializer)
;
.method public <init>()V
aload_0
invokenonvirtual java/lang/Object/<init>()V
return
.end method

.method public static main([Ljava/lang/String;)V

.limit locals 1
.limit stack 5
BeginGlobal:
	.line 2
			ldc		0x1
			ldc		0x1
			if_icmpeq	Cmp1893841232
			ldc		0x0
			goto		CmpDone1893841232
		Cmp1893841232:
			ldc		0x1
		CmpDone1893841232:
		ldc		0x1
		if_icmpeq		If-1736765035
		goto		IfElse-1736765035
	If-1736765035:
	.line 2
		.line 3
			getstatic		java/lang/System/out Ljava/io/PrintStream;
		ldc		0x1
			invokevirtual		java/io/PrintStream/print(I)V
		goto		IfDone-1736765035
	IfElse-1736765035:
	.line 4
		.line 4
				ldc		0x2
				ldc		0x1
				if_icmpeq	Cmp-1460884369
				ldc		0x0
				goto		CmpDone-1460884369
			Cmp-1460884369:
				ldc		0x1
			CmpDone-1460884369:
			ldc		0x1
			if_icmpeq		If-247349760
			goto		IfElse-247349760
		If-247349760:
		.line 4
			.line 5
				getstatic		java/lang/System/out Ljava/io/PrintStream;
			ldc		0x2
				invokevirtual		java/io/PrintStream/print(I)V
			goto		IfDone-247349760
		IfElse-247349760:
		.line 6
			.line 7
				getstatic		java/lang/System/out Ljava/io/PrintStream;
			ldc		0x3
				invokevirtual		java/io/PrintStream/print(I)V
		IfDone-247349760:
	IfDone-1736765035:

	.line 10
			ldc		0x1
			ldc		0x2
			if_icmpeq	Cmp933554851
			ldc		0x0
			goto		CmpDone933554851
		Cmp933554851:
			ldc		0x1
		CmpDone933554851:
		ldc		0x1
		if_icmpeq		If1623625546
		goto		IfElse1623625546
	If1623625546:
	.line 10
		.line 11
			getstatic		java/lang/System/out Ljava/io/PrintStream;
		ldc		0x1
			invokevirtual		java/io/PrintStream/print(I)V
		goto		IfDone1623625546
	IfElse1623625546:
	.line 12
		.line 12
				ldc		0x2
				ldc		0x2
				if_icmpeq	Cmp1572138409
				ldc		0x0
				goto		CmpDone1572138409
			Cmp1572138409:
				ldc		0x1
			CmpDone1572138409:
			ldc		0x1
			if_icmpeq		If126354425
			goto		IfElse126354425
		If126354425:
		.line 12
			.line 13
				getstatic		java/lang/System/out Ljava/io/PrintStream;
			ldc		0x2
				invokevirtual		java/io/PrintStream/print(I)V
			goto		IfDone126354425
		IfElse126354425:
		.line 14
			.line 15
				getstatic		java/lang/System/out Ljava/io/PrintStream;
			ldc		0x3
				invokevirtual		java/io/PrintStream/print(I)V
		IfDone126354425:
	IfDone1623625546:

	.line 18
			ldc		0x1
			ldc		0x2
			if_icmpeq	Cmp126493150
			ldc		0x0
			goto		CmpDone126493150
		Cmp126493150:
			ldc		0x1
		CmpDone126493150:
		ldc		0x1
		if_icmpeq		If1522284422
		goto		IfElse1522284422
	If1522284422:
	.line 18
		.line 19
			getstatic		java/lang/System/out Ljava/io/PrintStream;
		ldc		0x1
			invokevirtual		java/io/PrintStream/print(I)V
		goto		IfDone1522284422
	IfElse1522284422:
	.line 20
		.line 20
				ldc		0x2
				ldc		0x1
				if_icmpeq	Cmp-906666545
				ldc		0x0
				goto		CmpDone-906666545
			Cmp-906666545:
				ldc		0x1
			CmpDone-906666545:
			ldc		0x1
			if_icmpeq		If1083939031
			goto		IfElse1083939031
		If1083939031:
		.line 20
			.line 21
				getstatic		java/lang/System/out Ljava/io/PrintStream;
			ldc		0x2
				invokevirtual		java/io/PrintStream/print(I)V
			goto		IfDone1083939031
		IfElse1083939031:
		.line 22
			.line 23
				getstatic		java/lang/System/out Ljava/io/PrintStream;
			ldc		0x3
				invokevirtual		java/io/PrintStream/print(I)V
		IfDone1083939031:
	IfDone1522284422:

EndGlobal:
return
.end method

