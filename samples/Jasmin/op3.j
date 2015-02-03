.class public op3
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
		getstatic		java/lang/System/out Ljava/io/PrintStream;
		ldc		0x0
		ldc		0x1
		if_icmpeq	Cmp-1307183590
		ldc		0x0
		goto		CmpDone-1307183590
	Cmp-1307183590:
		ldc		0x1
	CmpDone-1307183590:
		invokevirtual		java/io/PrintStream/println(Z)V

	.line 3
		getstatic		java/lang/System/out Ljava/io/PrintStream;
		ldc		0x0
		ldc		0x0
		if_icmpeq	Cmp-1443270821
		ldc		0x0
		goto		CmpDone-1443270821
	Cmp-1443270821:
		ldc		0x1
	CmpDone-1443270821:
		invokevirtual		java/io/PrintStream/println(Z)V

	.line 4
		getstatic		java/lang/System/out Ljava/io/PrintStream;
		ldc		0x1
		ldc		0x1
		if_icmpeq	Cmp1759327329
		ldc		0x0
		goto		CmpDone1759327329
	Cmp1759327329:
		ldc		0x1
	CmpDone1759327329:
		invokevirtual		java/io/PrintStream/println(Z)V

	.line 5
		getstatic		java/lang/System/out Ljava/io/PrintStream;
		ldc		0x1
		ldc		0x0
		if_icmpeq	Cmp-678570146
		ldc		0x0
		goto		CmpDone-678570146
	Cmp-678570146:
		ldc		0x1
	CmpDone-678570146:
		invokevirtual		java/io/PrintStream/println(Z)V

EndGlobal:
return
.end method

