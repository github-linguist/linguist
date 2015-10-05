.class public op4
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
		if_icmpne	Cmp-191731100
		ldc		0x0
		goto		CmpDone-191731100
	Cmp-191731100:
		ldc		0x1
	CmpDone-191731100:
		invokevirtual		java/io/PrintStream/println(Z)V

	.line 3
		getstatic		java/lang/System/out Ljava/io/PrintStream;
		ldc		0x0
		ldc		0x0
		if_icmpne	Cmp-901585603
		ldc		0x0
		goto		CmpDone-901585603
	Cmp-901585603:
		ldc		0x1
	CmpDone-901585603:
		invokevirtual		java/io/PrintStream/println(Z)V

	.line 4
		getstatic		java/lang/System/out Ljava/io/PrintStream;
		ldc		0x1
		ldc		0x1
		if_icmpne	Cmp1522577937
		ldc		0x0
		goto		CmpDone1522577937
	Cmp1522577937:
		ldc		0x1
	CmpDone1522577937:
		invokevirtual		java/io/PrintStream/println(Z)V

	.line 5
		getstatic		java/lang/System/out Ljava/io/PrintStream;
		ldc		0x1
		ldc		0x0
		if_icmpne	Cmp-1653028684
		ldc		0x0
		goto		CmpDone-1653028684
	Cmp-1653028684:
		ldc		0x1
	CmpDone-1653028684:
		invokevirtual		java/io/PrintStream/println(Z)V

EndGlobal:
return
.end method

