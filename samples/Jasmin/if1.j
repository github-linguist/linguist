.class public if1
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
		if_icmpeq		If556261059
		goto		IfElse556261059
	If556261059:
	.line 3
		.line 3
			getstatic		java/lang/System/out Ljava/io/PrintStream;
		ldc		0x1
			invokevirtual		java/io/PrintStream/print(I)V
		goto		IfDone556261059
	IfElse556261059:
	.line 5
		.line 5
			getstatic		java/lang/System/out Ljava/io/PrintStream;
		ldc		0x2
			invokevirtual		java/io/PrintStream/print(I)V
	IfDone556261059:

	.line 6
		getstatic		java/lang/System/out Ljava/io/PrintStream;
	ldc		0x3
		invokevirtual		java/io/PrintStream/print(I)V

	.line 7
		ldc		0x1
		ldc		0x1
		if_icmpne		IfNot-920218690
	.line 8
		.line 8
			getstatic		java/lang/System/out Ljava/io/PrintStream;
		ldc		0x4
			invokevirtual		java/io/PrintStream/print(I)V
	IfNot-920218690:

EndGlobal:
return
.end method

