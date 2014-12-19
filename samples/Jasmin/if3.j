.class public if3
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
		if_icmpeq		If-811796083
		goto		IfElse-811796083
	If-811796083:
	.line 3
		.line 3
			ldc		0x0
			ldc		0x1
			if_icmpeq		If-1001319390
			goto		IfElse-1001319390
		If-1001319390:
		.line 4
			.line 4
				getstatic		java/lang/System/out Ljava/io/PrintStream;
			ldc		0x1
				invokevirtual		java/io/PrintStream/print(I)V
			goto		IfDone-1001319390
		IfElse-1001319390:
		.line 6
			.line 6
				getstatic		java/lang/System/out Ljava/io/PrintStream;
			ldc		0x2
				invokevirtual		java/io/PrintStream/print(I)V
		IfDone-1001319390:
		goto		IfDone-811796083
	IfElse-811796083:
	.line 8
		.line 8
			getstatic		java/lang/System/out Ljava/io/PrintStream;
		ldc		0x3
			invokevirtual		java/io/PrintStream/print(I)V
	IfDone-811796083:

EndGlobal:
return
.end method

