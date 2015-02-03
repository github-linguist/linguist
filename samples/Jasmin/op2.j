.class public op2
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
		ldc		0x1
		ldc		0x0
		iand
		invokevirtual		java/io/PrintStream/println(Z)V

	.line 3
		getstatic		java/lang/System/out Ljava/io/PrintStream;
		ldc		0x1
		ldc		0x0
		ior
		invokevirtual		java/io/PrintStream/println(Z)V

EndGlobal:
return
.end method

