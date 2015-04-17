.class public if4
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
		if_icmpne		IfNot1919266740
	.line 2
		.line 2
			ldc		0x1
			ldc		0x1
			if_icmpne		IfNot613368541
		.line 2
			.line 2
				getstatic		java/lang/System/out Ljava/io/PrintStream;
			ldc		0x1
				invokevirtual		java/io/PrintStream/print(I)V
		IfNot613368541:
	IfNot1919266740:

EndGlobal:
return
.end method

