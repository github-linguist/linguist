.class public op1
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
        getstatic java/lang/System/out Ljava/io/PrintStream;
        ldc 0x1
        ldc 0x1
        iadd
        invokevirtual java/io/PrintStream/println(I)V

    .line 3
        getstatic java/lang/System/out Ljava/io/PrintStream;
        ldc 0xa
        ldc 0x5
        isub
        invokevirtual java/io/PrintStream/println(I)V

    .line 4
        getstatic java/lang/System/out Ljava/io/PrintStream;
        ldc 0x15
        ldc 0x3
        idiv
        invokevirtual java/io/PrintStream/println(I)V

    .line 5
        getstatic java/lang/System/out Ljava/io/PrintStream;
        ldc 0x159
        ldc 0x38
        imul
        invokevirtual java/io/PrintStream/println(I)V

    .line 6
        getstatic java/lang/System/out Ljava/io/PrintStream;
        ldc 0x52
        ldc 0x9
        irem
        invokevirtual java/io/PrintStream/println(I)V

EndGlobal:
return
.end method