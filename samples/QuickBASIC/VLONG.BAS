'********** VLong.Bas demos three math functions and eight byte packing

'Copyright (c) 1988 Paul Passarelli
'Copyright (c) 1988 Crescent Software


DEFINT A-Z
DECLARE SUB VLAdd (Addend1#, Addend2#, Sum#, ErrFlag%)
DECLARE SUB VLSub (Minuend#, Subtrahend#, Difference#, ErrFlag%)
DECLARE SUB VLMul (Multiplicand#, Multiplier#, Product#, ErrFlagg%)
DECLARE SUB VLPack (Number$, Value#, ErrFlag%)
DECLARE SUB VLUnpack (Value#, Number$, ErrFlag%)
DECLARE FUNCTION StripZ$ (X$)            'strips leading zeros for the demo


CLS
LINE INPUT "Enter a big number (up to 19 digits): ", Num1$
LINE INPUT "            Enter another big number: ", Num2$
PRINT

VLPack Num1$, Num1#, ErrFlag
IF ErrFlag% GOTO ErrHandler

VLPack Num2$, Num2#, ErrFlag
IF ErrFlag% GOTO ErrHandler

VLAdd Num1#, Num2#, Sum#, ErrFlag
UPSum$ = SPACE$(20)
VLUnpack Sum#, UPSum$, ErrFlag%
IF ErrFlag% GOTO ErrHandler

PRINT Num1$; " + "; Num2$; " = "; StripZ$(UPSum$)

VLSub Num1#, Num2#, Sum#, ErrFlag
UPSum$ = SPACE$(20)
VLUnpack Sum#, UPSum$, ErrFlag%
IF ErrFlag% GOTO ErrHandler

PRINT Num1$; " - "; Num2$; " = "; StripZ$(UPSum$)

PRINT
VLPack "2", Num3#, ErrFlag
VLMul Num1#, Num3#, Prod#, ErrFlag
IF ErrFlag% GOTO ErrHandler
VLUnpack Prod#, UPSum$, ErrFlag%
PRINT Num1$; " * 2 = "; StripZ$(UPSum$)

VLPack "3", Num3#, ErrFlag
VLMul Num1#, Num3#, Prod#, ErrFlag
IF ErrFlag% GOTO ErrHandler
VLUnpack Prod#, UPSum$, ErrFlag%
PRINT Num1$; " * 3 = "; StripZ$(UPSum$)

END

ErrHandler:
PRINT "Error - press any key ";

FUNCTION StripZ$ (X$)
    FOR X = 2 TO LEN(X$)
        IF MID$(X$, X, 1) <> "0" THEN
           StripZ$ = LEFT$(X$, 1) + MID$(X$, X)
           EXIT FUNCTION
        END IF
    NEXT
END FUNCTION

