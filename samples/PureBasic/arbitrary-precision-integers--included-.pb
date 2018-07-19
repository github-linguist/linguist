IncludeFile "Decimal.pbi"

;- Declare the variables that will be used
Define.Decimal *a
Define n, L$, R$, out$, digits.s

;- 4^3^2 is withing 32 bit range, so normal procedures can be used
n=Pow(4,Pow(3,2))

;- 5^n is larger then 31^2, so the same library call as in the "Long multiplication" task is used
*a=PowerDecimal(IntegerToDecimal(5),IntegerToDecimal(n))

;- Convert the large number into a string & present the results
out$=DecimalToString(*a)
L$ = Left(out$,20)
R$ = Right(out$,20)
digits=Str(Len(out$))
out$="First 20 & last 20 chars of 5^4^3^2 are;"+#CRLF$+L$+#CRLF$+R$+#CRLF$
out$+"and the result is "+digits+" digits long."

MessageRequester("Arbitrary-precision integers, PureBasic",out$)
