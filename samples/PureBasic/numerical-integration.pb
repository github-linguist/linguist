Prototype.d TestFunction(Arg.d)

Procedure.d LeftIntegral(Start, Stop, Steps, *func.TestFunction)
  Protected.d n=(Stop-Start)/Steps, sum, x=Start
  While x <= Stop-n
    sum + n * *func(x)
    x + n
  Wend
  ProcedureReturn sum
EndProcedure

Procedure.d MidIntegral(Start, Stop, Steps, *func.TestFunction)
  Protected.d n=(Stop-Start)/Steps, sum, x=Start
  While x <= Stop-n
    sum + n * *func(x+n/2)
    x + n
  Wend
  ProcedureReturn sum
EndProcedure

Procedure.d RightIntegral(Start, Stop, Steps, *func.TestFunction)
  Protected.d n=(Stop-Start)/Steps, sum, x=Start
  While x < Stop
    x + n
    sum + n * *func(x)
  Wend
  ProcedureReturn sum
EndProcedure

Procedure.d Trapezium(Start, Stop, Steps, *func.TestFunction)
  Protected.d n=(Stop-Start)/Steps, sum, x=Start
  While x<=Stop
    sum + n * (*func(x) + *func(x+n))/2
    x+n
  Wend
  ProcedureReturn sum
EndProcedure

Procedure.d Simpson(Start, Stop, Steps, *func.TestFunction)
  Protected.d n=(Stop-Start)/Steps, sum1, sum2, x=Start
  Protected i
  For i=0 To steps-1
    sum1+ *func(Start+n*i+n/2)
  Next
  For i=1 To Steps-1
    sum2+ *func(Start+n*i)
  Next
  ProcedureReturn n * (*func(Start)+ *func(Stop)+4*sum1+2*sum2) / 6
EndProcedure

;- Set up functions to integrate
Procedure.d Test1(n.d)
  ProcedureReturn n*n*n
EndProcedure

Procedure.d Test2(n.d)
  ProcedureReturn 1/n
EndProcedure

; This function should be integrated as a integer function, but for
; comparably this will stay as a float.
Procedure.d Test3(n.d)
  ProcedureReturn n
EndProcedure

;- Test the code & present the results
CompilerIf #PB_Compiler_Debugger
  MessageRequester("Notice!","Running this program in Debug-mode will be slow")
CompilerEndIf

; = 0.25
Define Answer$
Answer$="Left     ="+StrD(LeftIntegral (0,1,100,@Test1()))+#CRLF$
Answer$+"Mid      ="+StrD(MidIntegral  (0,1,100,@Test1()))+#CRLF$
Answer$+"Right    ="+StrD(RightIntegral(0,1,100,@Test1()))+#CRLF$
Answer$+"Trapezium="+StrD(Trapezium    (0,1,100,@Test1()))+#CRLF$
Answer$+"Simpson  ="+StrD(Simpson      (0,1,100,@Test1()))
MessageRequester("Answer should be 1/4",Answer$)

; = Ln(100) e.g. ~4.60517019...
Answer$="Left     ="+StrD(LeftIntegral  (1,100,1000,@Test2()))+#CRLF$
Answer$+"Mid      ="+StrD(MidIntegral   (1,100,1000,@Test2()))+#CRLF$
Answer$+"Right    ="+StrD(RightIntegral (1,100,1000,@Test2()))+#CRLF$
Answer$+"Trapezium="+StrD(Trapezium     (1,100,1000,@Test2()))+#CRLF$
Answer$+"Simpson  ="+StrD(Simpson       (1,100,1000,@Test2()))
MessageRequester("Answer should be Ln(100), e.g. ~4.60517019",Answer$)

; 12,500,000
Answer$="Left     ="+StrD(LeftIntegral  (0,5000,5000000,@Test3()))+#CRLF$
Answer$+"Mid      ="+StrD(MidIntegral   (0,5000,5000000,@Test3()))+#CRLF$
Answer$+"Right    ="+StrD(RightIntegral (0,5000,5000000,@Test3()))+#CRLF$
Answer$+"Trapezium="+StrD(Trapezium     (0,5000,5000000,@Test3()))+#CRLF$
Answer$+"Simpson  ="+StrD(Simpson       (0,5000,5000000,@Test3()))
MessageRequester("Answer should be 12,500,000",Answer$)

; 18,000,000
Answer$="Left     ="+StrD(LeftIntegral  (0,6000,6000000,@Test3()))+#CRLF$
Answer$+"Mid      ="+StrD(MidIntegral   (0,6000,6000000,@Test3()))+#CRLF$
Answer$+"Right    ="+StrD(RightIntegral (0,6000,6000000,@Test3()))+#CRLF$
Answer$+"Trapezium="+StrD(Trapezium     (0,6000,6000000,@Test3()))+#CRLF$
Answer$+"Simpson  ="+StrD(Simpson       (0,6000,6000000,@Test3()))
MessageRequester("Answer should be 18,000,000",Answer$)
