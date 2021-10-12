#include "windows.bi"
#include "crt/setjmp.bi"

#define VERBOSE 0   ' 0 not verbose , not 0  verbose mode

If VERBOSE Then Print "in verbose mode"


Declare Function raise cdecl Alias "raise"(ByVal signal As Long) As Long

Declare Function setjmp2 cdecl Alias "_setjmp" (ByVal As jmp_buf Ptr, ByVal As Any Ptr = 0) As Long

Declare Function VectoredHandler(ByVal pexp As PEXCEPTION_POINTERS) As Long

Type exdata
	jump   As jmp_buf Ptr
	icod   As Long
	file   As String
	proc   As String
	msg    As String
	Line   As Long
	h1		 As Any Ptr
End Type

ReDim Shared As exdata		e_exceptions(0 To 1)
Dim Shared As Integer 		e_pos

e_exceptions(0).h1 = AddVectoredExceptionHandler(0 , @VectoredHandler)



#define Try Do : e_pos += 1  : _
If UBound(e_exceptions) < e_pos Then : ReDim Preserve e_exceptions(0 To UBound(e_exceptions) * 2 ) : End If : _
e_exceptions(e_pos).jump = New jmp_buf : _
e_exceptions(e_pos).h1 = AddVectoredExceptionHandler(e_pos , @VectoredHandler) : _
e_exceptions(e_pos).icod = setjmp2(e_exceptions(e_pos).jump): If (e_exceptions(e_pos).icod = 0) Then : _
	If VERBOSE Then Print Space(e_pos) & "in Try block , e_pos = " & e_pos

	#macro Catch(e , _type)
ElseIf e_exceptions(e_pos).icod = _type Then
	Dim As exdata e = e_exceptions(e_pos)
	If VERBOSE Then Print Space(e_pos) & " in Catch block , e_pos = " & e_pos
	#endmacro

	#macro Catch_Any(e)
Else
	Dim As exdata e = e_exceptions(e_pos)
	If VERBOSE Then Print Space(e_pos) & " in Catch_any block , e_pos = " & e_pos
	#endmacro

	#define Finally End If: If e_pos Then:

		#define EndTry  Delete e_exceptions(e_pos).jump : RemoveVectoredExceptionHandler(e_exceptions(e_pos).h1) : _
		e_pos -= 1 : End If  : Exit Do : Loop

#define THROW(_type)   _
e_exceptions(e_pos).file = __FILE__: _
e_exceptions(e_pos).proc = __FUNCTION__: _
e_exceptions(e_pos).line = __LINE__: _
e_exceptions(e_pos).msg = "": _
If e_pos > 0 Then : e_exceptions(e_pos).icod = _type : longjmp(e_exceptions(e_pos).jump, _type): _
Else : Show_Catch(e_exceptions(0), 0): End If

#define THROW_MSG(_type , mess)   _
e_exceptions(e_pos).file = __FILE__: _
e_exceptions(e_pos).proc = __FUNCTION__: _
e_exceptions(e_pos).line = __LINE__: _
e_exceptions(e_pos).msg = mess: _
If e_pos > 0 Then : e_exceptions(e_pos).icod = _type : longjmp(e_exceptions(e_pos).jump, _type): _
Else : Show_Catch(e_exceptions(0), 0) : End If


Sub Show_Catch(ByRef e As exdata, ByVal flag As Integer = 1)
	Dim As String status
	If flag Then
		If VERBOSE Then Print Space(e_pos) & "  e_exceptions message : " ; e.msg
		status = "e_exceptions message : " & e.msg
	Else
		If VERBOSE Then Print Space(e_pos) & "  error exceptions : Throw outside Try-Catch bloc   "  & e.msg
		status =  "error exceptions : Throw outside Try-Catch bloc"  & Chr(10,10) & "e_exceptions message : " & e.msg
	End If
	If VERBOSE Then Print Space(e_pos) & "   e_exception code = " ; Str(e.icod)
	status &= Chr(10,10) & "   e_exception code = " & Str(e.icod)
	If VERBOSE Then Print Space(e_pos) & "   file = " ; e.file
	status &= Chr(10) & "   file = " & e.file
	If VERBOSE Then Print Space(e_pos) & "   proc = " ; e.proc
	status &= Chr(10) & "   proc = " & e.proc
	If VERBOSE Then Print Space(e_pos) & "   line = " ; Str(e.line)
	status &= Chr(10) & "   line = " & Str(e.line)
	If flag Then
		messagebox 0, status, "Catched Exception ", MB_ICONWARNING
	Else
		messagebox 0, status, "Catched Exception  wihout Try_Catch", MB_ICONWARNING
	End If
End Sub


Function VectoredHandler(ByVal pexp As PEXCEPTION_POINTERS) As Long
	Dim As PEXCEPTION_RECORD pexr = pexp -> ExceptionRecord
	Dim As PCONTEXT pctxr = pexp -> ContextRecord
	Dim As Long iflag
	Dim As String status
	If VERBOSE Then Print "Exception code : &h" ; Hex(pexr -> ExceptionCode)

	Select Case (pexr -> ExceptionCode)
	Case EXCEPTION_ACCESS_VIOLATION
		status = "Error: EXCEPTION_ACCESS_VIOLATION"
	Case EXCEPTION_ARRAY_BOUNDS_EXCEEDED
		status = "Error: EXCEPTION_ARRAY_BOUNDS_EXCEEDED"
	Case EXCEPTION_BREAKPOINT
		status = "EXCEPTION_BREAKPOINT"
		iflag = -1
	Case EXCEPTION_DATATYPE_MISALIGNMENT
		status = "Error: EXCEPTION_DATATYPE_MISALIGNMENT"
	Case EXCEPTION_ILLEGAL_INSTRUCTION
		status = "Error: EXCEPTION_ILLEGAL_INSTRUCTION"
	Case EXCEPTION_IN_PAGE_ERROR
		status = "Error: EXCEPTION_IN_PAGE_ERROR"
	Case EXCEPTION_INT_DIVIDE_BY_ZERO
		status = "Error: EXCEPTION_INT_DIVIDE_BY_ZERO"
	Case EXCEPTION_INT_OVERFLOW
		status = "Error: EXCEPTION_INT_OVERFLOW"
	Case EXCEPTION_INVALID_DISPOSITION
		status = "Error: EXCEPTION_INVALID_DISPOSITION"
	Case EXCEPTION_NONCONTINUABLE_EXCEPTION
		status = "Error: EXCEPTION_NONCONTINUABLE_EXCEPTION"
	Case EXCEPTION_PRIV_INSTRUCTION
		status = "Error: EXCEPTION_PRIV_INSTRUCTION"
	Case EXCEPTION_SINGLE_STEP
		status = "Error: EXCEPTION_SINGLE_STEP"
	Case EXCEPTION_STACK_OVERFLOW
		status = "Error: EXCEPTION_STACK_OVERFLOW"
	Case Else
		iflag = 1
	End Select

	If iflag = 0 Then
		If VERBOSE Then
			Dim RetVal As Long
			RetVal = MessageBox(0,  Chr(10) & "   Warning ..." & Chr(10,10)  _
			& "          Ok to Abort now !" & Chr(10,10) & "          Cancel to try to continue", _
			status, MB_ICONERROR Or MB_OKCANCEL Or MB_APPLMODAL Or MB_TOPMOST)
			If RetVal = IDOK Then
				raise(4) 'signal abort
			Else
				iflag = -1
			End If
		Else
			messagebox 0, "Close to abort !", status, MB_ICONERROR
			raise(4) 'signal abort
		End If
	End If

	If VERBOSE Then Print "Exception address : &h" ;
	''--------------------------------------------------------------------
	'' Increment the instruction pointer in the context record past the
	'' 1-byte breakpoint instruction to avoid having the exception recur.
	''--------------------------------------------------------------------
	#ifndef __FB_64BIT__
		If VERBOSE Then Print Hex(pctxr -> Eip)
		pctxr -> Eip += 1
	#else
		If VERBOSE Then Print Hex(pctxr -> Rip)
		pctxr -> Rip += 1
	#endif
	'return EXCEPTION_CONTINUE_EXECUTION '-1
	'return EXCEPTION_CONTINUE_SEARCH '0
	If iflag = -1 Then  THROW_MSG( - 1, status)
	Return - 1
End Function



' define e_exception types
#define DIVISION_BY_ZERO 221                     ' can be any number
#define FORCED_TO_ZERO 222                       ' can be any number
#define NOTHING_EX 250                           ' can be any number


' function that can throw an e_exceptions
Function div1(ByVal a As Integer , ByVal b As Integer) As Double
	If b = 0 And a = 0 Then
		'print "Any key to Breakpoint..."
		'sleep
		Asm .byte 0xcc                                   '' Breakpoint (INT 3)
	End If
	If b = 0 Then THROW_MSG(DIVISION_BY_ZERO, "Division by zero")
	If b > a Then THROW_MSG(FORCED_TO_ZERO, "Forced to zero")

	Try
		If  b < a Then THROW_MSG(NOTHING_EX, "nothing noticed")
		Catch_Any(e)
		Show_Catch(e)
	Finally
		Print  " div result printed"
	EndTry
	Return a / b

End Function

' test function (calls div):
Function add_div(ByVal a As Integer , ByVal b As Integer , ByVal c As Single) As Integer
	Return div1(a + b , c)
End Function

' main func:
Sub test()
	Try
		Try
			Print add_div(10 , 5 , 1)
			Catch_Any(e)
			Show_Catch(e)

		Finally
			Print  " div 1 done"
		EndTry
		Try
			Print add_div(10 , 5 , 0)
			Catch_Any(e)
			Show_Catch(e)
		Finally
			Print  " div 2 done"
		EndTry
		Try
			Print add_div(10 , 5 , 30)
			Catch_Any(e)
			Show_Catch(e)
		Finally
			Print  " div 3 done"
		EndTry
		Try
			Print add_div(0 , 0 , 0)
			Catch_Any(e)
			Show_Catch(e)
		Finally
			Print  " div 4 done"
		EndTry
		Print "put a breakpoint here"
		Asm .byte 0xcc                                   '' Breakpoint (INT 3)
		Catch_Any(e)
		Print "catched something global in test()"
		Show_Catch(e)
	Finally
		Print "finally test()"
	EndTry

End Sub

Sub stack_overflow()
	Dim As Long  foo(10000)'allocate something big on the stack
	Print "loop"
	stack_overflow()
End Sub

'**********************************************************************************************
'test code begins here
'**********************************************************************************************
Print : Print "test a breakpoint here"
Asm .byte 0xcc

Print : Print "test function here"
test()    ' nested try_catch tests

If VERBOSE Then
	Print : Print "test a segmentation fault here"
	Dim intPtr As Integer Ptr = 0
	intPtr[1000] = 1  'really difficult to segfault (probably malloc is over protected)
End If



'print : print "test a stack_overflow here"    		'uncomment these 2 lines
'stack_overflow()  ' not really trapped   it crashs before been trapped....



' the Try..Catch block
Try


	test()


	Catch(e , DIVISION_BY_ZERO)
	If e.msg <> "" Then Show_Catch(e)
	Try
		test()

		Catch_Any(e)
		Show_Catch(e)

	Finally

		Print " finally1"
	EndTry
	Catch_Any(e)                               ' this will catch any other e_exceptions (that is not DIVISION_BY_ZERO)
	Show_Catch(e)

Finally

	Print " this is executed allways, no mather if an e_exceptions was thrown or not"

EndTry


Try

	test()
	Print "extra test"
	Dim d0 As Double = div1(17, 0)

	Catch(e , DIVISION_BY_ZERO)
	Show_Catch(e)

	Try
		test()
		Catch_Any(e)
		Show_Catch(e)
	Finally
		Print " finally2"
	EndTry
	Catch_Any(e)                               ' this will catch any other e_exceptions (that is not DIVISION_BY_ZERO)
	Show_Catch(e)
Finally
	Print " last finally"
EndTry

Try
	Dim k As String
	'print div(17 , 25)
	Print " test ctrl-c to abort ,  any key to continue"    'the ctrl-c aborts as normal , not trapped
	While 1
		'print div(17 , 25)
		k = Inkey()
		If k <> "" Then Exit While
		Sleep 1
	Wend
	Catch_Any(e)
	Show_Catch(e)
Finally

	Print " finally before leaving"
EndTry

Print "press any key to finish"
Sleep

