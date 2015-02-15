Prototype.d ValueFunction(f.d, t.d)

Class IntegralClass
  Time0.i
  Mutex.i
  S.d
  Freq.d
  Thread.i
  Quit.i
  *func.ValueFunction

  Protect Method Sampler()
    Repeat
      Delay(1)
      If This\func And This\Mutex
        LockMutex(This\Mutex)
        This\S + This\func(This\Freq, ElapsedMilliseconds()-This\Time0)
        UnlockMutex(This\Mutex)
      EndIf
    Until This\Quit
  EndMethod

  BeginPublic
    Method Input(*func.ValueFunction)
      LockMutex(This\Mutex)
      This\func = *func
      UnlockMutex(This\Mutex)
    EndMethod

    Method.d Output()
      Protected Result.d
      LockMutex(This\Mutex)
      Result = This\S
      UnlockMutex(This\Mutex)
      MethodReturn Result
    EndMethod

    Method Init(F.d, *f)
      This\Freq   = F
      This\func   = *f
      This\Mutex  = CreateMutex()
      This\Time0  = ElapsedMilliseconds()
      This\Thread = CreateThread(This\Sampler, This)
      ThreadPriority(This\Thread, 10)
    EndMethod

    Method Release()
      This\Quit = #True
      WaitThread(This\Thread)
    EndMethod
  EndPublic

EndClass

;- Procedures for generating values
Procedure.d n(f.d, t.d)
  ; Returns nothing
EndProcedure

Procedure.d f(f.d, t.d)
  ; Returns the function of this task
  ProcedureReturn Sin(2*#PI*f*t)
EndProcedure

;- Test Code
*a.IntegralClass = NewObject.IntegralClass(0.5, @n()) ; Create the AO
*a\Input(@f()) ; Start sampling function f()
Delay(2000)    ; Delay 2 sec
*a\Input(@n()) ; Change to sampling 'nothing'
Delay( 500)    ; Wait 1/2 sec
MessageRequester("Info", StrD(*a\Output()))           ; Present the result
*a= FreeObject
