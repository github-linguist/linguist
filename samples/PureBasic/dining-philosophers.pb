Macro Tell(Mutex, Message) ; Make a macro to easy send info back to main thread
  LockMutex(Mutex)
    LastElement(Queue())
    AddElement(Queue())
    Queue() = Message
    SignalSemaphore(Semaphore)
  UnlockMutex(Mutex)
EndMacro

;Set up a data structure to pass needed info into the threads
Structure Thread_Parameters
  Name.s
  fork1.i
  fork2.i
EndStructure

; Declare function to be used
Declare.i TryFork(n)
Declare   PutDownFork(n)
Declare   Invite(Namn.s, Fork1, Fork2)
Declare   _philosophers(*arg.Thread_Parameters)

Global Semaphore = CreateSemaphore()
Global Mutex1     = CreateMutex() ; Eg. fork 1
Global Mutex2     = CreateMutex() ; Eg. fork 2
Global Mutex3     = CreateMutex() ; Eg. fork 3
Global Mutex4     = CreateMutex() ; Eg. fork 4
Global Mutex5     = CreateMutex() ; Eg. fork 5
Global Mutex_main = CreateMutex() ; locking communication with the main thread which do all output.
Global NewList Queue.s()

If OpenConsole()
  Invite("Aristotle",1,2)  ; Get all Philosophers activated
  Invite("Kant",     2,3)
  Invite("Spinoza",  3,4)
  Invite("Marx",     4,5)
  Invite("Russell",  5,1)
  CompilerIf #PB_Compiler_OS=#PB_OS_Windows
    SetConsoleTitle_("Dining philosophers, by Jofur")   ; Using a Windows-API here, so checking before
  CompilerEndIf
  ; Wait and see if any Philosophers want to tell me anything
  Repeat
    WaitSemaphore(Semaphore)
    LockMutex(Mutex_main)
      ForEach Queue()
        PrintN( Queue() )  ; Print what the Philosopher(s) told me
        i-1
      Next Queue()
      ClearList(Queue())
    UnlockMutex(Mutex_main)
  ForEver
EndIf

Procedure TryFork(n)  ; Se is fork #n is free and if so pick it up
  Select n
    Case 1: ProcedureReturn TryLockMutex(Mutex1)
    Case 2: ProcedureReturn TryLockMutex(Mutex2)
    Case 3: ProcedureReturn TryLockMutex(Mutex3)
    Case 4: ProcedureReturn TryLockMutex(Mutex4)
    Default:ProcedureReturn TryLockMutex(Mutex5)
  EndSelect
EndProcedure

Procedure PutDownFork(n) ; put down fork #n and free it to be used by neighbors.
  Select n
    Case 1: UnlockMutex(Mutex1)
    Case 2: UnlockMutex(Mutex2)
    Case 3: UnlockMutex(Mutex3)
    Case 4: UnlockMutex(Mutex4)
    Default:UnlockMutex(Mutex5)
  EndSelect
EndProcedure

Procedure Invite(Namn.s, Fork1, Fork2)
  Protected *arg.Thread_Parameters ;create the structure containing the parameters
  Protected Thread
  *arg = AllocateMemory(SizeOf(Thread_Parameters))
  *arg\Name = Namn
  *arg\fork1 = Fork1
  *arg\fork2 = Fork2
  Thread=CreateThread(@_philosophers(), *arg) ;send the thread a pointer to our structure
  ProcedureReturn Thread
EndProcedure

Procedure _philosophers(*arg.Thread_Parameters)
  Protected Iam.s=*arg\Name, j=*arg\fork1, k=*arg\fork2
  Protected f1, f2
  ClearStructure(*arg, Thread_Parameters)
  FreeMemory(*arg)
  ;
  Repeat
    Tell(Mutex_main,Iam+": Going to the table")
    Repeat          ;Trying to get my two forks
      f1=TryFork(j)
      If f1
        f2=TryFork(k)
        If Not f2   ; I got only one fork
          PutDownFork(j)
          f1=0
        EndIf
      EndIf
      If Not f2
        Delay(Random(100))  ; Take a short breath, then try the forks in the other order
        Swap j,k
      EndIf
    Until f1 And f2
    Tell(Mutex_main,Iam+": I have fork #"+Str(j)+" & #"+Str(k)+" and I'm eating now")
    Delay(Random(1500)+15)
    Tell(Mutex_main,Iam+": release fork #"+Str(j)+" & #"+Str(k)+"")
    Delay(Random(45)+15)
    PutDownFork(j)
    PutDownFork(k)
    f1=0:f2=0
    Tell(Mutex_main,Iam+": Thinking about the nature of the universe...")
    Delay(Random(2500)+25)
  ForEver
EndProcedure
