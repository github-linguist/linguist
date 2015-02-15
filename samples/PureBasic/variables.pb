; Variables are initialized when they appear in sourcecode with default value of 0 and type int
Debug a
; or value "" for a string, they are not case sensitive
Debug b$
; This initializes a double precision float, if type is following the dot
Debug c.d
; They can be initialized with define (double precision float, string, integer)
Define d.d = 3.5, e$ = "Test", f.i = a + 2
; Define can have a default type (all bytes except j which is long):
Define.b g, h, j.l
; Define without following variables sets default type. In this case to single precision float
Define.f
; So this will be an single precision float and no integer
Debug k
; EnableExplicit forces declaration of used variables with define
EnableExplicit
; Will throw an error because L isn't initialized
Debug L
DisableExplicit
; Global Variables are available in Procedures and Threads too
Global M = 3, N = 2
Procedure Dummy(parameter1, parameter2 = 20)
  ; Parameter contain values which where used when calling the function,
  ; their types have to be specified in the above Procedure header.
  ; The last ones can have default values which get applied if this parameter is not given.

  ; Variables in Procedures are separate from those outside,
  ; so d can be initialized again with another type
  ; which would otherwise lead to an error
  d.i
  ; Protected makes a variable local even if another one with same name is declared as global (see above)
  Protected M = 2
  ; Shares a variable with main program like it was declared by global
  Shared a
  ; prevents a variable to be initialized with default value again when procedure is called a second time,
  ; could be used for example as a counter, which contains the number of times a function was called
  Static a
  ; N here also would have a value of 2, while for example
  ; f would, when named, initialize a new variable, and so have a value of 0
EndProcedure
; finally there are constants which are prefixed by an #:
#Test = 1
; Their value cannot be changed while program is running
#String_Constant = "blubb"
; In constrast  to variables, a constant has no types except an (optional) $ sign to mark  it as string constant
#Float_Constant = 2.3
; Maps, LinkedLists , Arrays and Structures are not handled here, because they are no elemental variables
