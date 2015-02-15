Structure IO_block
  ThreadID.i
  StartSeamaphore.i
  Value.q
  MinimumFactor.i
  List Factors.i()
EndStructure
;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

Declare Factorize(*IO.IO_block)
Declare main()
;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

Main()
End
;\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

Procedure Main()
  Protected AvailableCpu, MainSemaphore
  Protected i, j, qData.q, Title$, Message$
  NewList T.IO_block()
  ;
  AvailableCpu = Val(GetEnvironmentVariable("NUMBER_OF_PROCESSORS"))
  If AvailableCpu<1: AvailableCpu=1: EndIf
  MainSemaphore = CreateSemaphore(AvailableCpu)
  ;
  Restore Start_of_data
  For i=1 To (?end_of_data-?Start_of_data) / SizeOf(Quad)
    ; Start all threads at ones, they will then be let to
    ; self-oganize according to the availiable Cores.
    AddElement(T())
    Read.q  qData
    T()\Value = qData
    T()\StartSeamaphore = MainSemaphore
    T()\ThreadID = CreateThread(@Factorize(), @T())
  Next
  ;
  ForEach T()
    ; Wait for all threads to complete their work and
    ; find the smallest factor from eact task.
    WaitThread(T()\ThreadID)
  Next
  ;
  i = OffsetOf(IO_block\MinimumFactor)
  SortStructuredList(T(), #PB_Sort_Integer, i, #PB_Sort_Descending)
  FirstElement(T())
  Title$="Info"
  Message$="Number "+Str(T()\Value)+" has largest minimal factor:"+#CRLF$
  ForEach T()\Factors()
    Message$ + Str(T()\Factors())+" "
  Next
  MessageRequester(Title$, Message$)
EndProcedure

ProcedureDLL Factorize(*IO.IO_block) ; Fill list Factors() with the factor parts of Number
  ;Based on http://rosettacode.org/wiki/Prime_decomposition#PureBasic
  With *IO
    Protected Value.q=\Value
    WaitSemaphore(\StartSeamaphore)
    Protected I = 3
    ClearList(\Factors())
    While Value % 2 = 0
      AddElement(\Factors())
      \Factors() = 2
      Value / 2
    Wend
    Protected Max = Value
    While I <= Max And Value > 1
      While Value % I = 0
        AddElement(\Factors())
        \Factors() = I
        Value / I
      Wend
      I + 2
    Wend
    SortList(\Factors(), #PB_Sort_Ascending)
    FirstElement(\Factors())
    \MinimumFactor=\Factors()
    SignalSemaphore(\StartSeamaphore)
  EndWith ;*IO
EndProcedure

DataSection
  Start_of_data: ; Same numbers as Ada
  Data.q  12757923, 12878611, 12757923, 15808973, 15780709, 197622519
  end_of_data:
EndDataSection
