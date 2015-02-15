Structure mRule
  pattern.s
  replacement.s
  isTerminal.i
EndStructure

Procedure parseRule(text.s, List rules.mRule())
  #tab = 9: #space = 32: #whiteSpace$ = Chr(#space) + Chr(#tab)
  Protected tLen, cPtr, nChar.c, pEnd, pLast, pattern.s

  cPtr = 1
  If FindString(#whiteSpace$, Left(text, cPtr), 1): ProcedureReturn 0: EndIf ;parse error
  If Left(text, cPtr) = "#": ProcedureReturn 2: EndIf ;comment skipped

  tLen = Len(text)
  Repeat
    cPtr + 1
    If cPtr > tLen: ProcedureReturn 0: EndIf ;parse error
    nChar = Asc(Mid(text, cPtr, 1))
    Select nChar
      Case #space, #tab
        Select pEnd
          Case 0 To 2
            pEnd = 1
            pLast = cPtr - 1
          Case 3
            pattern = Left(text, pLast)
        EndSelect
      Case '-'
        If pEnd = 1: pEnd = 2: EndIf
      Case '>'
        If pEnd = 2: pEnd = 3: EndIf
    EndSelect
  Until pattern <> ""

  Repeat
    cPtr + 1
  Until Not FindString(#whiteSpace$, Mid(text, cPtr, 1), 1)
  Protected isTerminal
  If Mid(text, cPtr, 1) = "."
    isTerminal = #True: cPtr + 1
  EndIf

  LastElement(rules()): AddElement(rules())
  rules()\pattern = pattern
  rules()\replacement = Right(text, tLen - cPtr + 1)
  rules()\isTerminal = isTerminal
  ProcedureReturn 1 ;processed rule
EndProcedure

Procedure.s interpretMarkov(text.s, List rules.mRule())
  Repeat
    madeReplacement = #False
    ForEach rules()
      If FindString(text, rules()\pattern, 1)
        text = ReplaceString(text, rules()\pattern, rules()\replacement)
        madeReplacement = #True: isFinished = rules()\isTerminal
        Break
      EndIf
    Next
  Until Not madeReplacement Or isFinished
  ProcedureReturn text
EndProcedure

Procedure addRule(text.s, List rules.mRule())
  Protected result = parseRule(text, rules())
  Select result
    Case 0: AddGadgetItem(7, -1, "Invalid rule: " + #DQUOTE$ + text + #DQUOTE$)
    Case 1: AddGadgetItem(7, -1, "Added: " + #DQUOTE$ + text + #DQUOTE$)
    Case 2: AddGadgetItem(7, -1, "Comment: " + #DQUOTE$ + text + #DQUOTE$)
  EndSelect
EndProcedure

OpenWindow(0, 0, 0, 350, 300, "Markov Algorithm Interpreter", #PB_Window_SystemMenu)
ButtonGadget(0, 45, 10, 75, 20, "Load Ruleset")
ButtonGadget(1, 163, 10, 65, 20, "Add Rule")
ButtonGadget(2, 280, 10, 65, 20, "Interpret")
TextGadget(3, 5, 40, 30, 20, "Input:")
StringGadget(4, 45, 40, 300, 20,"")
TextGadget(5, 5, 100, 35, 20, "Output:")
ButtonGadget(6, 160, 70, 70, 20, "Clear Output")
EditorGadget(7, 45, 100, 300, 195, #PB_Editor_ReadOnly)

NewList rules.mRule()
Define event, isDone, text.s, result, file.s
Repeat
  event = WaitWindowEvent()
  Select event
    Case #PB_Event_Gadget
      Select EventGadget()
        Case 0
          Define file.s, rule.s
          file = OpenFileRequester("Select rule set", "*.txt", "Text (*.txt)|*.txt", 0)
          If file
            ClearList(rules())
            ReadFile(0, file)
            While Not(Eof(0))
              addRule(ReadString(0), rules())
            Wend
            AddGadgetItem(7, -1, "Loaded " +  Str(ListSize(rules())) + " rules."): AddGadgetItem(7, -1, "")
          EndIf
        Case 1
          addRule(GetGadgetText(4), rules())
        Case 2
          text = GetGadgetText(4): AddGadgetItem(7, -1, "Interpret: " + #DQUOTE$ + text + #DQUOTE$)
          AddGadgetItem(7, -1, "Result: " + #DQUOTE$ + interpretMarkov(text, rules()) + #DQUOTE$): AddGadgetItem(7, -1, "")
        Case 6
          ClearGadgetItems(7)
      EndSelect
    Case #PB_Event_CloseWindow
      isDone = #True
  EndSelect
Until isDone
