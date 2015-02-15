;---------------------------------------------------------------------------
; Markov Algorithm.ahk
; by wolf_II
;---------------------------------------------------------------------------
; interpreter for a Markov Algorithm
;---------------------------------------------------------------------------



;---------------------------------------------------------------------------
AutoExecute: ; auto-execute section of the script
;---------------------------------------------------------------------------
    #SingleInstance, Force          ; only one instance allowed
    #NoEnv                          ; don't check empty variables
    StartupDir := A_WorkingDir      ; remember startup directory
    SetWorkingDir, %A_ScriptDir%    ; change directoy
    StringCaseSense, On             ; case sensitive comparisons
    ;-----------------------------------------------------------------------
    AppName := "Markov Algorithm"
    Gosub, GuiCreate
    Gui, Show,, %AppName%

Return



;---------------------------------------------------------------------------
GuiCreate: ; create the GUI
;---------------------------------------------------------------------------
    ; GUI options
    Gui, -MinimizeBox
    Gui, Add, Edit, y0 h0 ; catch the focus

    ; Ruleset
    Gui, Add, GroupBox, w445 h145 Section, Ruleset
    Gui, Add, Edit, xs+15 ys+20 w300 r8 vRuleset
    Gui, Add, Button, x+15 w100, Load Ruleset
    Gui, Add, Button, wp, Save Ruleset
    Gui, Add, Button, w30, 1
    Gui, Add, Button, x+5 wp, 2
    Gui, Add, Button, x+5 wp, 3
    Gui, Add, Button, xs+330 y+6 wp, 4
    Gui, Add, Button, x+5 wp, 5

    ; String
    Gui, Add, GroupBox, xs w445 h75 Section, String
    Gui, Add, Edit, xs+15 ys+20 w300 vString
    Gui, Add, Button, x+15 w100, Apply Ruleset
    Gui, Add, Button, xp wp Hidden, Stop
    Gui, Add, CheckBox, xs+15 yp+30 vSingleStepping, Single Stepping?

    ; Output
    Gui, Add, GroupBox, xs w445 h235 Section, Output
    Gui, Add, Edit, xs+15 ys+20 w415 r15 ReadOnly vOutput HwndhOut

Return



;---------------------------------------------------------------------------
GuiClose:
;---------------------------------------------------------------------------
    ExitApp

Return



;---------------------------------------------------------------------------
ButtonLoadRuleset: ; load ruleset from file
;---------------------------------------------------------------------------
    Gui, +OwnDialogs
    FileSelectFile, RulesetFile,,, Load Ruleset, *.markov
    If Not SubStr(RulesetFile, -6) = ".markov"
        RulesetFile .= ".markov"
    If FileExist(RulesetFile) {
        FileRead, Ruleset, %RulesetFile%
        GuiControl,, Ruleset, %Ruleset%
    } Else
        MsgBox, 16, Error - %AppName%, File not found:`n`n"%RulesetFile%"

Return



;---------------------------------------------------------------------------
ButtonSaveRuleset: ; save ruleset to file
;---------------------------------------------------------------------------
    Gui, +OwnDialogs
    Gui, Submit, NoHide
    FileSelectFile, RulesetFile, S16,, Save Ruleset, *.markov
    If Not SubStr(RulesetFile, -6) = ".markov"
        RulesetFile .= ".markov"
    FileDelete, %RulesetFile%
    FileAppend, %Ruleset%, %RulesetFile%
    Gui, Show

Return


_
;---------------------------------------------------------------------------
Button1: ; http://rosettacode.org/wiki/Execute_a_Markov_algorithm#Ruleset_1
;---------------------------------------------------------------------------
    GuiControl,, Output ; clear output
    GuiControl,, String, I bought a B of As from T S.
    GuiControl,, Ruleset,
    (LTrim
    # This rules file is extracted from Wikipedia:
    # http://en.wikipedia.org/wiki/Markov_Algorithm
    A -> apple
    B -> bag
    S -> shop
    T -> the
    the shop -> my brother
    a never used -> .terminating rule
    )

Return



;---------------------------------------------------------------------------
Button2: ; http://rosettacode.org/wiki/Execute_a_Markov_algorithm#Ruleset_2
;---------------------------------------------------------------------------
    GuiControl,, Output ; clear output
    GuiControl,, String, I bought a B of As from T S.
    GuiControl,, Ruleset,
    (LTrim
    # Slightly modified from the rules on Wikipedia
    A -> apple
    B -> bag
    S -> .shop
    T -> the
    the shop -> my brother
    a never used -> .terminating rule
    )

Return



;---------------------------------------------------------------------------
Button3: ; http://rosettacode.org/wiki/Execute_a_Markov_algorithm#Ruleset_3
;---------------------------------------------------------------------------
    GuiControl,, Output ; clear output
    GuiControl,, String, I bought a B of As W my Bgage from T S.
    GuiControl,, Ruleset,
    (LTrim
    # BNF Syntax testing rules
    A -> apple
    WWWW -> with
    Bgage -> ->.*
    B -> bag
    ->.* -> money
    W -> WW
    S -> .shop
    T -> the
    the shop -> my brother
    a never used -> .terminating rule
    )

Return



;---------------------------------------------------------------------------
Button4: ; http://rosettacode.org/wiki/Execute_a_Markov_algorithm#Ruleset_4
;---------------------------------------------------------------------------
    GuiControl,, Output ; clear output
    GuiControl,, String, _1111*11111_
    GuiControl,, Ruleset,
    (LTrim
    ### Unary Multiplication Engine, for testing Markov Algorithm implementations
    ### By Donal Fellows.
    # Unary addition engine
    _+1 -> _1+
    1+1 -> 11+
    # Pass for converting from the splitting of multiplication into ordinary
    # addition
    1! -> !1
    ,! -> !+
    _! -> _
    # Unary multiplication by duplicating left side, right side times
    1*1 -> x,@y
    1x -> xX
    X, -> 1,1
    X1 -> 1X
    _x -> _X
    ,x -> ,X
    y1 -> 1y
    y_ -> _
    # Next phase of applying
    1@1 -> x,@y
    1@_ -> @_
    ,@_ -> !_
    ++ -> +
    # Termination cleanup for addition
    _1 -> 1
    1+_ -> 1
    _+_ ->
    )

Return



;---------------------------------------------------------------------------
Button5: ; http://rosettacode.org/wiki/Execute_a_Markov_algorithm#Ruleset_5
;---------------------------------------------------------------------------
    GuiControl,, Output ; clear output
    GuiControl,, String, 000000A000000
    GuiControl,, Ruleset,
    (LTrim
    # Turing machine: three-state busy beaver
    #
    # state A, symbol 0 => write 1, move right, new state B
    A0 -> 1B
    # state A, symbol 1 => write 1, move left, new state C
    0A1 -> C01
    1A1 -> C11
    # state B, symbol 0 => write 1, move left, new state A
    0B0 -> A01
    1B0 -> A11
    # state B, symbol 1 => write 1, move right, new state B
    B1 -> 1B
    # state C, symbol 0 => write 1, move left, new state B
    0C0 -> B01
    1C0 -> B11
    # state C, symbol 1 => write 1, move left, halt
    0C1 -> H01
    1C1 -> H11
    )

Return



;---------------------------------------------------------------------------
ButtonApplyRuleset: ; flow control for Algorithm
;---------------------------------------------------------------------------
    ; prepare
    Gui, Submit, NoHide
    GuiControl,, Output ; clear
    Controls(False) ; disable
    Count := 0
    Subst := True
    Stop  := False

    ; keep substituting for as long as necessary
    While, Subst {
        Subst := False ; reset control variable
        IfEqual, Stop, 1, Break
        Gosub, Algorithm
    }

    ; clean up
    Output("Substitution count: " Count)
    Controls(True) ; re-enable

Return



;---------------------------------------------------------------------------
ButtonStop: ; this button is initially hidden
;---------------------------------------------------------------------------
    Stop := True

Return



;---------------------------------------------------------------------------
Algorithm: ; http://rosettacode.org/wiki/Execute_a_Markov_algorithm
;---------------------------------------------------------------------------
    ; Parse the ruleset and apply each rule to the string. Whenever a rule
    ; has changed the string goto first rule. Continue until a encountering
    ; a terminating rule, or until no further changes to the strings are
    ; made.
    ;-----------------------------------------------------------------------
    Loop, Parse, Ruleset, `n, `r ; always start from the beginning
    {
        ; check for comment
        If SubStr(A_LoopField, 1, 1) = "#"
            Continue ; get next line

        ; split a rule into $Search, $Terminator and $Replace
        LookFor := "(?P<Search>.+) -> (?P<Terminator>\.?)(?P<Replace>.+)"
        RegExMatch(A_LoopField, LookFor, $)

        ; single stepping through possible substitutions
        If SingleStepping
            MsgBox,, %AppName%, % ""
            . "Rule = """ A_LoopField """`n`n"
            . "Search`t= """ $Search """`n"
            . "Replace`t= """ $Replace """`n"
            . "Termintor`t= """ ($Terminator ? "True" : "False") """`n"

        ; try to substitute
        StringReplace, String, String, %$Search%, %$Replace%, UseErrorLevel

        ; any success?
        If ErrorLevel {     ; yes, substitution done
            Count++         ; keep count
            Subst := True   ; set control variable
            Output(String)  ; write new string to output
        }

        ; terminate?
        If $Terminator {    ; yes, terminate
            Stop := True    ; set control variable
            Break           ; back to flow control
        }

        ; we are not yet terminated ...
        If Subst            ; but we just did a substitution
            Break           ; back to flow control
    }

Return



;---------------------------------------------------------------------------
Controls(Bool) { ; [en|dis]able controls
;---------------------------------------------------------------------------
    Enable  := Bool ? "+" : "-"
    Disable := Bool ? "-" : "+"
    Loop, 2
        GuiControl, %Disable%ReadOnly, % "Edit" A_Index + 1
    Loop, 7
        GuiControl, %Disable%Disabled, % "Button" A_Index + 1
    GuiControl, %Disable%Disabled, Edit4
    GuiControl, %Disable%Hidden, Button10
    GuiControl, %Enable%Hidden, Button11
    GuiControl, %Disable%Disabled, Button12
}



;---------------------------------------------------------------------------
Output(Text) { ; append text to output
;---------------------------------------------------------------------------
    static EM_REPLACESEL = 0xC2
    global hOut
    Sleep, 100
    Text .= "`r`n"
    SendMessage, EM_REPLACESEL,, &Text,, ahk_id %hOut%
}



;---------- end of file ----------------------------------------------------
