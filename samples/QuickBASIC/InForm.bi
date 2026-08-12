'-----------------------------------------------------------------------------------------------------------------------
' InForm-PE GUI engine for QB64-PE
' Copyright (c) 2025 Samuel Gomes
' Copyright (c) 2023 George McGinn
' Copyright (c) 2022 Fellippe Heitor
'-----------------------------------------------------------------------------------------------------------------------

$INCLUDEONCE

$LET INFORM_BI = TRUE

$SCREENHIDE
_ALLOWFULLSCREEN OFF
_CONTROLCHR OFF

'$INCLUDE:'InFormCommon.bi'

'Control types: -----------------------------------------------
__UI_Type(__UI_Type_Form).Name = "Form"

__UI_Type(__UI_Type_Frame).Name = "Frame"
__UI_Type(__UI_Type_Frame).DefaultWidth = 230
__UI_Type(__UI_Type_Frame).DefaultHeight = 150

__UI_Type(__UI_Type_Button).Name = "Button"
__UI_Type(__UI_Type_Button).DefaultWidth = 80
__UI_Type(__UI_Type_Button).DefaultHeight = 23

__UI_Type(__UI_Type_Label).Name = "Label"
__UI_Type(__UI_Type_Label).DefaultWidth = 150
__UI_Type(__UI_Type_Label).DefaultHeight = 23

__UI_Type(__UI_Type_CheckBox).Name = "CheckBox"
__UI_Type(__UI_Type_CheckBox).DefaultWidth = 150
__UI_Type(__UI_Type_CheckBox).DefaultHeight = 23
__UI_Type(__UI_Type_CheckBox).TurnsInto = __UI_Type_ToggleSwitch

__UI_Type(__UI_Type_RadioButton).Name = "RadioButton"
__UI_Type(__UI_Type_RadioButton).DefaultWidth = 150
__UI_Type(__UI_Type_RadioButton).DefaultHeight = 23

__UI_Type(__UI_Type_TextBox).Name = "TextBox"
__UI_Type(__UI_Type_TextBox).DefaultWidth = 120
__UI_Type(__UI_Type_TextBox).DefaultHeight = 23

__UI_Type(__UI_Type_ProgressBar).Name = "ProgressBar"
__UI_Type(__UI_Type_ProgressBar).DefaultWidth = 300
__UI_Type(__UI_Type_ProgressBar).DefaultHeight = 23

__UI_Type(__UI_Type_ListBox).Name = "ListBox"
__UI_Type(__UI_Type_ListBox).DefaultWidth = 200
__UI_Type(__UI_Type_ListBox).DefaultHeight = 200
__UI_Type(__UI_Type_ListBox).TurnsInto = __UI_Type_DropdownList

__UI_Type(__UI_Type_DropdownList).Name = "DropdownList"
__UI_Type(__UI_Type_DropdownList).DefaultWidth = 200
__UI_Type(__UI_Type_DropdownList).DefaultHeight = 23
__UI_Type(__UI_Type_DropdownList).TurnsInto = __UI_Type_ListBox

__UI_Type(__UI_Type_MenuBar).Name = "MenuBar"
__UI_Type(__UI_Type_MenuBar).TurnsInto = __UI_Type_ContextMenu
__UI_Type(__UI_Type_MenuBar).RestrictResize = __UI_CantResizeV

__UI_Type(__UI_Type_MenuItem).Name = "MenuItem"
__UI_Type(__UI_Type_MenuItem).RestrictResize = __UI_CantResizeV

__UI_Type(__UI_Type_MenuPanel).Name = "MenuPanel"

__UI_Type(__UI_Type_PictureBox).Name = "PictureBox"
__UI_Type(__UI_Type_PictureBox).DefaultWidth = 230
__UI_Type(__UI_Type_PictureBox).DefaultHeight = 150

__UI_Type(__UI_Type_TrackBar).Name = "TrackBar"
__UI_Type(__UI_Type_TrackBar).DefaultWidth = 300
__UI_Type(__UI_Type_TrackBar).DefaultHeight = 40
__UI_Type(__UI_Type_TrackBar).MinimumHeight = 30
__UI_Type(__UI_Type_TrackBar).RestrictResize = __UI_CantResizeV

__UI_Type(__UI_Type_ContextMenu).Name = "ContextMenu"
__UI_Type(__UI_Type_ContextMenu).TurnsInto = __UI_Type_MenuBar
__UI_Type(__UI_Type_ContextMenu).RestrictResize = __UI_CantResize
__UI_Type(__UI_Type_ContextMenu).DefaultWidth = 22
__UI_Type(__UI_Type_ContextMenu).DefaultHeight = 22

__UI_Type(__UI_Type_Font).Name = "Font"

__UI_Type(__UI_Type_ToggleSwitch).Name = "ToggleSwitch"
__UI_Type(__UI_Type_ToggleSwitch).DefaultWidth = 40
__UI_Type(__UI_Type_ToggleSwitch).DefaultHeight = 17
__UI_Type(__UI_Type_ToggleSwitch).TurnsInto = __UI_Type_CheckBox
__UI_Type(__UI_Type_ToggleSwitch).RestrictResize = __UI_CantResize
'--------------------------------------------------------------

__UI_RestoreFKeys

__UI_SubMenuDelay = .4
__UI_SnapDistance = 5
__UI_SnapDistanceFromForm = 10
__UI_MaxBorderSize = 10
__UI_Font8Offset = 5
__UI_Font16Offset = 3
__UI_ClipboardCheck$ = "InForm" + STRING$(2, 10) + "BEGIN CONTROL DATA" + CHR$(10) + STRING$(60, 45) + CHR$(10)

__UI_ThemeSetup
__UI_InternalMenus
__UI_LoadForm

__UI_Init

'Main loop
DO
    _LIMIT 1
LOOP

SYSTEM

__UI_ErrorHandler:
RESUME NEXT
