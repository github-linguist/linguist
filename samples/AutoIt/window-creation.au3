GUICreate("Test")
GUISetState(@SW_SHOW)

Do
	Switch GUIGetMsg()
		Case -3 ; $GUI_EVENT_CLOSE
			Exit
	EndSwitch
Until False
