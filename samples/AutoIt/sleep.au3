#AutoIt Version: 3.2.10.0
$sleep_me=InputBox("Sleep", "Number of seconds to sleep", "10", "", -1, -1, 0, 0)
Dim $sleep_millisec=$sleep_me*1000
MsgBox(0,"Sleep","Sleeping for "&$sleep_me&" sec")
sleep ($sleep_millisec)
MsgBox(0,"Awake","... Awaking")
