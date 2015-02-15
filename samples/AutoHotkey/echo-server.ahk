#SingleInstance Force
Network_Port = 12321
Network_Address = 127.0.0.1

NewData := false
DataReceived =
Gosub Connection_Init
return

Connection_Init:
OnExit, ExitSub
socket := PrepareForIncomingConnection(Network_Address, Network_Port)
if socket = -1
    ExitApp

Process, Exist
DetectHiddenWindows On
ScriptMainWindowId := WinExist("ahk_class AutoHotkey ahk_pid " . ErrorLevel)
DetectHiddenWindows Off

NotificationMsg = 0x5555
OnMessage(NotificationMsg, "ReceiveData")

ExitMsg = 0x6666
OnMessage(ExitMsg, "ExitData")

FD_READ = 1
FD_CLOSE = 32
FD_CONNECT = 20

if DllCall("Ws2_32\WSAAsyncSelect", "UInt", socket,
      "UInt", ScriptMainWindowId, "UInt", ExitMsg, "Int", FD_CLOSE)
{
    msgbox, closed
}

if DllCall("Ws2_32\WSAAsyncSelect", "UInt", socket,
        "UInt", ScriptMainWindowId, "UInt", NotificationMsg, "Int",
        FD_READ|FD_CONNECT)
{
    MsgBox % "WSAAsyncSelect() indicated Winsock error "
          . DllCall("Ws2_32\WSAGetLastError")
    DllCall("Ws2_32\WSAAsyncSelect", "UInt", socket,
          "UInt", ScriptMainWindowId, "UInt", ExitMsg, "Int", FD_CLOSE)
    ExitApp
}

SetTimer, NewConnectionCheck, 500
return

PrepareForIncomingConnection(IPAddress, Port)
{
    VarSetCapacity(wsaData, 32)
    result := DllCall("Ws2_32\WSAStartup", "UShort", 0x0002, "UInt", &wsaData)
    if ErrorLevel
    {
        MsgBox % "WSAStartup() could not be called due to error %ErrorLevel%. "
                . "Winsock 2.0 or higher is required."
        return -1
    }
    if result
    {
        MsgBox % "WSAStartup() indicated Winsock error "
                . DllCall("Ws2_32\WSAGetLastError")
        return -1
    }
    AF_INET = 2
    SOCK_STREAM = 1
    IPPROTO_TCP = 6
    socket := DllCall("Ws2_32\socket", "Int", AF_INET,
          "Int", SOCK_STREAM, "Int", IPPROTO_TCP)
    if socket = -1
    {
        MsgBox % "socket() indicated Winsock error "
                . DllCall("Ws2_32\WSAGetLastError")
        return -1
    }
    SizeOfSocketAddress = 16
    VarSetCapacity(SocketAddress, SizeOfSocketAddress)
    InsertInteger(2, SocketAddress, 0, AF_INET)
    InsertInteger(DllCall("Ws2_32\htons", "UShort", Port), SocketAddress, 2, 2)
    InsertInteger(DllCall("Ws2_32\inet_addr", "Str", IPAddress),
            SocketAddress, 4, 4)
    if DllCall("Ws2_32\bind", "UInt", socket,
            "UInt", &SocketAddress, "Int", SizeOfSocketAddress)
    {
        MsgBox % "bind() indicated Winsock error "
                . DllCall("Ws2_32\WSAGetLastError") . "?"
        return -1
    }
    if DllCall("Ws2_32\listen", "UInt", socket, "UInt", "SOMAXCONN")
    {
        MsgBox % "LISTEN() indicated Winsock error "
                . DllCall("Ws2_32\WSAGetLastError") . "?"
        return -1
    }
    return socket
}

ReceiveData(wParam, lParam)
{
    global DataReceived
    global NewData
    global mydata
    global ConnectionList
    socket := wParam
    ReceivedDataSize = 4096
    Loop
    {
        VarSetCapacity(ReceivedData, ReceivedDataSize, 0)
        ReceivedDataLength := DllCall("Ws2_32\recv", "UInt",
              socket, "Str", ReceivedData, "Int", ReceivedDataSize, "Int", 0)
	if ReceivedDataLength = 0
        {
            StringReplace, ConnectionList, ConnectionList, %socket%`n
            DllCall("Ws2_32\closesocket", "UInt", socket)
        }
        if ReceivedDataLength = -1
        {
            WinsockError := DllCall("Ws2_32\WSAGetLastError")
            if WinsockError = 10035
            {
                DataReceived = %TempDataReceived%
                NewData := true
                return 1
            }
            if WinsockError <> 10054
            {
                MsgBox % "recv() indicated Winsock error " . WinsockError
                StringReplace, ConnectionList, ConnectionList, %socket%`n
                DllCall("Ws2_32\closesocket", "UInt", socket)
            }
        }
        mydata := ReceivedData
        gosub myreceive
	if (A_Index = 1)
            TempDataReceived =
                TempDataReceived = %TempDataReceived%%ReceivedData%
    }
    return 1
}

ExitData(wParam, lParam)
{
    global ConnectionList
    socket := wParam
    ReceivedDataSize = 16
    VarSetCapacity(ReceivedData, ReceivedDataSize, 0)
    ReceivedDataLength := DllCall("Ws2_32\recv", "UInt", socket,
          "Str", ReceivedData, "Int", ReceivedDataSize, "Int", 0)
    StringReplace, ConnectionList, ConnectionList, %socket%`n
    DllCall("Ws2_32\closesocket", "UInt", socket)
    return 1
}

SendData(wParam,SendData)
{
    SendDataSize := VarSetCapacity(SendData)
    SendDataSize += 1
    Loop, parse, wParam, `n
    {
        If A_LoopField =
           Continue
        socket := A_LoopField
        sendret := DllCall("Ws2_32\send", "UInt", socket,
              "Str", SendData, "Int", SendDatasize, "Int", 0)
    }
}


InsertInteger(pInteger, ByRef pDest, pOffset = 0, pSize = 4)
{
    Loop %pSize%
        DllCall("RtlFillMemory", "UInt", &pDest + pOffset + A_Index-1,
                "UInt", 1, "UChar", pInteger >> 8*(A_Index-1) & 0xFF)
}

NewConnectionCheck:
ConnectionCheck := DllCall("Ws2_32\accept", "UInt", socket,
      "UInt", &SocketAddress, "Int", SizeOfSocketAddress)
if ConnectionCheck > 1
    ConnectionList = %ConnectionList%%ConnectionCheck%`n
Return

SendProcedure:
If ConnectionList <>
{
    SendText = %A_Hour%:%A_Min%:%A_Sec%
    SendData(ConnectionList,SendText)
}
Return

myreceive:
 TrayTip, server, %mydata%, ,16
  return

GuiClose:
ExitSub:
DllCall("Ws2_32\WSACleanup")
ExitApp
