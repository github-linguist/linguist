Network_Port = 256
Network_Address = 127.0.0.1
NewData := false
DataReceived =
GoSub, Connection_Init
SendData(socket,"hello socket world")
return

Connection_Init:
OnExit, ExitSub

socket := ConnectToAddress(Network_Address, Network_Port)
if socket = -1
    ExitApp

Process, Exist
DetectHiddenWindows On
ScriptMainWindowId := WinExist("ahk_class AutoHotkey ahk_pid " . ErrorLevel)
DetectHiddenWindows Off

NotificationMsg = 0x5556
OnMessage(NotificationMsg, "ReceiveData")

FD_READ = 1
FD_CLOSE = 32
if DllCall("Ws2_32\WSAAsyncSelect", "UInt", socket, "UInt", ScriptMainWindowId, "UInt", NotificationMsg, "Int", FD_READ|FD_CLOSE)
{
    MsgBox % "WSAAsyncSelect() indicated Winsock error " . DllCall("Ws2_32\WSAGetLastError")
    ExitApp
}

return


ConnectToAddress(IPAddress, Port)
{
    VarSetCapacity(wsaData, 32)
    result := DllCall("Ws2_32\WSAStartup", "UShort", 0x0002, "UInt", &wsaData)
    if ErrorLevel
    {
        MsgBox WSAStartup() could not be called due to error %ErrorLevel%. Winsock 2.0 or higher is required.
        return -1
    }
    if result
    {
        MsgBox % "WSAStartup() indicated Winsock error " . DllCall("Ws2_32\WSAGetLastError")
        return -1
    }
    AF_INET = 2
    SOCK_STREAM = 1
    IPPROTO_TCP = 6
    socket := DllCall("Ws2_32\socket", "Int", AF_INET, "Int", SOCK_STREAM, "Int", IPPROTO_TCP)
    if socket = -1
    {
        MsgBox % "socket() indicated Winsock error " . DllCall("Ws2_32\WSAGetLastError")
        return -1
    }
    SizeOfSocketAddress = 16
    VarSetCapacity(SocketAddress, SizeOfSocketAddress)
    InsertInteger(2, SocketAddress, 0, AF_INET)
    InsertInteger(DllCall("Ws2_32\htons", "UShort", Port), SocketAddress, 2, 2)
    InsertInteger(DllCall("Ws2_32\inet_addr", "Str", IPAddress), SocketAddress, 4, 4)
    if DllCall("Ws2_32\connect", "UInt", socket, "UInt", &SocketAddress, "Int", SizeOfSocketAddress)
    {
        MsgBox % "connect() indicated Winsock error " . DllCall("Ws2_32\WSAGetLastError") . "?"
        return -1
    }
    return socket
}

ReceiveData(wParam, lParam)
{
    global DataReceived
    global NewData
    socket := wParam
    ReceivedDataSize = 4096
    Loop
    {
        VarSetCapacity(ReceivedData, ReceivedDataSize, 0)
        ReceivedDataLength := DllCall("Ws2_32\recv", "UInt", socket, "Str", ReceivedData, "Int", ReceivedDataSize, "Int", 0)
        if ReceivedDataLength = 0
            ExitApp
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
                MsgBox % "recv() indicated Winsock error " . WinsockError
            ExitApp
        }
        if (A_Index = 1)
            TempDataReceived =
        TempDataReceived = %TempDataReceived%%ReceivedData%
    }
    return 1
}

SendData(wParam,SendData)
{
    socket := wParam
    SendDataSize := VarSetCapacity(SendData)
    SendDataSize += 1
    sendret := DllCall("Ws2_32\send", "UInt", socket, "Str", SendData, "Int", SendDatasize, "Int", 0)
}

InsertInteger(pInteger, ByRef pDest, pOffset = 0, pSize = 4)
{
    Loop %pSize%
        DllCall("RtlFillMemory", "UInt", &pDest + pOffset + A_Index-1, "UInt", 1, "UChar", pInteger >> 8*(A_Index-1) & 0xFF)
}

ReceiveProcedure:
    if NewData
        GuiControl, , ReceivedText, %DataReceived%
    NewData := false
Return

ExitSub:
DllCall("Ws2_32\WSACleanup")
ExitApp
