NewMap RecData.s()
OpenWindow(0, 100, 200, 200, 100, "Echo Server", #PB_Window_SystemMenu | #PB_Window_MinimizeGadget )
InitNetwork()
CreateNetworkServer(1, 12321)

Repeat
   Event = NetworkServerEvent()
   ClientID = EventClient()

   If Event = #PB_NetworkEvent_Connect    ; When a new client has been connected...
      AddMapElement(RecData(), Str(ClientID))

   ElseIf Event = #PB_NetworkEvent_Data
      *Buffer = AllocateMemory(20000)
      count = ReceiveNetworkData(ClientID, *Buffer, 20000)
      For i = 1 To count
         RecData(Str(ClientID)) + Mid( PeekS(*Buffer, count), i , 1)
         If Right( RecData(Str(ClientID)), 2) = #CRLF$
            SendNetworkString (ClientID, RecData(Str(ClientID)))
            Debug  IPString(GetClientIP(ClientID)) + ":" + Str(GetClientPort(ClientID)) + "  " + RecData(Str(ClientID))
            RecData(Str(ClientID)) = ""
         EndIf
      Next
      FreeMemory(*Buffer)

   ElseIf Event = #PB_NetworkEvent_Disconnect  ; When a client has closed the connection...
      DeleteMapElement(RecData(), Str(ClientID))
   EndIf

   Event = WaitWindowEvent(10)
Until Event = #PB_Event_CloseWindow
