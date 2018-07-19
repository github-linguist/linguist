If InitNetwork() = 0
  MessageRequester("Error", "Can't initialize the network !")
  End
EndIf

Port = 8080

If CreateNetworkServer(0, Port)
  Repeat
    Delay(1)
    SEvent = NetworkServerEvent()
    If SEvent
      ClientID = EventClient()
      Select SEvent
        Case #PB_NetworkEvent_Data
          SendNetworkData(ClientID,@"Goodbye, World!",Len("Goodbye, World!"))
          CloseNetworkConnection(ClientID)
      EndSelect
    EndIf
  ForEver
Else
  MessageRequester("Error", "Can't create the server (port in use ?).")
EndIf
