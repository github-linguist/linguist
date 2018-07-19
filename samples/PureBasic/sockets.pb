InitNetwork()
ConnectionID = OpenNetworkConnection("localhost", 256)
SendNetworkString(ConnectionID, "hello socket world")
CloseNetworkConnection(ConnectionID)
