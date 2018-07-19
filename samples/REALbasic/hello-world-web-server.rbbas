Class HTTPSock
Inherits TCPSocket
  Event Sub DataAvailable()
    Dim headers As New InternetHeaders
    headers.AppendHeader("Content-Length", Str(LenB("Goodbye, World!")))
    headers.AppendHeader("Content-Type", "text/plain")
    headers.AppendHeader("Content-Encoding", "identity")
    headers.AppendHeader("Connection", "close")
    Dim data As String = "HTTP/1.1 200 OK" + EndOfLine.Windows + headers.Source + EndOfLine.Windows + EndOfLine.Windows + "Goodbye, World!"
    Me.Write(data)
    Me.Close
  End Sub
End Class

Class HTTPServ
Inherits ServerSocket
  Event Sub AddSocket() As TCPSocket
    Return New HTTPSock
  End Sub
End Class

Class App
Inherits Application
  Event Sub Run(Args() As String)
    Dim sock As New HTTPServ
    sock.Port = 8080
    sock.Listen()
    While True
      App.DoEvents
    Wend
  End Sub
End Class
