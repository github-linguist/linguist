Class EchoSocket
Inherits TCPSocket
  Sub DataAvailable()
    If Instr(Me.LookAhead, EndofLine.Windows) > 0 Then
      Dim data As String = Me.ReadAll
      Dim lines() As String = Split(data, EndofLine.Windows)
      For i As Integer = 0 To Ubound(lines)
        Me.Write(lines(i) + EndOfLine.Windows)
        Print(lines(i))
      Next
    End If
  End Sub
End Class

Class EchoServer
Inherits ServerSocket
  Function AddSocket() As TCPSocket
    Return New EchoSocket
  End Function
End Class

Class App
Inherits ConsoleApplication
  Function Run(args() As String) As Integer
    Listener = New EchoServer
    Listener.Port = 12321
    Listener.Listen()
    While True
      DoEvents() 'pump the event loop
    Wend
  End Function
  Private Listener As EchoListener
End Class
