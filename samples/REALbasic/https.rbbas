      Dim sock As New HTTPSecureSocket
      Print(sock.Get("https://sourceforge.net", 10))  //set the timeout period to 10 seconds.
