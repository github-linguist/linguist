objConn := CreateObject("ADODB.Connection")
objCmd := CreateObject("ADODB.Command")
objConn.Provider := "ADsDSOObject"
objConn.Open()
