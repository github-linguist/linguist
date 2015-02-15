WS_Initialize()
    WS_Exec("Set client = CreateObject(""MSSOAP.SoapClient"")")
    WS_Exec("client.MSSoapInit ""http://example.com/soap/wsdl""")
    callhello = client.soapFunc("hello")
    callanother = client.anotherSoapFunc(34234)

    WS_Eval(result, callhello)
    WS_Eval(result2, callanother)
    Msgbox % result . "`n" . result2
    WS_Uninitialize()
#Include ws4ahk.ahk  ; http://www.autohotkey.net/~easycom/ws4ahk_public_api.html
