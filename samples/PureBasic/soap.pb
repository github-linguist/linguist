XIncludeFile "COMatePLUS.pbi"
Define.COMateObject soapObject = COMate_CreateObject("MSSOAP.SoapClient")
soapObject\Invoke("MSSoapInit('http://example.com/soap/wsdl')")
result = soapObject\Invoke("soapFunc('hello')")
result2 = soapObject\Invoke("anotherSoapFunc(34234)")
