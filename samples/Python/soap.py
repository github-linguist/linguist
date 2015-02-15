from SOAPpy import WSDL
proxy = WSDL.Proxy("http://example.com/soap/wsdl")
result = proxy.soapFunc("hello")
result = proxy.anotherSoapFunc(34234)
