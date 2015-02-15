require 'soap/wsdlDriver'

wsdl = SOAP::WSDLDriverFactory.new("http://example.com/soap/wsdl")
soap = wsdl.create_rpc_driver

response1 = soap.soapFunc(:elementName => "value")
puts response1.soapFuncReturn

response2 = soap.anotherSoapFunc(:aNumber => 42)
puts response2.anotherSoapFuncReturn
