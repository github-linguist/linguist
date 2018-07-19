use SOAP::Lite;

print SOAP::Lite
  -> service('http://example.com/soap/wsdl')
  -> soapFunc("hello");
print SOAP::Lite
  -> service('http://example.com/soap/wsdl')
  -> anotherSoapFunc(34234);
