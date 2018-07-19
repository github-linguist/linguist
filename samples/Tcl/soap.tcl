package require WS::Client

# Grok the service, and generate stubs
::WS::Client::GetAndParseWsdl http://example.com/soap/wsdl
::WS::Client::CreateStubs ExampleService   ;# Assume that's the service name...

# Do the calls
set result1 [ExampleService::soapFunc "hello"]
set result2 [ExampleService::anotherSoapFunc 34234]
