<cfset client = createObject("webservice","http://example.com/soap/wsdl")>
<cfset result = client.soapFunc("hello")>
<cfset result = client.anotherSoapFunc(34234)>
