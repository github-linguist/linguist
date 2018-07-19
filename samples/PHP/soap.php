<?php
//load the wsdl file
$client = new SoapClient("http://example.com/soap/definition.wsdl");
//functions are now available to be called
$result = $client->soapFunc("hello");
$result = $client->anotherSoapFunc(34234);

//SOAP Information
$client = new SoapClient("http://example.com/soap/definition.wsdl");
//list of SOAP types
print_r($client->__getTypes());
//list if SOAP Functions
print_r($client->__getFunctions());
?>
