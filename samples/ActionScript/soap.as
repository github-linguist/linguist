import mx.rpc.soap.WebService;
import mx.rpc.events.ResultEvent;
var ws:WebService = new WebService();
ws.wsdl = 'http://example.com/soap/wsdl';
ws.soapFunc.addEventListener("result",soapFunc_Result);
ws.anotherSoapFunc.addEventListener("result",anotherSoapFunc_Result);
ws.loadWSDL();
ws.soapFunc();
ws.anotherSoapFunc();
//  method invocation callback handlers
private function soapFunc_Result(event:ResultEvent):void {
  // do something
}
private function anotherSoapFunc_Result(event:ResultEvent):void {
  // do another something
}
