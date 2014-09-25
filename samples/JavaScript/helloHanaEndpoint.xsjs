/*
 invoke endpoint by calling in a browser:
 http://<hanaserveradress>:<xsengineport(usually 8000)>/<path>/<to>/<endpoint>/helloHanaMath.xsjslib?x=4&y=2
 e.g.:
 http://192.168.178.20:8000/geekflyer/linguist/helloHanaEndpoint.xsjs?x=4&y=2
 */

var hanaMath = $.import("./helloHanaMath.xsjslib");

var x = parseFloat($.request.parameters.get("x"));
var y = parseFloat($.request.parameters.get("y"));


var result = hanaMath.multiply(x, y);

var output = {
    title: "Hello HANA XS - do some simple math",
    input: {x: x, y: y},
    result: result
};

$.response.contentType = "application/json";
$.response.statusCode = $.net.http.OK;
$.response.setBody(JSON.stringify(output));