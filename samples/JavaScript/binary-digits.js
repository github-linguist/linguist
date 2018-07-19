function toBinary(number) {
  return new Number(number).toString(2);
}
var demoValues = [5, 50, 9000];
for (var i=0; i<demoValues.length; ++i) {
  print(toBinary(demoValues[i])); // alert() in a browser, wscript.echo in WSH, etc.
}
