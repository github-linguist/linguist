var network = new ActiveXObject('WScript.Network');
var hostname = network.computerName;
WScript.echo(hostname);
