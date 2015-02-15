var shell = new ActiveXObject("WScript.Shell");
var env = shell.Environment("PROCESS");
WScript.echo('SYSTEMROOT=' + env.item('SYSTEMROOT'));
