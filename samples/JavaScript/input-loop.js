var text_stream = WScript.StdIn;
var i = 0;

while ( ! text_stream.AtEndOfStream ) {
    var line = text_stream.ReadLine();
    // do something with line
    WScript.echo(++i + ": " + line);
}
