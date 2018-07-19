var notes = 'NOTES.TXT';

var args = WScript.Arguments;
var fso = new ActiveXObject("Scripting.FileSystemObject");
var ForReading = 1, ForWriting = 2, ForAppending = 8;

if (args.length == 0) {
    if (fso.FileExists(notes)) {
        var f = fso.OpenTextFile(notes, ForReading);
        WScript.Echo(f.ReadAll());
        f.Close();
    }
}
else {
    var f = fso.OpenTextFile(notes, ForAppending, true);
    var d = new Date();
    f.WriteLine(d.toLocaleString());
    f.Write("\t");
    // note that WScript.Arguments is not an array, it is a "collection"
    // it does not have a join() method.
    for (var i = 0; i < args.length; i++) {
        f.Write(args(i) + " ");
    }
    f.WriteLine();
    f.Close();
}
