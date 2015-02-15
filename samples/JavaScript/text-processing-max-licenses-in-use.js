var file_system = new ActiveXObject("Scripting.FileSystemObject");
var fh = file_system.openTextFile('mlijobs.txt', 1); // 1 == open for reading
var in_use = 0, max_in_use = -1, max_in_use_at = [];

while ( ! fh.atEndOfStream) {
    var line = fh.readline();
    if (line.substr(8,3) == "OUT") {
        in_use++;
        if (in_use > max_in_use) {
            max_in_use = in_use;
            max_in_use_at = [ line.split(' ')[3] ];
        }
        else if (in_use == max_in_use)
            max_in_use_at.push( line.split(' ')[3] );
    }
    else if (line.substr(8,2) == "IN")
        in_use--;
}

fh.close();

WScript.echo("Max licenses out: " + max_in_use + "\n  " + max_in_use_at.join('\n  '));
