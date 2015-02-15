var fso = new ActiveXObject("Scripting.FileSystemObject");
var dir = fso.GetFolder('test_folder');

function walkDirectory(dir, re_pattern) {
    WScript.Echo("Files in " + dir.name + " matching '" + re_pattern +"':");
    walkDirectoryFilter(dir.Files, re_pattern);

    WScript.Echo("Folders in " + dir.name + " matching '" + re_pattern +"':");
    walkDirectoryFilter(dir.Subfolders, re_pattern);
}

function walkDirectoryFilter(items, re_pattern) {
    var e = new Enumerator(items);
    while (! e.atEnd()) {
        var item = e.item();
        if (item.name.match(re_pattern))
            WScript.Echo(item.name);
        e.moveNext();
    }
}

walkDirectory(dir, '\\.txt$');
