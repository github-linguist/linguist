var fso = new ActiveXObject("Scripting.FileSystemObject");

function walkDirectoryTree(folder, folder_name, re_pattern) {
    WScript.Echo("Files in " + folder_name + " matching '" + re_pattern + "':");
    walkDirectoryFilter(folder.files, re_pattern);

    var subfolders = folder.SubFolders;
    WScript.Echo("Folders in " + folder_name + " matching '" + re_pattern + "':");
    walkDirectoryFilter(subfolders, re_pattern);

    WScript.Echo();
    var en = new Enumerator(subfolders);
    while (! en.atEnd()) {
        var subfolder = en.item();
        walkDirectoryTree(subfolder, folder_name + "/" + subfolder.name, re_pattern);
        en.moveNext();
    }
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

walkDirectoryTree(dir, dir.name, '\\.txt$');
