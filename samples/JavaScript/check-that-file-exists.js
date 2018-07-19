var fso = new ActiveXObject("Scripting.FileSystemObject");

fso.FileExists('input.txt');
fso.FileExists('c:/input.txt');
fso.FolderExists('docs');
fso.FolderExists('c:/docs');
