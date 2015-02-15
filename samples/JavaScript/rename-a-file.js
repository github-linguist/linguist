var fso = new ActiveXObject("Scripting.FileSystemObject");
fso.MoveFile('input.txt', 'output.txt');
fso.MoveFile('c:/input.txt', 'c:/output.txt');
fso.MoveFolder('docs', 'mydocs');
fso.MoveFolder('c:/docs', 'c:/mydocs');
